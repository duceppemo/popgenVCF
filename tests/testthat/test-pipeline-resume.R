# Real scenario this exists for (see NEWS.md): a real 44-hour production run
# was deliberately `docker stop`-ped mid-way and had to restart from scratch,
# since nothing survived to resume from. These tests exercise the actual
# run_pipeline()/run_pipeline_resume() wiring end to end, not just the
# lower-level checkpoint primitives test-execution-checkpoint.R already
# covers directly.

resume_fixture_gds <- function() {
  set.seed(1)
  n <- 8L; l <- 10L
  sample_id <- paste0("s", seq_len(n))
  genmat <- matrix(sample(0:2, n * l, replace = TRUE), n, l)
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = seq_len(l),
    snp.chromosome = rep(1L, l), snp.position = seq_len(l) * 100L,
    snp.allele = rep("A/G", l), snpfirstdim = FALSE
  )
  list(path = gds_path, sample_id = sample_id)
}

resume_fixture_analysis <- function(outdir, gds_path) {
  cfg <- default_config()
  cfg$output$directory <- outdir
  # write_manifest() (called at the very end, via finalize_pipeline_analysis())
  # normalizePath()s and checksums cfg$input$vcf for real, so it must point
  # at a real, existing file, even a trivial one -- matching what a genuine
  # checkpoint's stored config always has (validate_config() requires it).
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  writeLines("##fileformat=VCFv4.2", cfg$input$vcf)
  analysis <- new_popgen_vcf_analysis(cfg)
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  list(
    analysis = analysis,
    context = list(gds_path = gds_path)
  )
}

resume_fixture_module <- function(name, counter = NULL) {
  force(name)
  force(counter)
  function(analysis, context) {
    if (!is.null(counter)) counter[[name]] <- (counter[[name]] %||% 0L) + 1L
    # A real module would touch context$gds here; asserting it is present
    # and actually a live, reopened connection (not the pre-checkpoint
    # external pointer, which cannot survive serialization) is the point.
    if (!is.null(context$gds)) SNPRelate::snpgdsSummary(context$gds, show = FALSE)
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

resume_fixture_registry <- function(fail_second = FALSE, counter = NULL) {
  registry <- new_analysis_registry()
  registry <- register_analysis(registry, "first", resume_fixture_module("first", counter))
  second <- if (fail_second) {
    function(analysis, context) stop("interrupted", call. = FALSE)
  } else {
    resume_fixture_module("second", counter)
  }
  registry <- register_analysis(registry, "second", second, requires = "first")
  register_analysis(registry, "third", resume_fixture_module("third", counter), requires = "second")
}

test_that("execute_analysis_plan writes a real, valid checkpoint after each batch when checkpoint_path is set", {
  outdir <- tempfile("resume-checkpoint-write-")
  fx <- resume_fixture_gds()
  built <- resume_fixture_analysis(outdir, fx$path)
  registry <- resume_fixture_registry()
  checkpoint_path <- tempfile(fileext = ".rds")

  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  context <- c(built$context, list(gds = gds))

  executed <- execute_analysis_registry(
    built$analysis, context, registry, checkpoint_path = checkpoint_path
  )
  expect_identical(executed$order, c("first", "second", "third"))
  expect_true(file.exists(checkpoint_path))

  restored <- read_execution_checkpoint(checkpoint_path, registry)
  expect_identical(restored$completed, c("first", "second", "third"))
  # context$gds (the live gdsfmt connection) must never be embedded in a
  # written checkpoint -- it is an external pointer that cannot survive
  # serialization (see execution_checkpoint_safe_context()); only the plain
  # path string should round-trip. Checked via names(), not `$gds` (which
  # would silently partial-match the surviving "gds_path" element once
  # "gds" itself is genuinely absent).
  expect_false("gds" %in% names(restored$context))
  expect_identical(restored$context$gds_path, fx$path)
})

test_that("run_pipeline_resume picks up an interrupted run without re-running completed modules", {
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  outdir <- tempfile("resume-integration-")
  fx <- resume_fixture_gds()
  built <- resume_fixture_analysis(outdir, fx$path)
  failing_registry <- resume_fixture_registry(fail_second = TRUE)
  checkpoint_path <- file.path(outdir, "execution_checkpoint.rds")

  gds <- SNPRelate::snpgdsOpen(fx$path)
  interrupted <- tryCatch({
    context <- c(built$context, list(gds = gds))
    execute_analysis_registry(
      built$analysis, context, failing_registry,
      engine = new_execution_engine(fail_fast = TRUE),
      checkpoint_path = checkpoint_path
    )
  }, error = function(e) e)
  SNPRelate::snpgdsClose(gds)

  # "second" throws with fail_fast = TRUE, so execute_analysis_registry()
  # itself errors out here -- exactly like a real crash partway through a
  # real run. Only "first" ever completed and was checkpointed.
  expect_s3_class(interrupted, "error")
  expect_true(file.exists(checkpoint_path))
  on_disk <- read_execution_checkpoint(checkpoint_path, failing_registry)
  expect_identical(on_disk$completed, "first")

  counter <- new.env(parent = emptyenv())
  fixed_registry <- resume_fixture_registry(counter = counter)
  resumed <- run_pipeline_resume(outdir, fixed_registry)

  # "first" must not run again -- only "second" and "third", the modules
  # the interrupted run never reached.
  expect_null(counter$first)
  expect_identical(counter$second, 1L)
  expect_identical(counter$third, 1L)
  expect_identical(
    sort(resumed$results$execution_order),
    c("first", "second", "third")
  )
  expect_identical(resumed$status, "complete")
  expect_true(file.exists(file.path(outdir, "analysis_results.rds")))
  expect_match(
    resumed$messages[stage == "analysis registry", message],
    "resumed from checkpoint: 1 module\\(s\\) reused, 2 module\\(s\\) executed"
  )
})

test_that("run_pipeline_resume errors clearly when no checkpoint exists", {
  outdir <- tempfile("resume-missing-")
  dir.create(outdir)
  expect_error(
    run_pipeline_resume(outdir, resume_fixture_registry()),
    "No execution checkpoint found"
  )
})

test_that("run_pipeline_resume on an already-fully-completed checkpoint re-runs nothing", {
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  outdir <- tempfile("resume-complete-")
  fx <- resume_fixture_gds()
  built <- resume_fixture_analysis(outdir, fx$path)
  counter <- new.env(parent = emptyenv())
  registry <- resume_fixture_registry(counter = counter)
  checkpoint_path <- file.path(outdir, "execution_checkpoint.rds")

  gds <- SNPRelate::snpgdsOpen(fx$path)
  context <- c(built$context, list(gds = gds))
  execute_analysis_registry(built$analysis, context, registry, checkpoint_path = checkpoint_path)
  SNPRelate::snpgdsClose(gds)
  counter$first <- counter$second <- counter$third <- 0L

  resumed <- run_pipeline_resume(outdir, registry)

  expect_identical(counter$first, 0L)
  expect_identical(counter$second, 0L)
  expect_identical(counter$third, 0L)
  expect_match(
    resumed$messages[stage == "analysis registry", message],
    "resumed from checkpoint: 3 module\\(s\\) reused, 0 module\\(s\\) executed"
  )
})

# Real gap this closes (see the GitHub issue this was filed from): editing a
# config value and resuming used to be undetectable, since run_pipeline_resume()
# never accepted a config at all -- there was no way to even attempt it, let
# alone safely. These tests exercise the opt-in config-drift check directly.
resume_interrupted_checkpoint <- function() {
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  outdir <- tempfile("resume-config-drift-")
  fx <- resume_fixture_gds()
  built <- resume_fixture_analysis(outdir, fx$path)
  # run_pipeline() always stores the post-validate_config() config on a real
  # analysis (see run_pipeline()); resume_fixture_analysis() does not, so
  # validate here to make config_fingerprint() comparisons apples-to-apples
  # with what run_pipeline_resume()'s own internal validate_config(candidate)
  # call produces.
  built$analysis$config <- validate_config(built$analysis$config)
  failing_registry <- resume_fixture_registry(fail_second = TRUE)
  checkpoint_path <- file.path(outdir, "execution_checkpoint.rds")

  gds <- SNPRelate::snpgdsOpen(fx$path)
  tryCatch({
    context <- c(built$context, list(gds = gds))
    execute_analysis_registry(
      built$analysis, context, failing_registry,
      engine = new_execution_engine(fail_fast = TRUE),
      checkpoint_path = checkpoint_path
    )
  }, error = function(e) NULL)
  SNPRelate::snpgdsClose(gds)
  list(outdir = outdir, checkpoint_path = checkpoint_path)
}

test_that("run_pipeline_resume with no config argument reuses the checkpointed configuration unconditionally, unchanged behavior", {
  fx <- resume_interrupted_checkpoint()
  counter <- new.env(parent = emptyenv())
  resumed <- run_pipeline_resume(fx$outdir, resume_fixture_registry(counter = counter))
  expect_identical(resumed$status, "complete")
  expect_identical(counter$second, 1L)
})

test_that("run_pipeline_resume with a matching config resumes normally", {
  fx <- resume_interrupted_checkpoint()
  on_disk <- popgenVCF:::read_execution_checkpoint(fx$checkpoint_path, resume_fixture_registry())
  matching_config <- on_disk$analysis$config

  counter <- new.env(parent = emptyenv())
  resumed <- run_pipeline_resume(
    fx$outdir, resume_fixture_registry(counter = counter), config = matching_config
  )
  expect_identical(resumed$status, "complete")
  expect_identical(counter$second, 1L)
})

test_that("run_pipeline_resume refuses, loudly and clearly, when the supplied config differs from the checkpointed one", {
  fx <- resume_interrupted_checkpoint()
  on_disk <- popgenVCF:::read_execution_checkpoint(fx$checkpoint_path, resume_fixture_registry())
  changed_config <- on_disk$analysis$config
  changed_config$qc$maf <- changed_config$qc$maf + 0.1

  counter <- new.env(parent = emptyenv())
  expect_error(
    run_pipeline_resume(fx$outdir, resume_fixture_registry(counter = counter), config = changed_config),
    "does not match the one this checkpoint was written under.*qc"
  )
  # Nothing should have executed -- the check happens before any module runs.
  expect_null(counter$second)
})

test_that("run_pipeline_resume's config-mismatch error names every top-level section that actually differs", {
  fx <- resume_interrupted_checkpoint()
  on_disk <- popgenVCF:::read_execution_checkpoint(fx$checkpoint_path, resume_fixture_registry())
  changed_config <- on_disk$analysis$config
  changed_config$qc$maf <- changed_config$qc$maf + 0.1
  changed_config$analyses$pca <- !isTRUE(changed_config$analyses$pca)

  expect_error(
    run_pipeline_resume(fx$outdir, resume_fixture_registry(), config = changed_config),
    "qc, analyses|analyses, qc"
  )
})

test_that("run_pipeline_resume refuses to compare against a checkpoint with no recorded config fingerprint", {
  fx <- resume_interrupted_checkpoint()
  on_disk <- popgenVCF:::read_execution_checkpoint(fx$checkpoint_path, resume_fixture_registry())
  legacy <- on_disk
  legacy$config_fingerprint <- NULL
  legacy$checkpoint_digest <- popgenVCF:::checkpoint_payload_digest(legacy)
  envelope <- popgenVCF:::new_runtime_integrity_envelope("checkpoint", legacy)
  saveRDS(envelope, fx$checkpoint_path, version = 3, compress = "xz")
  writeLines(
    paste(digest::digest(file = fx$checkpoint_path, algo = "sha256"), basename(fx$checkpoint_path)),
    paste0(fx$checkpoint_path, ".sha256")
  )

  expect_error(
    run_pipeline_resume(fx$outdir, resume_fixture_registry(), config = on_disk$analysis$config),
    "predates config-drift detection"
  )
  # Resuming without a config still works against the same legacy checkpoint.
  counter <- new.env(parent = emptyenv())
  resumed <- run_pipeline_resume(fx$outdir, resume_fixture_registry(counter = counter))
  expect_identical(resumed$status, "complete")
})
