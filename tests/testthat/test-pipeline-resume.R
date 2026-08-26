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
