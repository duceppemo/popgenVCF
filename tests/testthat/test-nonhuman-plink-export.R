test_that("undefined autosome bounds are detected without ambiguous conditions", {
  undefined <- function(gds) {
    list(autosome.start = NA_integer_, autosome.end = NA_integer_)
  }
  missing_end <- function(gds) {
    list(autosome.start = 1L, autosome.end = NULL)
  }
  defined <- function(gds) {
    list(autosome.start = 1L, autosome.end = 6L)
  }

  expect_false(popgenVCF:::gds_autosome_bounds_defined(NULL, undefined))
  expect_false(popgenVCF:::gds_autosome_bounds_defined(NULL, missing_end))
  expect_true(popgenVCF:::gds_autosome_bounds_defined(NULL, defined))
})

test_that("PLINK invocation arguments shell-quote dynamic paths, closing a real command-injection gap", {
  # Real security finding from a pre-release audit: command_runner defaults
  # to system2(), which -- confirmed directly -- shell-interprets its `args`
  # vector on Unix (base R pastes command + args into one string run through
  # a shell), so an unquoted dynamic path is not passed as one opaque argv
  # element the way it would be under execve()/a shell-free API. A real
  # output.directory containing a shell metacharacter (a space, a semicolon)
  # would otherwise either break or, worse, be silently shell-interpreted.
  root <- tempfile("nonhuman-plink-injection-")
  dir.create(root)
  prefix <- file.path(root, "cohort")
  captured_args <- NULL
  ped_converter <- function(gdsobj, ped.fn, sample.id, snp.id,
                            use.snp.rsid = FALSE, format = "A/G/C/T",
                            verbose = FALSE) {
    writeLines("synthetic PED", paste0(ped.fn, ".ped"))
    writeLines("synthetic MAP", paste0(ped.fn, ".map"))
    invisible(NULL)
  }
  command_runner <- function(command, args, stdout, stderr) {
    captured_args <<- args
    stop("stop before actually invoking anything -- only inspecting args")
  }
  expect_error(
    popgenVCF:::portable_gds_to_bed(
      gdsobj = NULL, bed.fn = prefix, sample.id = c("s1", "s2"), snp.id = 1:3,
      option_reader = function(gds) list(autosome.start = NA_integer_, autosome.end = NA_integer_),
      direct_converter = function(...) stop("direct converter must not be called"),
      ped_converter = ped_converter,
      plink_locator = function(executable) "/usr/bin/plink",
      command_runner = command_runner
    ),
    "inspecting args"
  )
  file_index <- match("--file", captured_args)
  out_index <- match("--out", captured_args)
  expect_true(grepl("^'.*'$", captured_args[[file_index + 1L]]))
  expect_true(grepl("^'.*'$", captured_args[[out_index + 1L]]))
})

test_that("non-human chromosome metadata falls back through PED and PLINK", {
  root <- tempfile("nonhuman-plink-")
  dir.create(root)
  prefix <- file.path(root, "cohort")
  samples <- c("sample_1", "sample_2", "sample_3")
  snps <- 101:105
  direct_called <- FALSE
  ped_called <- FALSE

  option_reader <- function(gds) {
    list(autosome.start = NA_integer_, autosome.end = NA_integer_)
  }
  direct_converter <- function(...) {
    direct_called <<- TRUE
    stop("direct converter must not be called")
  }
  ped_converter <- function(gdsobj, ped.fn, sample.id, snp.id,
                            use.snp.rsid = FALSE, format = "A/G/C/T",
                            verbose = FALSE) {
    ped_called <<- TRUE
    writeLines("synthetic PED", paste0(ped.fn, ".ped"))
    writeLines("synthetic MAP", paste0(ped.fn, ".map"))
    invisible(NULL)
  }
  command_runner <- function(command, args, stdout, stderr) {
    out_index <- match("--out", args)
    # portable_gds_to_bed() shQuote()s dynamic path arguments before handing
    # them to command_runner, matching what a real shell-interpreted
    # system2() call requires (a real security fix -- see R/nonhuman_plink_export.R);
    # this mock bypasses any real shell, so it must undo that quoting itself
    # to recover the plain path, the same way a real shell would.
    out_prefix <- gsub("^'(.*)'$", "\\1", args[[out_index + 1L]])
    writeBin(as.raw(c(0x6c, 0x1b, 0x01, 0x00)), paste0(out_prefix, ".bed"))
    data.table::fwrite(
      data.table::data.table(
        V1 = 0, V2 = samples, V3 = 0, V4 = 0, V5 = 0, V6 = -9
      ),
      paste0(out_prefix, ".fam"),
      sep = "\t", col.names = FALSE
    )
    data.table::fwrite(
      data.table::data.table(
        V1 = 1, V2 = snps, V3 = 0, V4 = seq_along(snps),
        V5 = "A", V6 = "C"
      ),
      paste0(out_prefix, ".bim"),
      sep = "\t", col.names = FALSE
    )
    "synthetic PLINK success"
  }

  expect_invisible(
    popgenVCF:::portable_gds_to_bed(
      gdsobj = NULL,
      bed.fn = prefix,
      sample.id = samples,
      snp.id = snps,
      option_reader = option_reader,
      direct_converter = direct_converter,
      ped_converter = ped_converter,
      plink_locator = function(executable) "/usr/bin/plink",
      command_runner = command_runner
    )
  )

  expect_false(direct_called)
  expect_true(ped_called)
  inspection <- popgenVCF:::inspect_plink_bundle(prefix, samples, snps)
  expect_true(inspection$valid)
  expect_identical(inspection$n_samples, 3L)
  expect_identical(inspection$n_snps, 5L)
})

test_that("defined chromosome bounds retain the direct SNPRelate path", {
  root <- tempfile("direct-plink-")
  dir.create(root)
  prefix <- file.path(root, "cohort")
  samples <- c("A", "B")
  snps <- 1:3
  direct_called <- FALSE

  direct_converter <- function(gdsobj, bed.fn, sample.id, snp.id,
                               verbose = FALSE) {
    direct_called <<- TRUE
    writeBin(as.raw(c(0x6c, 0x1b, 0x01, 0x00)), paste0(bed.fn, ".bed"))
    data.table::fwrite(
      data.table::data.table(V1 = 0, V2 = sample.id, V3 = 0,
                             V4 = 0, V5 = 0, V6 = -9),
      paste0(bed.fn, ".fam"), sep = "\t", col.names = FALSE
    )
    data.table::fwrite(
      data.table::data.table(V1 = 1, V2 = snp.id, V3 = 0,
                             V4 = seq_along(snp.id), V5 = "A", V6 = "C"),
      paste0(bed.fn, ".bim"), sep = "\t", col.names = FALSE
    )
    invisible(NULL)
  }

  expect_invisible(
    popgenVCF:::portable_gds_to_bed(
      gdsobj = NULL,
      bed.fn = prefix,
      sample.id = samples,
      snp.id = snps,
      option_reader = function(gds) {
        list(autosome.start = 1L, autosome.end = 6L)
      },
      direct_converter = direct_converter,
      ped_converter = function(...) stop("PED fallback should not run"),
      plink_locator = function(executable) stop("PLINK lookup should not run"),
      command_runner = function(...) stop("PLINK should not run")
    )
  )

  expect_true(direct_called)
  expect_true(popgenVCF:::inspect_plink_bundle(prefix, samples, snps)$valid)
})

test_that("ancestry modules inject the portable converter", {
  admixture_body <- paste(
    deparse(body(popgenVCF:::run_module_admixture)), collapse = "\n"
  )
  faststructure_body <- paste(
    deparse(body(popgenVCF:::run_module_faststructure)), collapse = "\n"
  )

  expect_match(admixture_body, "converter = portable_gds_to_bed", fixed = TRUE)
  expect_match(
    admixture_body, "select_structure_k_if_informative", fixed = TRUE
  )
  expect_match(faststructure_body, "converter = portable_gds_to_bed", fixed = TRUE)
})
