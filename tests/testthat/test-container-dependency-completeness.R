container_dependency_source_root <- function() {
  required <- c("DESCRIPTION", "inst/conda/environment.yml", "inst/scripts/install-bioconductor.R")

  is_root <- function(path) {
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required)))
  }

  ancestors <- function(path) {
    out <- character()
    current <- path
    repeat {
      out <- c(out, current)
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
    out
  }

  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  test_dir <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  working_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  bases <- unique(c(workspace, ancestors(test_dir), ancestors(working_dir)))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(
    bases,
    file.path(bases, "popgenVCF"),
    file.path(bases, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(candidates, is_root, logical(1L))]
  if (!length(matches)) return(NA_character_)
  matches[[1L]]
}

require_container_dependency_root <- function() {
  root <- container_dependency_source_root()
  if (is.na(root)) {
    testthat::skip("Repository source tree (DESCRIPTION, environment.yml, install-bioconductor.R) is unavailable")
  }
  root
}

test_that("every required Imports package is installed by the Docker/Apptainer image build, not left to transitive resolution", {
  # A real production incident: `pcadapt` (a Suggests package needed by a
  # default-on module) was never added to inst/conda/environment.yml, so
  # every real Docker/Apptainer image genuinely lacked it -- the container
  # build itself never failed, because R CMD INSTALL doesn't verify
  # Suggests, and any Imports gap has so far been silently papered over by
  # conda pulling packages in transitively as *other* listed packages'
  # dependencies. That's fragile: if some other package ever drops an
  # unlisted transitive dependency, the image would silently stop
  # providing it. This check makes every required Imports package an
  # explicit, intentional line in environment.yml instead.
  root <- require_container_dependency_root()

  description <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Imports")
  imports <- trimws(strsplit(unname(description[1L, 1L]), ",")[[1L]])
  imports <- sub("\\s*\\(.*\\)$", "", imports)
  imports <- imports[nzchar(imports)]

  base_pkgs <- rownames(utils::installed.packages(priority = "base"))

  bioc_script <- readLines(file.path(root, "inst", "scripts", "install-bioconductor.R"), warn = FALSE)
  # install-bioconductor.R declares its own required/optional Bioconductor
  # package vectors; these are installed via BiocManager, not conda, so
  # they are legitimately absent from environment.yml.
  bioc_line <- grep("^required <- c\\(", bioc_script, value = TRUE)
  bioc_handled <- gsub("[\"' ]", "", regmatches(
    bioc_line, gregexpr("\"[^\"]+\"", bioc_line)
  )[[1L]])
  bioc_handled <- c(bioc_handled, "LEA") # declared as `optional`, same install path

  required_cran <- setdiff(imports, c(base_pkgs, bioc_handled))
  expect_true(length(required_cran) > 0L) # sanity: the extraction itself worked

  env_yaml <- readLines(file.path(root, "inst", "conda", "environment.yml"), warn = FALSE)
  env_r_packages <- sub("^\\s*-\\s*r-", "", grep("^\\s*-\\s*r-", env_yaml, value = TRUE))
  env_r_packages <- tolower(sub("[><=].*$", "", env_r_packages))

  missing <- setdiff(tolower(required_cran), env_r_packages)
  expect_length(missing, 0L)
})
