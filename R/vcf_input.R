vcf_command_status <- function(command, args) {
  output <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status") %||% 0L
  list(status = as.integer(status), output = output)
}

require_vcf_tool <- function(tool) {
  path <- Sys.which(tool)
  if (!nzchar(path)) {
    stop(
      "Required VCF utility '", tool, "' was not found on PATH. ",
      "Install bcftools/htslib or use the published popgenVCF container.",
      call. = FALSE
    )
  }
  unname(path)
}

vcf_index_path <- function(vcf) {
  if (file.exists(paste0(vcf, ".tbi"))) return(paste0(vcf, ".tbi"))
  if (file.exists(paste0(vcf, ".csi"))) return(paste0(vcf, ".csi"))
  NA_character_
}

vcf_index_is_valid <- function(vcf, bcftools = require_vcf_tool("bcftools")) {
  if (is.na(vcf_index_path(vcf))) return(FALSE)
  result <- vcf_command_status(bcftools, c("index", "--nrecords", shQuote(vcf)))
  identical(result$status, 0L)
}

#' Prepare a VCF input for analysis
#'
#' Accepts an uncompressed `.vcf` or compressed `.vcf.gz`. Plain VCF files,
#' ordinary gzip files, and compressed files that cannot be indexed are sorted
#' and converted to an indexed BGZF copy in `cache_dir`. A valid existing
#' Tabix/CSI index is reused. When an existing BGZF input is writable and lacks
#' an index, a Tabix index is created beside the original file.
#'
#' @param vcf Path to a `.vcf` or `.vcf.gz` file.
#' @param cache_dir Directory for normalized cached inputs.
#' @param force Recreate the normalized cached copy and index.
#' @return A list with `path`, `index`, `source`, and `normalized` fields.
#' @export
prepare_vcf_input <- function(vcf, cache_dir, force = FALSE) {
  if (!is.character(vcf) || length(vcf) != 1L || is.na(vcf) || !nzchar(vcf)) {
    stop("vcf must be one non-empty path", call. = FALSE)
  }
  vcf <- normalizePath(vcf, winslash = "/", mustWork = TRUE)
  if (!grepl("\\.vcf(?:\\.gz)?$", vcf, ignore.case = TRUE, perl = TRUE)) {
    stop("VCF input must end in .vcf or .vcf.gz: ", vcf, call. = FALSE)
  }
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("force must be TRUE or FALSE", call. = FALSE)
  }

  bcftools <- require_vcf_tool("bcftools")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)

  compressed <- grepl("\\.vcf\\.gz$", vcf, ignore.case = TRUE)
  if (compressed && !isTRUE(force) && vcf_index_is_valid(vcf, bcftools)) {
    return(list(
      path = vcf,
      index = vcf_index_path(vcf),
      source = vcf,
      normalized = FALSE
    ))
  }

  if (compressed && !isTRUE(force) && file.access(dirname(vcf), 2L) == 0L) {
    indexed <- vcf_command_status(
      bcftools,
      c("index", "--tbi", "--force", shQuote(vcf))
    )
    if (identical(indexed$status, 0L) && vcf_index_is_valid(vcf, bcftools)) {
      return(list(
        path = vcf,
        index = vcf_index_path(vcf),
        source = vcf,
        normalized = FALSE
      ))
    }
  }

  normalized <- file.path(cache_dir, "input.normalized.vcf.gz")
  index <- paste0(normalized, ".tbi")
  source_info <- file.info(vcf)
  normalized_info <- if (file.exists(normalized)) file.info(normalized) else NULL
  cache_current <- !is.null(normalized_info) &&
    !is.na(normalized_info$mtime) &&
    normalized_info$mtime >= source_info$mtime &&
    vcf_index_is_valid(normalized, bcftools)

  if (isTRUE(force) || !cache_current) {
    unlink(c(normalized, index, paste0(normalized, ".csi")), force = TRUE)
    sorted <- vcf_command_status(
      bcftools,
      c("sort", "--output-type", "z", "--output", shQuote(normalized), shQuote(vcf))
    )
    if (!identical(sorted$status, 0L) || !file.exists(normalized)) {
      stop(
        "Failed to sort and BGZF-compress VCF with bcftools: ",
        paste(sorted$output, collapse = "\n"),
        call. = FALSE
      )
    }
    indexed <- vcf_command_status(
      bcftools,
      c("index", "--tbi", "--force", shQuote(normalized))
    )
    if (!identical(indexed$status, 0L) || !file.exists(index)) {
      stop(
        "Failed to create Tabix index for normalized VCF: ",
        paste(indexed$output, collapse = "\n"),
        call. = FALSE
      )
    }
  }

  if (!vcf_index_is_valid(normalized, bcftools)) {
    stop("The normalized VCF index could not be validated: ", normalized, call. = FALSE)
  }
  list(path = normalized, index = vcf_index_path(normalized), source = vcf, normalized = TRUE)
}
