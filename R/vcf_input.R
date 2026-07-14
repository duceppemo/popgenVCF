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

vcf_index_is_valid <- function(vcf, bcftools = require_vcf_tool("bcftools")) {
  if (!file.exists(paste0(vcf, ".tbi")) && !file.exists(paste0(vcf, ".csi"))) {
    return(FALSE)
  }
  result <- vcf_command_status(bcftools, c("index", "--nrecords", shQuote(vcf)))
  identical(result$status, 0L)
}

#' Prepare a VCF input for analysis
#'
#' Accepts an uncompressed `.vcf` or compressed `.vcf.gz`. Plain VCF files and
#' non-BGZF gzip files are converted to an indexed BGZF copy in `cache_dir`.
#' A valid existing Tabix/CSI index is reused. When a BGZF input is writable and
#' lacks an index, the index is created beside the original file; otherwise an
#' indexed cached copy is created.
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
    index <- if (file.exists(paste0(vcf, ".tbi"))) paste0(vcf, ".tbi") else paste0(vcf, ".csi")
    return(list(path = vcf, index = index, source = vcf, normalized = FALSE))
  }

  if (compressed && !isTRUE(force)) {
    input_dir <- dirname(vcf)
    can_write_index <- file.access(input_dir, 2L) == 0L
    if (can_write_index) {
      indexed <- vcf_command_status(
        bcftools,
        c("index", "--tbi", "--force", shQuote(vcf))
      )
      if (identical(indexed$status, 0L) && vcf_index_is_valid(vcf, bcftools)) {
        return(list(
          path = vcf,
          index = paste0(vcf, ".tbi"),
          source = vcf,
          normalized = FALSE
        ))
      }
    }
  }

  normalized <- file.path(cache_dir, "input.normalized.vcf.gz")
  index <- paste0(normalized, ".tbi")
  if (isTRUE(force) || !file.exists(normalized) || !file.exists(index)) {
    unlink(c(normalized, index, paste0(normalized, ".csi")), force = TRUE)
    converted <- vcf_command_status(
      bcftools,
      c("view", "--output-type", "z", "--output", shQuote(normalized), shQuote(vcf))
    )
    if (!identical(converted$status, 0L) || !file.exists(normalized)) {
      stop(
        "Failed to convert VCF to BGZF with bcftools: ",
        paste(converted$output, collapse = "\n"),
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
  list(path = normalized, index = index, source = vcf, normalized = TRUE)
}
