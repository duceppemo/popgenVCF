normalize_sample_aliases <- function(metadata) {
  x <- data.table::copy(data.table::as.data.table(metadata))
  if (!"sample" %in% names(x)) stop("metadata must contain a sample column", call. = FALSE)
  x[, sample := as.character(sample)]
  if (!"alias" %in% names(x)) x[, alias := NA_character_]
  x[, alias := trimws(as.character(alias))]
  x[is.na(alias) | !nzchar(alias), alias := NA_character_]

  duplicate_aliases <- unique(x[!is.na(alias) & duplicated(alias), alias])
  if (length(duplicate_aliases)) {
    stop(
      "Metadata aliases must be unique: ",
      paste(duplicate_aliases, collapse = ", "),
      call. = FALSE
    )
  }

  x[, display_sample := data.table::fifelse(is.na(alias), sample, alias)]
  duplicate_display <- unique(x[duplicated(display_sample), display_sample])
  if (length(duplicate_display)) {
    stop(
      paste0(
        "Aliases and original sample IDs must resolve to globally unique public names: ",
        paste(duplicate_display, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  x[]
}

public_sample_ids <- function(metadata, vcf_sample_ids) {
  metadata <- normalize_sample_aliases(metadata)
  vcf_sample_ids <- as.character(vcf_sample_ids)
  index <- match(vcf_sample_ids, metadata$sample)
  if (anyNA(index)) {
    stop("Cannot resolve public sample names for IDs absent from metadata", call. = FALSE)
  }
  metadata$display_sample[index]
}

relabel_sample_matrix <- function(x, metadata) {
  out <- as.matrix(x)
  if (!is.null(rownames(out))) rownames(out) <- public_sample_ids(metadata, rownames(out))
  if (!is.null(colnames(out))) colnames(out) <- public_sample_ids(metadata, colnames(out))
  out
}

normalize_ld_window_bp <- function(x = Inf) {
  value <- suppressWarnings(as.numeric(x)[1L])
  if (is.na(value) || value <= 0) {
    stop("LD window must be a positive number or Inf", call. = FALSE)
  }
  if (!is.finite(value) || value > .Machine$integer.max) {
    return(.Machine$integer.max)
  }
  as.integer(value)
}
