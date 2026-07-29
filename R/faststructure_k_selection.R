parse_faststructure_k_votes <- function(text, evaluated_k = NULL) {
  lines <- unlist(strsplit(as.character(text), "\\n", fixed = FALSE),
                  use.names = FALSE)
  rows <- list()
  for (line in lines) {
    lower <- tolower(line)
    method <- if (grepl("marginal likelihood", lower, fixed = TRUE)) {
      "Maximum marginal likelihood"
    } else if (grepl("components", lower, fixed = TRUE) &&
               grepl("structure", lower, fixed = TRUE)) {
      "Model components explaining structure"
    } else {
      NULL
    }
    if (is.null(method)) next
    hits <- regmatches(line, gregexpr("[0-9]+", line))[[1L]]
    if (!length(hits) || identical(hits, character(0))) next
    k <- suppressWarnings(as.integer(utils::tail(hits, 1L)))
    if (is.na(k)) next
    rows[[length(rows) + 1L]] <- data.table::data.table(method = method, K = k)
  }
  votes <- data.table::rbindlist(rows)
  if (!nrow(votes)) {
    fallback <- parse_faststructure_k(paste(lines, collapse = "\n"))
    if (!is.null(evaluated_k)) fallback <- fallback[fallback %in% evaluated_k]
    if (length(fallback)) {
      votes <- data.table::data.table(
        method = paste("fastStructure recommendation", seq_along(fallback)),
        K = fallback
      )
    }
  }
  if (!is.null(evaluated_k) && nrow(votes)) {
    votes <- votes[K %in% as.integer(evaluated_k)]
  }
  unique(votes, by = c("method", "K"))
}
