# Package-wide, locale-independent ordering of character data.
#
# base::sort() and base::order() collate strings by the session's LC_COLLATE.
# "S-2" sorts before "S1" under C and after it under en_US; "ne_ld" and
# "neighbor", "B.txt" and "a.txt" swap the same way. Everything this package
# calls canonical -- record fingerprints, "must be sorted" validators,
# manifests, population and pair ordering in tables -- therefore depended on
# the locale of the machine that produced it: the same inputs gave different
# identifiers on a laptop (en_US) and a cluster or container (C), and a record
# written on one failed the canonical-order check on the other (confirmed
# directly for workspace manifests). data.table, which does most of the
# pipeline's ordering, already collates in C; these two masks make the ~250
# base-R call sites agree with it instead of patching them one at a time.
#
# Only character input is affected, and an explicit `method` is respected.
sort <- function(x, decreasing = FALSE, ...) {
  if (is.character(x) && !is.object(x) && !"method" %in% names(list(...))) {
    return(base::sort(x, decreasing = decreasing, method = "radix", ...))
  }
  base::sort(x, decreasing = decreasing, ...)
}

order <- function(..., method = NULL) {
  if (!is.null(method)) return(base::order(..., method = method))
  keys <- list(...)
  if (!is.null(names(keys))) keys <- keys[!names(keys) %in% c("na.last", "decreasing")]
  if (any(vapply(keys, function(key) is.character(key) && !is.object(key), logical(1L)))) {
    return(base::order(..., method = "radix"))
  }
  base::order(...)
}
