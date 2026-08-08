#' Locate the bundled real quickstart example dataset
#'
#' A small, real 1000 Genomes chromosome 22 subset (160 samples, 8
#' populations) bundled with the package so a new user can see real popgenVCF
#' output without downloading anything. See
#' `inst/extdata/quickstart/README.md` for full provenance, and
#' `vignette("quickstart", package = "popgenVCF")` for a complete walkthrough.
#'
#' @return A list with `directory`, `vcf`, and `metadata` paths.
#' @export
quickstart_dataset_paths <- function() {
  base <- system.file("extdata", "quickstart", package = "popgenVCF")
  if (!nzchar(base)) stop("Installed quickstart dataset was not found", call. = FALSE)
  list(
    directory = base,
    vcf = file.path(base, "chr22_quickstart.vcf.gz"),
    metadata = file.path(base, "chr22_quickstart_metadata.tsv")
  )
}
