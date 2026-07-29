amova_population_strata <- function(sample_ids, metadata, individual_names = sample_ids) {
  sample_ids <- as.character(sample_ids)
  individual_names <- as.character(individual_names)
  if (!length(sample_ids) || length(individual_names) != length(sample_ids)) {
    stop("AMOVA sample and individual-name vectors must have the same positive length", call. = FALSE)
  }
  if (anyDuplicated(sample_ids)) stop("AMOVA sample IDs must be unique", call. = FALSE)
  if (!all(c("sample", "population") %in% names(metadata))) {
    stop("AMOVA requires metadata columns 'sample' and 'population'", call. = FALSE)
  }

  matched <- match(sample_ids, as.character(metadata$sample))
  if (anyNA(matched)) {
    stop("AMOVA retained samples are missing from metadata", call. = FALSE)
  }
  population <- trimws(as.character(metadata$population[matched]))
  if (anyNA(population) || any(!nzchar(population))) {
    stop("AMOVA requires a non-missing population for every retained sample", call. = FALSE)
  }
  population <- factor(population)
  if (nlevels(population) < 2L) {
    stop(
      "AMOVA requires at least two populations after sample QC; found ",
      nlevels(population),
      call. = FALSE
    )
  }
  data.frame(population = population, row.names = individual_names)
}

run_amova_analysis <- function(geno, sample_ids, metadata, permutations = 999L, seed = 42L) {
  gl <- genlight_from_gds(geno, sample_ids, metadata)
  adegenet::strata(gl) <- amova_population_strata(
    sample_ids, metadata, adegenet::indNames(gl)
  )
  set.seed(seed)
  model <- poppr::poppr.amova(gl, ~population, within = TRUE, quiet = TRUE)
  test <- tryCatch(ade4::randtest(model, nrepet = permutations), error = function(e) NULL)
  components <- data.table::as.data.table(model$componentsofcovariance, keep.rownames = "component")
  phi <- data.table::as.data.table(model$statphi, keep.rownames = "statistic")
  permutation <- if (is.null(test)) data.table::data.table() else data.table::data.table(
    observed = test$obs, p_value = test$pvalue, permutations = permutations)
  list(model = model, test = test, components = components, phi = phi, permutation = permutation)
}
