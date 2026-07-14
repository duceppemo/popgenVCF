run_amova_analysis <- function(geno, sample_ids, metadata, permutations = 999L, seed = 42L) {
  gl <- genlight_from_gds(geno, sample_ids, metadata)
  adegenet::strata(gl) <- data.frame(population = metadata[match(adegenet::indNames(gl), sample), population])
  set.seed(seed)
  model <- poppr::poppr.amova(gl, ~population, within = TRUE, quiet = TRUE)
  test <- tryCatch(ade4::randtest(model, nrepet = permutations), error = function(e) NULL)
  components <- data.table::as.data.table(model$componentsofcovariance, keep.rownames = "component")
  phi <- data.table::as.data.table(model$statphi, keep.rownames = "statistic")
  permutation <- if (is.null(test)) data.table::data.table() else data.table::data.table(
    observed = test$obs, p_value = test$pvalue, permutations = permutations)
  list(model = model, test = test, components = components, phi = phi, permutation = permutation)
}
