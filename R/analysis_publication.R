publication_result_id <- function(x, fallback = "analysis") {
  value <- x$analysis_id %||% x$id %||% x$module %||% fallback
  tolower(gsub("[^a-z0-9]+", "_", as.character(value)[1L]))
}

publication_result_parameters <- function(x) {
  x$parameters %||% x$provenance$parameters %||% list()
}

publication_result_metadata <- function(x) {
  x$metadata %||% x$provenance$metadata %||% list()
}

publication_count <- function(x, candidates) {
  for (name in candidates) {
    value <- x[[name]]
    if (!is.null(value) && length(value) == 1L && is.finite(suppressWarnings(as.numeric(value)))) {
      return(as.integer(value))
    }
  }
  NA_integer_
}

publication_percent <- function(x) {
  value <- suppressWarnings(as.numeric(x)[1L])
  if (!is.finite(value)) return(NA_character_)
  if (value <= 1) value <- value * 100
  sprintf("%.1f%%", value)
}

publication_result_kind <- function(x, name = NULL) {
  classes <- tolower(class(x))
  key <- tolower(paste(c(name, classes, x$analysis_id, x$type, x$module), collapse = " "))
  kinds <- c("pca", "ibs", "tree", "diversity", "fst", "amova", "dapc", "ibd", "ancestry")
  hits <- kinds[vapply(kinds, grepl, logical(1L), x = key, fixed = TRUE)]
  if (length(hits)) hits[[1L]] else "analysis"
}

publication_method_pca <- function(x) {
  p <- publication_result_parameters(x)
  n_samples <- publication_count(x, c("n_samples", "sample_count"))
  n_snps <- publication_count(x, c("n_snps", "variant_count", "snp_count"))
  method <- p$method %||% x$method %||% "SNPRelate principal component analysis"
  paste0("Principal component analysis was performed using ", method,
         if (!is.na(n_samples)) paste0(" on ", n_samples, " samples") else "",
         if (!is.na(n_snps)) paste0(" and ", n_snps, " variants") else "",
         ". Genotypes were analyzed using the filtering and linkage-disequilibrium pruning parameters preserved in the reproducible project.")
}

publication_legend_pca <- function(x) {
  variance <- x$variance %||% x$variance_explained %||% x$eigenvalue_table$variance_explained
  axes <- if (length(variance) >= 2L) paste0("PC1 and PC2 explained ", publication_percent(variance[[1L]]),
                                             " and ", publication_percent(variance[[2L]]), " of the variance, respectively. ") else ""
  paste0("Principal component analysis of samples using LD-pruned SNPs. ", axes,
         "Sample labels use metadata aliases when available; original VCF identifiers remain preserved in provenance.")
}

publication_method_ibs <- function(x) {
  paste0("Pairwise identity-by-state similarity was estimated from the retained genotype matrix",
         if (!is.null(x$method)) paste0(" using ", x$method) else "",
         ". Multidimensional scaling coordinates were derived from the IBS-based distance matrix. IBS-derived distance was not interpreted as FST.")
}

publication_legend_ibs <- function(x) {
  "Multidimensional scaling of pairwise IBS-derived genetic distances among samples. Smaller distances indicate greater genotype similarity and do not represent population-level FST estimates."
}

publication_method_tree <- function(x) {
  "A neighbour-joining tree was constructed from the recorded genetic distance matrix using the method and transformation stored in the canonical tree result. Tip labels use sample aliases when available."
}

publication_legend_tree <- function(x) {
  "Neighbour-joining representation of genetic distances among samples. Branch lengths reflect the input distance measure and are not a time-calibrated phylogeny."
}

publication_method_diversity <- function(x) {
  "Population and sample diversity statistics were calculated from the filtered biallelic SNP data. The reported sample size, observed and expected heterozygosity, allelic summaries, and missingness were computed using the population assignments and estimators recorded in the canonical diversity result."
}

publication_legend_diversity <- function(x) {
  "Genetic diversity summaries by population. Values and sample counts are reported from the canonical diversity result after VCF filtering and sample identity resolution."
}

publication_method_fst <- function(x) {
  method <- x$method %||% publication_result_parameters(x)$method %||% "Weir and Cockerham's 1984 estimator"
  paste0("Population differentiation was quantified using ", method,
         ". Global and pairwise FST estimates were calculated at the population level from the filtered SNP data; IBS-derived sample distances were analyzed separately.")
}

publication_legend_fst <- function(x) {
  "Pairwise population FST estimates. Values represent population-level differentiation using the estimator recorded in the canonical result; diagonal cells denote within-population comparisons."
}

publication_method_amova <- function(x) {
  "Analysis of molecular variance was performed using the hierarchy and genetic distance definition recorded in the canonical AMOVA result. Variance components and permutation significance were reported only for metadata levels available in the analysis project."
}

publication_legend_amova <- function(x) {
  "Analysis of molecular variance showing the partitioning of genetic variation among and within the available hierarchical metadata groups."
}

publication_method_dapc <- function(x) {
  p <- publication_result_parameters(x)
  pcs <- p$n_pca %||% p$n_pcs %||% x$n_pca
  das <- p$n_da %||% x$n_da
  paste0("Discriminant analysis of principal components was performed using adegenet",
         if (!is.null(pcs)) paste0(" with ", pcs, " retained principal components") else "",
         if (!is.null(das)) paste0(" and ", das, " discriminant functions") else "",
         ". Group membership was derived from the recorded metadata and was not interpreted as model-based ancestry.")
}

publication_legend_dapc <- function(x) {
  "Discriminant analysis of principal components showing separation among the predefined groups. DAPC coordinates describe discrimination among supplied groups and are not ancestry proportions."
}

publication_method_ibd <- function(x) {
  "Isolation by distance was evaluated by relating pairwise genetic distance to geographic distance for samples with valid coordinates. Mantel and regression statistics were computed only when the required spatial metadata were available."
}

publication_legend_ibd <- function(x) {
  "Relationship between pairwise geographic and genetic distances for samples with available coordinates. The reported association and significance follow the canonical IBD result."
}

publication_method_ancestry <- function(x) {
  backend <- x$backend %||% x$method %||% "the configured ancestry backend"
  k <- x$selected_k %||% x$k
  paste0("Population structure was evaluated with ", backend,
         if (!is.null(k)) paste0(" using K = ", k, " as the selected model") else " across the recorded K range",
         ". Replicates, convergence or fit statistics, cluster-label alignment, and K-selection evidence were retained in the canonical ancestry result. Q matrices were interpreted as ancestry coefficients only for model-based ancestry backends.")
}

publication_legend_ancestry <- function(x) {
  k <- x$selected_k %||% x$k
  paste0("Estimated ancestry coefficients",
         if (!is.null(k)) paste0(" for K = ", k) else "",
         ". Each bar represents one sample and segment heights show inferred cluster membership. Samples use aliases when available; cluster labels are arbitrary and aligned across replicates for comparison.")
}

publication_analysis_narrative <- function(result, name = NULL) {
  kind <- publication_result_kind(result, name)
  method_fun <- get0(paste0("publication_method_", kind), mode = "function", inherits = TRUE)
  legend_fun <- get0(paste0("publication_legend_", kind), mode = "function", inherits = TRUE)
  if (is.null(method_fun)) method_fun <- function(x) paste0("The ", name %||% kind, " analysis was performed using the parameters and software recorded in the canonical result.")
  if (is.null(legend_fun)) legend_fun <- function(x) paste0("Publication output for the ", name %||% kind, " analysis.")
  data.table::data.table(
    analysis = name %||% publication_result_id(result, kind),
    kind = kind,
    method = method_fun(result),
    legend = legend_fun(result),
    citation_keys = paste(publication_analysis_citations(kind, result), collapse = ";")
  )
}

#' Return canonical publication citations for an analysis
#'
#' @param kind Analysis kind.
#' @param result Optional canonical result.
#' @return Character vector of citation keys.
#' @export
publication_analysis_citations <- function(kind, result = NULL) {
  citations <- list(
    pca = c("Zheng2012SNPRelate"),
    ibs = c("Zheng2012SNPRelate"),
    tree = c("Saitou1987NJ", "Paradis2004ape"),
    diversity = c("Nei1978Heterozygosity"),
    fst = c("Weir1984FST"),
    amova = c("Excoffier1992AMOVA"),
    dapc = c("Jombart2010DAPC"),
    ibd = c("Mantel1967"),
    ancestry = c("Alexander2009ADMIXTURE", "Raj2014fastStructure", "Frichot2014sNMF")
  )
  unique(citations[[tolower(kind)]] %||% character())
}

#' Build analysis-specific publication narratives
#'
#' @param project A reproducible project.
#' @return A data table with methods, legends, and citation keys.
#' @export
publication_analysis_narratives <- function(project) {
  validate_popgenvcf_project(project)
  results <- project$results %||% list()
  if (!length(results)) return(data.table::data.table(
    analysis = character(), kind = character(), method = character(), legend = character(), citation_keys = character()))
  rows <- lapply(seq_along(results), function(i) {
    name <- names(results)[[i]]
    if (is.null(name) || is.na(name) || !nzchar(name)) name <- paste0("analysis_", i)
    publication_analysis_narrative(results[[i]], name)
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' Write a BibTeX library for analyses present in a project
#'
#' @param narratives Output from `publication_analysis_narratives()`.
#' @param path Destination `.bib` path.
#' @return Normalized path, invisibly.
#' @export
write_publication_bibliography <- function(narratives, path) {
  keys <- unique(unlist(strsplit(narratives$citation_keys[nzchar(narratives$citation_keys)], ";", fixed = TRUE)))
  entries <- c(
    Zheng2012SNPRelate = "@article{Zheng2012SNPRelate, title={A high-performance computing toolset for relatedness and principal component analysis of SNP data}, author={Zheng, Xiuwen and others}, journal={Bioinformatics}, year={2012}}",
    Saitou1987NJ = "@article{Saitou1987NJ, title={The neighbor-joining method}, author={Saitou, Naruya and Nei, Masatoshi}, journal={Molecular Biology and Evolution}, year={1987}}",
    Paradis2004ape = "@article{Paradis2004ape, title={APE: Analyses of Phylogenetics and Evolution in R language}, author={Paradis, Emmanuel and Claude, Julien and Strimmer, Korbinian}, journal={Bioinformatics}, year={2004}}",
    Nei1978Heterozygosity = "@article{Nei1978Heterozygosity, title={Estimation of average heterozygosity and genetic distance from a small number of individuals}, author={Nei, Masatoshi}, journal={Genetics}, year={1978}}",
    Weir1984FST = "@article{Weir1984FST, title={Estimating F-statistics for the analysis of population structure}, author={Weir, Bruce and Cockerham, Clark}, journal={Evolution}, year={1984}}",
    Excoffier1992AMOVA = "@article{Excoffier1992AMOVA, title={Analysis of molecular variance inferred from metric distances among DNA haplotypes}, author={Excoffier, Laurent and Smouse, Peter and Quattro, Joseph}, journal={Genetics}, year={1992}}",
    Jombart2010DAPC = "@article{Jombart2010DAPC, title={Discriminant analysis of principal components}, author={Jombart, Thibaut and Devillard, Sebastien and Balloux, Francois}, journal={BMC Genetics}, year={2010}}",
    Mantel1967 = "@article{Mantel1967, title={The detection of disease clustering and a generalized regression approach}, author={Mantel, Nathan}, journal={Cancer Research}, year={1967}}",
    Alexander2009ADMIXTURE = "@article{Alexander2009ADMIXTURE, title={Fast model-based estimation of ancestry in unrelated individuals}, author={Alexander, David and Novembre, John and Lange, Kenneth}, journal={Genome Research}, year={2009}}",
    Raj2014fastStructure = "@article{Raj2014fastStructure, title={fastSTRUCTURE: Variational inference of population structure in large SNP data sets}, author={Raj, Anil and Stephens, Matthew and Pritchard, Jonathan}, journal={Genetics}, year={2014}}",
    Frichot2014sNMF = "@article{Frichot2014sNMF, title={Fast and efficient estimation of individual ancestry coefficients}, author={Frichot, Eric and Mathieu, Francois and Trouillon, Thibault and Bouchard, Guillaume and Francois, Olivier}, journal={Genetics}, year={2014}}"
  )
  selected <- unname(entries[intersect(keys, names(entries))])
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(selected, path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
