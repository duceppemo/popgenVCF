# Genuine maximum-likelihood individual-level tree (GTR+Gamma, Lewis (2001)
# ascertainment-bias correction) -- distinct from, and complementary to, the
# existing "tree" module's neighbour-joining tree from IBS distance. Applies
# only to the individual-level tree: ML nucleotide substitution models
# operate on per-taxon sequence characters, and there is no standard,
# defensible way to apply them to the population-level tree, which is built
# from allele-*frequency* distances (Nei's D), not per-sample sequences.
#
# phangorn (a Suggests dependency, checked at module-enable time -- this
# adds no SystemRequirements, unlike ADMIXTURE/fastStructure/sNMF, since it
# is a pure R package with a compiled likelihood backend, no external
# binary) implements the ascertainment-bias correction natively
# (`pml(ASC = TRUE)`, "allows to estimate models like Lewis' Mkv" per its own
# documentation) -- the correction a SNP-only alignment with no invariant
# sites recorded genuinely requires for a valid likelihood, not an optional
# refinement. Verified real timing before committing to this design: a full
# ML search (GTR+Gamma+ASC, NNI topology optimization) on the quickstart
# dataset (160 samples, 357 LD-pruned SNPs) takes ~2.5 seconds; 100 bootstrap
# replicates (phangorn::bootstrap.pml()'s own native parallel support)
# extrapolate to well under a minute with a few cores.

ml_tree_iupac_heterozygote <- c(
  AG = "R", GA = "R", CT = "Y", TC = "Y", GC = "S", CG = "S",
  AT = "W", TA = "W", GT = "K", TG = "K", AC = "M", CA = "M"
)

# Encodes a samples x SNPs reference-allele-dosage matrix (0/1/2/NA) into an
# IUPAC-coded DNA character matrix for phangorn::phyDat(type = "DNA"):
# homozygous reference/alternate become the plain base, heterozygous becomes
# the standard IUPAC ambiguity code for that base pair (phyDat/pml already
# treat ambiguity codes as "compatible with either state" during likelihood
# calculation, standard practice for real sequence data), missing becomes
# "N". Loci whose ref/alt alleles are not both single standard DNA bases
# (A/C/G/T, case-insensitive, and distinct) are dropped rather than
# guessed at -- SNPRelate's biallelic-SNP-only extraction should already
# guarantee this for a real VCF, but this is checked directly, not assumed,
# since a malformed allele string would otherwise silently corrupt the
# encoding.
ml_tree_encode_dna <- function(genotype, ref, alt) {
  ref <- toupper(as.character(ref)); alt <- toupper(as.character(alt))
  bases <- c("A", "C", "G", "T")
  valid <- ref %in% bases & alt %in% bases & ref != alt
  genotype <- genotype[, valid, drop = FALSE]
  ref <- ref[valid]; alt <- alt[valid]
  out <- matrix("N", nrow(genotype), ncol(genotype), dimnames = dimnames(genotype))
  for (j in seq_len(ncol(genotype))) {
    dosage <- genotype[, j]
    out[!is.na(dosage) & dosage == 0, j] <- ref[j]
    out[!is.na(dosage) & dosage == 2, j] <- alt[j]
    het <- which(!is.na(dosage) & dosage == 1)
    if (length(het)) out[het, j] <- ml_tree_iupac_heterozygote[[paste0(ref[j], alt[j])]]
  }
  list(matrix = out, n_used = ncol(genotype), n_dropped = sum(!valid))
}

# Builds the ML tree and, when requested, its bootstrap support. Reuses the
# existing NJ-tree machinery (bootstrap_tree_support(), R/tree_bootstrap.R)
# for the final support tally, since phangorn::bootstrap.pml() already
# returns a plain list of ape::phylo replicate trees -- the identical input
# shape that helper already consumes for the NJ trees.
run_ml_tree <- function(genotype, ref, alt, sample_ids, seed,
                        threads = 1L, bootstrap_replicates = 100L) {
  if (!requireNamespace("phangorn", quietly = TRUE)) {
    stop("The maximum-likelihood tree module requires the optional 'phangorn' package", call. = FALSE)
  }
  rownames(genotype) <- sample_ids
  encoded <- ml_tree_encode_dna(genotype, ref, alt)
  # GTR (5 free rate parameters) + a gamma shape parameter + 2n-3 branch
  # lengths for n tips is a lot of free parameters, and too few SNPs
  # relative to sample count is a real, directly observed numerical failure
  # mode in phangorn's branch-length optimizer, not merely a slow-but-correct
  # edge case: a 20-tip/40-SNP case reliably failed; the real quickstart
  # dataset (160 tips, 357 SNPs -- a *lower* SNPs-per-tip ratio) works fine,
  # so the failure is evidently driven by specific data patterns (near-
  # identical individuals within too little distinguishing data) rather than
  # a simple aggregate ratio -- not something a general pre-flight formula
  # can reliably predict from n_tips and n_snps alone. This floor only
  # catches the most obviously degenerate cases; the tryCatch() below is
  # the actual safety net, converting whatever numerical failure genuinely
  # occurs into a clear, actionable error instead of phangorn's cryptic one.
  min_snps <- 4L
  if (encoded$n_used < min_snps) {
    stop(
      "Maximum-likelihood tree requires at least ", min_snps, " usable biallelic ",
      "SNP loci with single-base alleles; ", encoded$n_used, " available", call. = FALSE
    )
  }

  pd <- phangorn::phyDat(encoded$matrix, type = "DNA")
  starting_tree <- phangorn::NJ(phangorn::dist.ml(pd, model = "JC69"))
  fit <- phangorn::pml(starting_tree, pd, ASC = TRUE, k = 4)

  set.seed(as.integer(seed))
  fit_opt <- tryCatch(
    phangorn::optim.pml(
      fit, model = "GTR", optNni = TRUE, optBf = TRUE, optQ = TRUE,
      optGamma = TRUE, optEdge = TRUE, rearrangement = "NNI",
      control = phangorn::pml.control(trace = 0)
    ),
    error = function(e) {
      stop(
        "Maximum-likelihood tree optimization failed to converge (", conditionMessage(e), "); ",
        "this is a known phangorn numerical-stability failure mode when there is too little ",
        "SNP data relative to the number of samples -- try more SNPs or fewer samples",
        call. = FALSE
      )
    }
  )
  # optim.pml() does not always propagate this failure mode as an R error --
  # confirmed directly: it can instead return normally with a non-finite
  # logLik (an internal recovery inside phangorn's own optimizer, not
  # something the tryCatch above ever sees). Checked explicitly rather than
  # trusting a successful return to mean a successful fit.
  if (!is.finite(fit_opt$logLik)) {
    stop(
      "Maximum-likelihood tree optimization did not converge to a finite likelihood; ",
      "this is a known phangorn numerical-stability failure mode when there is too little ",
      "SNP data relative to the number of samples -- try more SNPs or fewer samples",
      call. = FALSE
    )
  }
  tree <- fit_opt$tree

  bootstrap_n <- 0L
  bootstrap_failed <- FALSE
  if (bootstrap_replicates > 0L) {
    # A resampled-with-replacement replicate can, by chance, draw a locus
    # subset too uninformative to fit at all (a real, directly observed
    # failure -- "cannot unroot a tree with less than three edges" -- from a
    # degenerate/near-star resampled topology, not a fixture artifact: more
    # likely with small sample/locus counts, not reproduced in a real
    # 160-sample/357-SNP run, but not something a fixed SNP count can rule
    # out in general). The main tree above is already a complete, valid
    # result on its own; failing the whole analysis over a supplementary
    # confidence measure would be disproportionate, so this degrades to "no
    # bootstrap support computed" instead of propagating the error.
    bs_trees <- tryCatch(
      phangorn::bootstrap.pml(
        fit_opt, bs = bootstrap_replicates, optNni = TRUE, model = "GTR",
        multicore = threads > 1L, mc.cores = max(1L, threads),
        control = phangorn::pml.control(trace = 0)
      ),
      error = function(e) NULL
    )
    support <- if (!is.null(bs_trees)) bootstrap_tree_support(tree, bs_trees) else NULL
    if (!is.null(support)) {
      tree$node.label <- as.character(support)
      bootstrap_n <- length(bs_trees)
    } else {
      bootstrap_failed <- TRUE
    }
  }
  attr(tree, "bootstrap_replicates") <- bootstrap_n

  list(
    tree = tree,
    log_likelihood = as.numeric(fit_opt$logLik),
    model = "GTR+Gamma+ASC",
    n_snps_used = encoded$n_used,
    n_snps_dropped = encoded$n_dropped,
    bootstrap_failed = bootstrap_failed
  )
}
