# Nei's (1972) standard genetic distance between populations, computed from
# allele frequencies -- distinct from the existing individual-level "tree"
# module, which builds an NJ tree from IBS distance between samples, not
# from population allele frequencies. Uses adegenet::dist.genpop() (method
# 1 = Nei's standard distance, its documented default), already a hard
# dependency (used elsewhere for DAPC), rather than hand-deriving the
# formula -- the genpop object is built directly from
# compute_diversity()'s already-computed per-population allele counts, no
# genind/df2genind() intermediate needed. Verified against an independent
# hand calculation before shipping (see NEWS.md).
# Split out from compute_population_genetic_distance() so the tree-bootstrap
# path (bootstrap_population_nj_tree(), below) can build this once and then
# resample SNP column-pairs from it directly per replicate, rather than
# re-running dcast() from the raw locus table on every replicate -- both
# simpler (bootstrap resampling is then just positional column indexing,
# matching ape::boot.phylo()'s own column-resampling model for a plain
# matrix) and faster.
population_allele_count_matrix <- function(locus_table) {
  empty <- list(tab = NULL, populations = character(), n_snps = 0L)
  if (!nrow(locus_table)) return(empty)

  # Excludes loci where any population has zero calls (n_called == 0):
  # allele frequency is undefined there, and Nei's distance needs a valid
  # frequency for every population at every retained locus.
  usable_snps <- locus_table[, .(usable = all(n_called > 0L)), by = snp_id][usable == TRUE, snp_id]
  lt <- locus_table[snp_id %in% usable_snps]
  populations <- sort(unique(locus_table$population))
  if (length(populations) < 2L || !length(usable_snps)) {
    return(list(tab = NULL, populations = populations, n_snps = 0L))
  }

  alt_wide <- data.table::dcast(lt, population ~ snp_id, value.var = "alternate_allele_count")
  ref_wide <- data.table::dcast(lt, population ~ snp_id, value.var = "reference_allele_count")
  data.table::setorder(alt_wide, population)
  data.table::setorder(ref_wide, population)
  snp_ids <- names(alt_wide)[-1L]

  tab <- matrix(0, nrow = length(populations), ncol = 2L * length(snp_ids))
  col_names <- character(2L * length(snp_ids))
  for (i in seq_along(snp_ids)) {
    tab[, 2L * i - 1L] <- ref_wide[[snp_ids[i]]]
    tab[, 2L * i] <- alt_wide[[snp_ids[i]]]
    col_names[2L * i - 1L] <- paste0(snp_ids[i], ".0")
    col_names[2L * i] <- paste0(snp_ids[i], ".1")
  }
  colnames(tab) <- col_names
  rownames(tab) <- alt_wide$population
  list(tab = tab, populations = populations, n_snps = length(snp_ids))
}

population_genpop_distance <- function(tab) {
  gp <- methods::new("genpop", tab = tab, ploidy = 2L)
  as.matrix(adegenet::dist.genpop(gp, method = 1L))
}

compute_population_genetic_distance <- function(locus_table) {
  built <- population_allele_count_matrix(locus_table)
  if (is.null(built$tab)) {
    return(list(
      distance = matrix(numeric(0), 0L, 0L, dimnames = list(character(), character())),
      n_snps = 0L, populations = built$populations
    ))
  }
  list(
    distance = population_genpop_distance(built$tab),
    n_snps = built$n_snps, populations = built$populations
  )
}

bootstrap_population_nj_tree <- function(reference_tree, locus_table, replicates, workers, seed) {
  if (replicates <= 0L) return(NULL)
  built <- population_allele_count_matrix(locus_table)
  if (is.null(built$tab) || built$n_snps < 2L) return(NULL)
  tab <- built$tab
  n_snps <- built$n_snps
  seeds <- tree_bootstrap_replicate_seeds(seed, replicates)
  build_one <- function(i) {
    set.seed(seeds[[i]])
    idx <- sample.int(n_snps, n_snps, replace = TRUE)
    cols <- as.vector(rbind(2L * idx - 1L, 2L * idx))
    tryCatch(ape::nj(stats::as.dist(population_genpop_distance(tab[, cols, drop = FALSE]))),
             error = function(e) e)
  }
  trees <- run_tree_bootstrap_replicates(replicates, workers, build_one)
  support <- bootstrap_tree_support(reference_tree, trees)
  if (is.null(support)) return(NULL)
  list(support = support, replicates = length(trees))
}

# ape::nj() errors below 3 tips.
build_population_tree <- function(distance, dirs, cfg = NULL, locus_table = NULL) {
  if (nrow(distance) < 3L) return(NULL)
  tree <- ape::nj(stats::as.dist(distance))
  bootstrap <- NULL
  if (isTRUE(cfg$analyses$tree_bootstrap$enabled) && !is.null(locus_table)) {
    bootstrap <- bootstrap_population_nj_tree(
      tree, locus_table, cfg$analyses$tree_bootstrap$replicates, cfg$compute$threads, cfg$compute$seed
    )
    if (!is.null(bootstrap)) tree$node.label <- as.character(bootstrap$support)
  }
  ape::write.tree(tree, file.path(dirs$trees, "population_Nei_neighbor_joining.nwk"))
  attr(tree, "bootstrap_replicates") <- if (is.null(bootstrap)) 0L else bootstrap$replicates
  tree
}
