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
compute_population_genetic_distance <- function(locus_table) {
  empty <- function(populations = character()) list(
    distance = matrix(numeric(0), 0L, 0L, dimnames = list(character(), character())),
    n_snps = 0L, populations = populations
  )
  if (!nrow(locus_table)) return(empty())

  # Excludes loci where any population has zero calls (n_called == 0):
  # allele frequency is undefined there, and Nei's distance needs a valid
  # frequency for every population at every retained locus.
  usable_snps <- locus_table[, .(usable = all(n_called > 0L)), by = snp_id][usable == TRUE, snp_id]
  lt <- locus_table[snp_id %in% usable_snps]
  populations <- sort(unique(locus_table$population))
  if (length(populations) < 2L || !length(usable_snps)) return(empty(populations))

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

  gp <- methods::new("genpop", tab = tab, ploidy = 2L)
  d <- adegenet::dist.genpop(gp, method = 1L)
  list(distance = as.matrix(d), n_snps = length(snp_ids), populations = populations)
}

# ape::nj() errors below 3 tips; mirrors build_nj_tree()'s existing
# Newick-only output (no figure -- this codebase has no tree-plotting
# infrastructure for either the individual or population tree).
build_population_tree <- function(distance, dirs) {
  if (nrow(distance) < 3L) return(NULL)
  tree <- ape::nj(stats::as.dist(distance))
  ape::write.tree(tree, file.path(dirs$trees, "population_Nei_neighbor_joining.nwk"))
  tree
}
