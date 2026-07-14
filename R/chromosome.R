run_chromosome_analyses <- function(gds, qc_snps, final_snps, ids, sample_ids, metadata, cfg) {
  chr_qc <- split(qc_snps, ids$chromosome[match(qc_snps, ids$snp)])
  chr_ld <- split(final_snps, ids$chromosome[match(final_snps, ids$snp)])
  chromosomes <- sort(unique(c(names(chr_qc), names(chr_ld))))
  out <- list()
  for (chr in chromosomes) {
    q <- chr_qc[[chr]] %||% character(); l <- chr_ld[[chr]] %||% character()
    if (length(q) < cfg$analyses$chromosome_min_snps || length(l) < 2L) next
    fst <- run_fst(gds, q, metadata)
    pca <- run_pca(gds, sample_ids, l, metadata, min(3L, cfg$analyses$n_pcs), cfg$compute$threads)
    out[[chr]] <- list(summary = data.table::data.table(chromosome = chr, qc_snps = length(q), ld_snps = length(l),
                                                        global_fst = fst$global, pc1_percent = pca$variance$percent[1]),
                       fst = fst$long, pca = pca$scores)
  }
  out
}

write_chromosome_results <- function(x, dirs) {
  if (!length(x)) return(data.table::data.table())
  summary <- data.table::rbindlist(lapply(x, `[[`, "summary"))
  write_tsv(summary, file.path(dirs$tables, "chromosome_summary.tsv"))
  for (chr in names(x)) {
    safe <- gsub("[^A-Za-z0-9_.-]", "_", chr)
    write_tsv(x[[chr]]$fst, file.path(dirs$chromosomes, paste0(safe, "_pairwise_FST.tsv")))
    write_tsv(x[[chr]]$pca, file.path(dirs$chromosomes, paste0(safe, "_PCA.tsv")))
  }
  summary
}
