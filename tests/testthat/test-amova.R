
test_that("genlight_from_gds pins diploid ploidy instead of letting adegenet infer it per sample", {
  # adegenet infers ploidy from each individual's largest genotype value, so
  # a sample with no homozygous-alternate call was treated as haploid: its
  # heterozygous calls entered DAPC as allele frequency 1.0, and
  # poppr.amova() stopped with "min(ploidy(x)) == max(ploidy(x)) is not TRUE".
  geno <- rbind(s1 = c(0, 1, 2, 1, 0, 2), s2 = c(0, 1, 1, 1, 0, 0), s3 = c(2, 2, 1, 0, 1, 2))
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = rownames(geno), population = c("A", "A", "B")
  ))
  gl <- popgenVCF:::genlight_from_gds(geno, rownames(geno), metadata)
  expect_identical(as.integer(adegenet::ploidy(gl)), c(2L, 2L, 2L))
  expect_equal(unname(as.matrix(gl) / adegenet::ploidy(gl)), unname(geno / 2))
})

test_that("run_amova_analysis completes when some samples carry no homozygous-alternate call", {
  set.seed(11L)
  n <- 30L; n_loci <- 80L
  pop <- rep(c("A", "B", "C"), 10L)
  freq <- c(A = 0.15, B = 0.5, C = 0.85)
  geno <- t(vapply(seq_len(n), function(i) stats::rbinom(n_loci, 2, freq[[pop[i]]]), numeric(n_loci)))
  ids <- paste0("s", seq_len(n))
  dimnames(geno) <- list(ids, paste0("l", seq_len(n_loci)))
  expect_true(any(apply(geno, 1L, max) < 2))
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(sample = ids, population = pop))
  res <- popgenVCF:::run_amova_analysis(geno, ids, metadata, permutations = 0L, seed = 1L)
  expect_true(all(is.finite(res$phi$Phi)))
})
