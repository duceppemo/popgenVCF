test_that("qc_reports orders sequential/independent filtering steps as ordered factors, not alphabetical", {
  vq <- data.table::data.table(
    snp_id = 1:10,
    pass_maf = c(rep(TRUE, 8), rep(FALSE, 2)),
    pass_missing = c(rep(TRUE, 7), rep(FALSE, 3)),
    pass_combined = c(rep(TRUE, 6), rep(FALSE, 4))
  )
  final_snps <- 1:4
  reports <- popgenVCF:::qc_reports(vq, final_snps)

  # Alphabetically "After LD pruning" would sort before "Input biallelic",
  # and "Final LD-pruned" before "Input biallelic" -- both genuinely
  # different from, and misleading relative to, the real filtering
  # sequence these tables are built in.
  expect_s3_class(reports$sequential$step, "factor")
  expect_identical(
    levels(reports$sequential$step),
    c("Input biallelic", "After MAF", "After missingness", "After LD pruning")
  )
  expect_identical(as.character(reports$sequential$step), levels(reports$sequential$step))

  expect_s3_class(reports$independent$criterion, "factor")
  expect_identical(
    levels(reports$independent$criterion),
    c("Input biallelic", "Pass MAF", "Pass missingness", "Pass both", "Final LD-pruned")
  )
  expect_identical(as.character(reports$independent$criterion), levels(reports$independent$criterion))
})

test_that("qc_reports computes real, hand-verified sequential and independent counts", {
  vq <- data.table::data.table(
    snp_id = 1:10,
    pass_maf = c(rep(TRUE, 8), rep(FALSE, 2)),
    pass_missing = c(rep(TRUE, 7), rep(FALSE, 3)),
    pass_combined = c(rep(TRUE, 6), rep(FALSE, 4))
  )
  final_snps <- 1:4
  reports <- popgenVCF:::qc_reports(vq, final_snps)

  expect_identical(reports$sequential$variants, c(10L, 8L, 6L, 4L))
  expect_identical(reports$sequential$removed_at_step, c(0L, 2L, 2L, 2L))
  expect_identical(reports$independent$variants, c(10L, 8L, 7L, 6L, 4L))
})
