test_that("character ordering inside the package does not follow the session locale", {
  # testthat itself runs under LC_COLLATE = C, so the user's locale has to be
  # switched on explicitly to see what a real session sees.
  skip_if(inherits(try(withr::with_collate("en_US.UTF-8", NULL), silent = TRUE), "try-error"))
  ids <- c("S1", "S-2", "b", "A", "ne_ld", "neighbor")
  in_package <- function(expr) eval(expr, envir = list2env(list(ids = ids), parent = asNamespace("popgenVCF")))
  withr::with_collate("en_US.UTF-8", {
    if (identical(base::sort(ids), base::sort(ids, method = "radix"))) skip("en_US collation unavailable")
    expect_identical(in_package(quote(sort(ids))), base::sort(ids, method = "radix"))
    expect_identical(in_package(quote(order(ids))), base::order(ids, method = "radix"))
    expect_identical(in_package(quote(order(ids, decreasing = TRUE))), base::order(ids, decreasing = TRUE, method = "radix"))
    # Non-character input and an explicit method are left alone.
    expect_identical(in_package(quote(order(c(3, 1, 2), decreasing = TRUE))), c(1L, 3L, 2L))
    expect_identical(in_package(quote(order(ids, method = "shell"))), base::order(ids, method = "shell"))
    expect_identical(in_package(quote(sort(c(2.5, 1)))), c(1, 2.5))

    spec <- new_publication_fst_spec()
    pairwise <- data.frame(population1 = c("S1", "b"), population2 = c("S-2", "A"), fst = c(0.1, 0.2))
    english <- new_publication_fst_output(spec, pairwise, result_fingerprint = "r")
  })
  withr::with_collate("C", c_locale <- new_publication_fst_output(spec, pairwise, result_fingerprint = "r"))
  expect_identical(english$fingerprint, c_locale$fingerprint)
  expect_identical(english$pairwise$population1, c("A", "S-2"))
  expect_identical(english$pairwise$population2, c("b", "S1"))
})
