test_that("resource policies validate capacity", {
  expect_error(new_execution_resource_policy(threads = 0), "positive")
  expect_error(new_execution_resource_policy(processes = 1.5), "whole numbers")
  expect_error(new_execution_resource_policy(label = ""), "non-empty")

  policy <- new_execution_resource_policy(
    threads = 8L,
    memory_mb = 16384,
    temp_mb = 4096,
    processes = 4L,
    label = "workstation"
  )
  expect_s3_class(policy, "PopgenVCFExecutionResourcePolicy")
  expect_equal(policy$threads, 8L)
  expect_equal(policy$label, "workstation")
})

test_that("module requirements validate declared resources", {
  expect_error(new_module_resource_requirements(threads = 0), "at least one")
  expect_error(new_module_resource_requirements(memory_mb = -1), "non-negative")
  expect_error(new_module_resource_requirements(processes = 1.5), "whole numbers")

  requirements <- new_module_resource_requirements(
    threads = 4L,
    memory_mb = 2048,
    temp_mb = 512,
    processes = 1L
  )
  expect_identical(
    names(requirements),
    c("threads", "memory_mb", "temp_mb", "processes")
  )
})

test_that("exact-capacity requests are admitted", {
  policy <- new_execution_resource_policy(
    threads = 4L,
    memory_mb = 2048,
    temp_mb = 512,
    processes = 1L,
    label = "exact"
  )
  decision <- admit_execution_resources(
    new_module_resource_requirements(4L, 2048, 512, 1L),
    policy
  )

  expect_s3_class(decision, "PopgenVCFExecutionAdmissionDecision")
  expect_true(decision$admitted)
  expect_equal(decision$status, "admitted")
  expect_length(decision$exceeded, 0)
  expect_equal(decision$policy, "exact")
})

test_that("over-capacity requests fail closed with explicit dimensions", {
  policy <- new_execution_resource_policy(
    threads = 4L,
    memory_mb = 1024,
    temp_mb = 256,
    processes = 1L
  )
  decision <- admit_execution_resources(
    new_module_resource_requirements(8L, 2048, 128, 2L),
    policy
  )

  expect_false(decision$admitted)
  expect_equal(decision$status, "resource_unavailable")
  expect_identical(decision$exceeded, c("threads", "memory_mb", "processes"))
})
