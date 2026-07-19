test_that("execution supervision API is exported", {
  required_exports <- c(
    "new_execution_cancellation_token",
    "request_execution_cancellation",
    "validate_execution_cancellation_token",
    "execute_analysis_plan_with_cancellation",
    "execute_analysis_registry_with_cancellation",
    "new_execution_resource_policy",
    "new_module_resource_requirements",
    "admit_execution_resources",
    "new_external_command",
    "validate_external_command",
    "run_external_command",
    "new_external_process_supervision_policy",
    "run_supervised_external_command",
    "new_external_process_workspace_policy",
    "run_supervised_external_command_in_workspace"
  )

  exports <- getNamespaceExports("popgenVCF")
  expect_setequal(intersect(required_exports, exports), required_exports)

  for (name in required_exports) {
    expect_true(is.function(getExportedValue("popgenVCF", name)), info = name)
  }
})

test_that("execution supervision print methods are registered", {
  required_classes <- c(
    "PopgenVCFExecutionCancellationToken",
    "PopgenVCFExternalCommand",
    "PopgenVCFExternalProcessResult",
    "PopgenVCFExternalProcessSupervisionPolicy",
    "PopgenVCFExternalProcessWorkspacePolicy"
  )

  for (class_name in required_classes) {
    method <- getS3method("print", class_name, optional = TRUE)
    expect_true(is.function(method), info = class_name)
  }
})

test_that("registered execution objects dispatch their print methods", {
  token <- new_execution_cancellation_token("namespace-test")
  command <- new_external_command(file.path(R.home("bin"), "Rscript"))
  supervision <- new_external_process_supervision_policy()
  workspace <- new_external_process_workspace_policy()

  expect_output(print(token), "PopgenVCFExecutionCancellationToken", fixed = TRUE)
  expect_output(print(command), "PopgenVCFExternalCommand", fixed = TRUE)
  expect_output(print(supervision), "PopgenVCFExternalProcessSupervisionPolicy", fixed = TRUE)
  expect_output(print(workspace), "PopgenVCFExternalProcessWorkspacePolicy", fixed = TRUE)

  result <- run_external_command(new_external_command(
    file.path(R.home("bin"), "Rscript"),
    args = c("-e", "quit(status = 0)")
  ))
  expect_output(print(result), "PopgenVCFExternalProcessResult", fixed = TRUE)
})
