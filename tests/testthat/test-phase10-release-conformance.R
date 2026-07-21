test_that("release conformance binds every distribution channel", {
  descriptor <- phase10_api_descriptor()
  compatibility <- compare_phase10_api_descriptors(descriptor, descriptor)
  guidance <- data.frame(
    operation_id = character(), action = character(),
    successor_operation = character(), schema_guidance = character(),
    deprecated_in = character(), removal_not_before = character(),
    stringsAsFactors = FALSE
  )
  policy <- phase10_api_evolution_policy()
  plan <- new_phase10_api_migration_plan(compatibility, guidance, policy)
  identities <- do.call(rbind, lapply(
    c("package", "container", "apptainer", "documentation", "scientific_validation"),
    function(channel) new_phase10_release_identity(
      channel, "0.9.0", paste0("sha256:", channel), descriptor$fingerprint
    )
  ))

  manifest <- new_phase10_release_conformance(
    descriptor, compatibility, plan, identities, policy
  )

  expect_s3_class(manifest, "PopgenVCFPublicAPIReleaseConformance")
  expect_true(manifest$release_ready)
  expect_true(assert_phase10_release_conformance(
    manifest, descriptor, compatibility, plan, policy
  ))
  expect_match(
    paste(phase10_release_conformance_report(
      manifest, descriptor, compatibility, plan, policy
    ), collapse = "\n"),
    "Release ready: `true`",
    fixed = TRUE
  )
})

test_that("release identities fail closed on missing and mismatched channels", {
  descriptor <- phase10_api_descriptor()
  identities <- do.call(rbind, lapply(
    c("package", "container", "apptainer", "documentation"),
    function(channel) new_phase10_release_identity(
      channel, "0.9.0", paste0("sha256:", channel), descriptor$fingerprint
    )
  ))
  compatibility <- compare_phase10_api_descriptors(descriptor, descriptor)
  guidance <- data.frame(
    operation_id = character(), action = character(),
    successor_operation = character(), schema_guidance = character(),
    deprecated_in = character(), removal_not_before = character(),
    stringsAsFactors = FALSE
  )
  policy <- phase10_api_evolution_policy()
  plan <- new_phase10_api_migration_plan(compatibility, guidance, policy)

  expect_error(
    new_phase10_release_conformance(
      descriptor, compatibility, plan, identities, policy
    ),
    "every required release channel"
  )

  identities <- rbind(
    identities,
    new_phase10_release_identity(
      "scientific_validation", "0.9.0", "sha256:validation",
      descriptor$fingerprint
    )
  )
  identities$api_fingerprint[identities$channel == "container"] <- "wrong"
  expect_error(
    new_phase10_release_conformance(
      descriptor, compatibility, plan, identities, policy
    ),
    "canonical public API fingerprint"
  )
})

test_that("release conformance detects mutation", {
  descriptor <- phase10_api_descriptor()
  compatibility <- compare_phase10_api_descriptors(descriptor, descriptor)
  guidance <- data.frame(
    operation_id = character(), action = character(),
    successor_operation = character(), schema_guidance = character(),
    deprecated_in = character(), removal_not_before = character(),
    stringsAsFactors = FALSE
  )
  policy <- phase10_api_evolution_policy()
  plan <- new_phase10_api_migration_plan(compatibility, guidance, policy)
  identities <- do.call(rbind, lapply(
    c("package", "container", "apptainer", "documentation", "scientific_validation"),
    function(channel) new_phase10_release_identity(
      channel, "0.9.0", paste0("sha256:", channel), descriptor$fingerprint
    )
  ))
  manifest <- new_phase10_release_conformance(
    descriptor, compatibility, plan, identities, policy
  )
  manifest$release_identities$artifact_digest[[1L]] <- "sha256:tampered"

  expect_error(
    validate_phase10_release_conformance(
      manifest, descriptor, compatibility, plan, policy
    ),
    "fingerprint verification failed"
  )
})
