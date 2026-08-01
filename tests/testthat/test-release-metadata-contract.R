test_that("packaged software identity is complete and release-consistent", {
  identity <- popgenvcf_software_identity()

  expect_identical(identity$name, "popgenVCF")
  expect_identical(identity$title, "Population Genomics Toolkit for VCF Data")
  expect_identical(identity$citation_title,
                   "popgenVCF: Population Genomics Toolkit for VCF Data")
  expect_identical(identity$version, as.character(utils::packageVersion("popgenVCF")))
  expect_identical(identity$release_status, "released")
  expect_identical(identity$date_released, "2026-08-01")
  expect_identical(identity$doi, "10.5281/zenodo.21747548")
  expect_identical(identity$license$spdx, "MIT")
  expect_identical(identity$author$email, "marc-olivier.duceppe@inspection.gc.ca")
  expect_identical(identity$author$orcid, "0000-0003-2130-0427")
  expect_true(all(c("aut", "cre") %in% identity$author$roles))
  expect_invisible(validate_popgenvcf_software_identity(identity))
})

test_that("development identity rejects premature release claims", {
  identity <- popgenvcf_software_identity()
  identity$release_status <- "development"
  identity$date_released <- NULL
  identity$doi <- NULL

  premature_date <- identity
  premature_date$date_released <- "2026-07-22"
  expect_error(validate_popgenvcf_software_identity(premature_date), "must not claim")

  premature_doi <- identity
  premature_doi$doi <- "10.0000/example"
  expect_error(validate_popgenvcf_software_identity(premature_doi), "must not claim")
})

test_that("installed package citation follows package metadata", {
  identity <- popgenvcf_software_identity()
  citation <- utils::citation("popgenVCF", auto = FALSE)
  expect_gte(length(citation), 1L)

  entry <- citation[[1L]]
  expect_identical(unname(entry$title), identity$citation_title)
  expect_identical(unname(entry$note), paste("R package version", identity$version))
  expect_identical(unname(entry$url), identity$repository)

  text <- paste(format(citation, style = "text"), collapse = "\n")
  expect_false(grepl("R package version 0.8.0", text, fixed = TRUE))
})

test_that("FAIR software records use canonical software identity", {
  identity <- popgenvcf_software_identity()
  project <- new_popgenvcf_project(
    "Metadata integration",
    project_id = "00000000-0000-0000-0000-000000000933"
  )
  metadata <- new_fair_metadata(
    project,
    creators = list(new_fair_creator("Jane Project-Creator"))
  )
  documents <- fair_documents(metadata)

  expect_identical(documents$codemeta$name, identity$name)
  expect_identical(documents$codemeta$version, project$package_version)
  expect_identical(documents$codemeta$codeRepository, identity$repository)
  expect_identical(documents$codemeta$issueTracker, identity$issue_tracker)
  expect_identical(documents$codemeta$license, identity$license$url)
  expect_identical(documents$codemeta$keywords, identity$keywords)
  expect_identical(documents$codemeta$author[[1L]]$givenName,
                   identity$author$given_name)
  expect_identical(documents$codemeta$author[[1L]]$familyName,
                   identity$author$family_name)

  graph <- documents$ro_crate$`@graph`
  software <- graph[vapply(graph, function(entity) {
    is.list(entity) && identical(entity$`@id`, identity$repository)
  }, logical(1L))]
  expect_length(software, 1L)
  expect_identical(software[[1L]]$version, project$package_version)
  expect_identical(software[[1L]]$url, identity$documentation)

  expect_identical(documents$citation_cff$type, "dataset")
  expect_identical(documents$citation_cff$authors[[1L]][["family-names"]],
                   "Project-Creator")
})
