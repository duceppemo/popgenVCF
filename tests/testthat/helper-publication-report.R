publication_report_test_manuscript <- function() {
  x <- list(
    schema_version = "1.0",
    project_id = "project-001",
    project_digest = "project-digest",
    publication_digest = "publication-digest",
    title = "Deterministic report",
    authors = data.table::data.table(
      name = "A. Author", affiliation = NA_character_, email = NA_character_,
      orcid = NA_character_, corresponding = TRUE
    ),
    abstract = "Abstract.", keywords = "population genetics",
    introduction = "Introduction.", methods = "Generated methods.",
    results = "Author interpretation.", discussion = "Discussion.",
    captions = data.table::data.table(id = character(), caption = character()),
    artifacts = data.table::data.table(), software = data.table::data.table(),
    parameters = data.table::data.table(),
    declarations = list(
      data_availability = "Data statement.",
      software_availability = "Software statement.",
      reproducibility = "Reproducibility statement.",
      funding = "Funding statement.",
      author_contributions = "Contribution statement.",
      competing_interests = "None."
    ),
    bibliography = NULL
  )
  class(x) <- c("PopgenVCFManuscript", "list")
  validate_manuscript(x)
  x
}
