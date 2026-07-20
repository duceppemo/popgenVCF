# Canonical reusable scientific data objects
#
# Versioned contracts for scientifically meaningful objects shared across
# analysis modules. Constructors validate identity and dimensional invariants
# before an object can enter the execution or publication layers.

.canonical_object_schema <- "1.0.0"

.assert_unique_ids <- function(x, field) {
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x))) {
    stop(field, " must be a non-empty character vector without missing values.", call. = FALSE)
  }
  if (anyDuplicated(x)) {
    stop(field, " contains duplicate identities.", call. = FALSE)
  }
  invisible(x)
}

.new_scientific_object <- function(type, payload, identities, provenance = list()) {
  if (!is.character(type) || length(type) != 1L || !nzchar(type)) {
    stop("type must be one non-empty string.", call. = FALSE)
  }
  if (!is.list(payload) || !is.list(identities) || !is.list(provenance)) {
    stop("payload, identities, and provenance must be lists.", call. = FALSE)
  }

  structure(
    list(
      schema_version = .canonical_object_schema,
      object_type = type,
      identities = identities,
      payload = payload,
      provenance = provenance
    ),
    class = c(paste0("PopgenVCF", type), "PopgenVCFScientificObject")
  )
}

#' Construct a canonical genotype object
#'
#' @keywords internal
new_canonical_genotype <- function(genotypes, sample_ids, variant_ids,
                                   ploidy = 2L, allele_coding,
                                   genome_build = NULL, provenance = list()) {
  .assert_unique_ids(sample_ids, "sample_ids")
  .assert_unique_ids(variant_ids, "variant_ids")
  if (!is.matrix(genotypes)) stop("genotypes must be a matrix.", call. = FALSE)
  if (!identical(dim(genotypes), c(length(sample_ids), length(variant_ids)))) {
    stop("genotype dimensions must match sample_ids and variant_ids.", call. = FALSE)
  }
  if (!is.numeric(ploidy) || length(ploidy) != 1L || is.na(ploidy) || ploidy < 1) {
    stop("ploidy must be one positive integer.", call. = FALSE)
  }
  if (!is.character(allele_coding) || length(allele_coding) != 1L || !nzchar(allele_coding)) {
    stop("allele_coding must be explicit.", call. = FALSE)
  }

  .new_scientific_object(
    "Genotype",
    payload = list(
      genotypes = genotypes,
      ploidy = as.integer(ploidy),
      allele_coding = allele_coding,
      genome_build = genome_build
    ),
    identities = list(sample_ids = sample_ids, variant_ids = variant_ids),
    provenance = provenance
  )
}

#' Construct a canonical allele-frequency object
#'
#' @keywords internal
new_canonical_frequency <- function(frequencies, population_ids, variant_ids,
                                    allele, provenance = list()) {
  .assert_unique_ids(population_ids, "population_ids")
  .assert_unique_ids(variant_ids, "variant_ids")
  if (!is.matrix(frequencies)) stop("frequencies must be a matrix.", call. = FALSE)
  if (!identical(dim(frequencies), c(length(population_ids), length(variant_ids)))) {
    stop("frequency dimensions must match population_ids and variant_ids.", call. = FALSE)
  }
  if (any(frequencies < 0 | frequencies > 1, na.rm = TRUE)) {
    stop("frequencies must lie in [0, 1].", call. = FALSE)
  }
  if (!is.character(allele) || length(allele) != 1L || !nzchar(allele)) {
    stop("allele orientation must be explicit.", call. = FALSE)
  }

  .new_scientific_object(
    "Frequency",
    payload = list(frequencies = frequencies, allele = allele),
    identities = list(population_ids = population_ids, variant_ids = variant_ids),
    provenance = provenance
  )
}

#' Construct a canonical genetic-distance object
#'
#' @keywords internal
new_canonical_distance <- function(distance, entity_ids, metric, units = "unitless",
                                   provenance = list()) {
  .assert_unique_ids(entity_ids, "entity_ids")
  if (!is.matrix(distance) || nrow(distance) != ncol(distance) ||
      nrow(distance) != length(entity_ids)) {
    stop("distance must be a square matrix matching entity_ids.", call. = FALSE)
  }
  if (!isTRUE(all.equal(distance, t(distance), check.attributes = FALSE))) {
    stop("distance must be symmetric.", call. = FALSE)
  }
  if (!is.character(metric) || length(metric) != 1L || !nzchar(metric)) {
    stop("metric must be explicit.", call. = FALSE)
  }

  .new_scientific_object(
    "Distance",
    payload = list(distance = distance, metric = metric, units = units),
    identities = list(entity_ids = entity_ids),
    provenance = provenance
  )
}

#' Construct canonical sample/population metadata
#'
#' @keywords internal
new_canonical_metadata <- function(data, sample_id_column = "sample_id",
                                   population_column = NULL, provenance = list()) {
  if (!is.data.frame(data) || !sample_id_column %in% names(data)) {
    stop("data must contain the declared sample identity column.", call. = FALSE)
  }
  .assert_unique_ids(data[[sample_id_column]], sample_id_column)
  if (!is.null(population_column) && !population_column %in% names(data)) {
    stop("declared population column is missing.", call. = FALSE)
  }

  .new_scientific_object(
    "Metadata",
    payload = list(
      data = data,
      sample_id_column = sample_id_column,
      population_column = population_column
    ),
    identities = list(sample_ids = data[[sample_id_column]]),
    provenance = provenance
  )
}
