metadata_capabilities <- function(metadata, metadata_supplied = TRUE) {
  columns <- names(metadata)
  has_population <- "population" %in% columns &&
    all(!is.na(metadata$population) & nzchar(metadata$population))
  has_coordinates <- all(c("latitude", "longitude") %in% columns) &&
    any(stats::complete.cases(metadata[, c("latitude", "longitude"), with = FALSE]))
  list(
    metadata_supplied = isTRUE(metadata_supplied),
    sample = "sample" %in% columns,
    population = has_population,
    coordinates = has_coordinates,
    columns = columns
  )
}

analysis_capability_table <- function(registry, capabilities) {
  modules <- names(registry$modules)
  population_modules <- intersect(modules, c("diversity", "fst", "dapc", "amova", "bootstrap"))
  coordinate_modules <- intersect(modules, c("mantel", "isolation_by_distance", "ibd", "spatial_pca", "spca", "maps"))
  sample_modules <- intersect(modules, c(
    "pca", "ibs", "mds", "nj", "neighbor_joining", "neighbour_joining",
    "admixture", "faststructure", "snmf"
  ))

  enabled <- modules
  reason <- stats::setNames(rep("available", length(modules)), modules)

  if (!isTRUE(capabilities$population)) {
    disabled <- union(population_modules, coordinate_modules)
    enabled <- setdiff(enabled, disabled)
    reason[population_modules] <- if (isTRUE(capabilities$metadata_supplied)) {
      "complete population annotations unavailable"
    } else {
      "metadata not supplied; population annotations unavailable"
    }
    reason[coordinate_modules] <- if (isTRUE(capabilities$metadata_supplied)) {
      "population and/or usable coordinates unavailable"
    } else {
      "metadata not supplied; spatial annotations unavailable"
    }
  } else if (!isTRUE(capabilities$coordinates)) {
    enabled <- setdiff(enabled, coordinate_modules)
    reason[coordinate_modules] <- "no complete latitude/longitude pairs available"
  }

  if (!isTRUE(capabilities$metadata_supplied)) {
    reason[sample_modules] <- "available from VCF sample IDs"
  }

  data.table::data.table(
    module = modules,
    available = modules %in% enabled,
    reason = unname(reason[modules]),
    metadata_supplied = isTRUE(capabilities$metadata_supplied),
    has_population = isTRUE(capabilities$population),
    has_coordinates = isTRUE(capabilities$coordinates)
  )
}

resolve_capability_modules <- function(registry, capabilities, selected = NULL) {
  table <- analysis_capability_table(registry, capabilities)
  available <- table[available == TRUE, module]
  if (is.null(selected)) return(available)
  requested_unavailable <- setdiff(selected, available)
  if (length(requested_unavailable)) {
    details <- table[module %in% requested_unavailable, paste0(module, " (", reason, ")")]
    warning("Skipping unavailable analysis module(s): ", paste(details, collapse = ", "), call. = FALSE)
  }
  intersect(selected, available)
}

resolve_pipeline_modules <- function(registry, capabilities, config, selected = NULL) {
  configured <- selected
  if (is.null(configured)) {
    configured <- names(registry$modules)[vapply(
      registry$modules,
      module_is_enabled,
      logical(1L),
      config = config
    )]
  }
  resolve_capability_modules(registry, capabilities, configured)
}
