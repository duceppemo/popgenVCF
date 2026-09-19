metadata_capabilities <- function(metadata, metadata_supplied = TRUE,
                                  geographic_columns = c("latitude", "longitude")) {
  columns <- names(metadata)
  population <- if ("population" %in% columns) {
    trimws(as.character(metadata$population))
  } else character()
  has_population <- "population" %in% columns && length(population) == nrow(metadata) &&
    all(!is.na(population) & nzchar(population))
  population_levels <- if (has_population) {
    data.table::uniqueN(population)
  } else 0L
  # The configured coordinate columns (input.geographic_columns), not a
  # hardcoded "latitude"/"longitude": with custom column names the Mantel and
  # spatial-autocorrelation modules were reported "no complete
  # latitude/longitude pairs available" and skipped, even though both read
  # the configured names themselves. Non-numeric entries count as missing,
  # matching those modules' own as.numeric()/is.finite() handling.
  has_coordinates <- length(geographic_columns) == 2L && all(geographic_columns %in% columns) && {
    lat <- suppressWarnings(as.numeric(metadata[[geographic_columns[1L]]]))
    lon <- suppressWarnings(as.numeric(metadata[[geographic_columns[2L]]]))
    any(is.finite(lat) & is.finite(lon))
  }
  list(
    metadata_supplied = isTRUE(metadata_supplied),
    sample = "sample" %in% columns,
    population = has_population,
    population_levels = as.integer(population_levels),
    coordinates = has_coordinates,
    columns = columns
  )
}

analysis_capability_table <- function(registry, capabilities) {
  modules <- names(registry$modules)
  population_modules <- intersect(
    modules,
    c("diversity", "bottleneck", "fst", "genome_scan", "dapc", "amova", "clonality", "sexbias", "bootstrap", "chromosome", "ne_ld",
      "population_tree", "population_assignment")
  )
  multi_population_modules <- intersect(
    modules,
    c("fst", "genome_scan", "amova", "chromosome", "population_tree", "population_assignment")
  )
  n_populations <- as.integer(
    capabilities$population_levels %||%
      if (isTRUE(capabilities$population)) 1L else 0L
  )
  coordinate_modules <- intersect(modules, c("mantel", "isolation_by_distance", "ibd", "spatial_autocorrelation", "spatial_pca", "spca", "maps"))
  sample_modules <- intersect(modules, c(
    "pca", "ibs", "mds", "nj", "neighbor_joining", "neighbour_joining",
    "admixture", "faststructure", "snmf"
  ))

  enabled <- modules
  reason <- stats::setNames(rep("available", length(modules)), modules)

  if (!isTRUE(capabilities$population)) {
    enabled <- setdiff(enabled, population_modules)
    reason[population_modules] <- if (isTRUE(capabilities$metadata_supplied)) {
      "complete population annotations unavailable"
    } else {
      "metadata not supplied; population annotations unavailable"
    }
  }
  # Coordinate modules depend on coordinates alone. They used to be disabled
  # whenever population annotations were incomplete as well, although neither
  # the Mantel/isolation-by-distance test nor spatial autocorrelation uses
  # population labels (the partial Mantel control is simply skipped without
  # them) -- so a study with coordinates but no, or partly missing, population
  # labels silently lost both analyses.
  if (!isTRUE(capabilities$coordinates)) {
    enabled <- setdiff(enabled, coordinate_modules)
    reason[coordinate_modules] <- if (isTRUE(capabilities$metadata_supplied)) {
      "no complete latitude/longitude pairs available"
    } else {
      "metadata not supplied; spatial annotations unavailable"
    }
  }
  if (isTRUE(capabilities$population) && n_populations < 2L) {
    enabled <- setdiff(enabled, multi_population_modules)
    reason[multi_population_modules] <- "at least two populations are required"
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
    n_populations = n_populations,
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
