metadata_capabilities <- function(metadata, metadata_supplied = TRUE) {
  columns <- names(metadata)
  has_population <- "population" %in% columns && any(!is.na(metadata$population) & nzchar(metadata$population))
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
  vcf_only_modules <- intersect(modules, c("pca", "ibs", "mds", "nj", "neighbor_joining", "neighbour_joining"))
  enabled <- modules
  reason <- rep("available", length(modules))
  names(reason) <- modules

  if (!isTRUE(capabilities$metadata_supplied)) {
    enabled <- character()
    reason[] <- "metadata not supplied; VCF-only QC workflow"
  } else {
    if (!isTRUE(capabilities$population)) {
      disabled <- union(population_modules, coordinate_modules)
      enabled <- setdiff(enabled, disabled)
      reason[population_modules] <- "population column unavailable"
      reason[coordinate_modules] <- "population and/or coordinates unavailable"
    } else if (!isTRUE(capabilities$coordinates)) {
      enabled <- setdiff(enabled, coordinate_modules)
      reason[coordinate_modules] <- "latitude/longitude unavailable"
    }
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
  available <- table[available, module]
  if (is.null(selected)) return(available)
  requested_unavailable <- setdiff(selected, available)
  if (length(requested_unavailable)) {
    details <- table[module %in% requested_unavailable, paste0(module, " (", reason, ")")]
    warning("Skipping unavailable analysis module(s): ", paste(details, collapse = ", "), call. = FALSE)
  }
  intersect(selected, available)
}
