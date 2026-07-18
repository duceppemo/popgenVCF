checkpoint_remaining_plan <- function(full_plan, registry_or_completed, remaining = NULL) {
  registry <- NULL
  if (is.null(remaining)) {
    completed <- registry_or_completed
    remaining <- setdiff(full_plan$order, completed)
  } else {
    registry <- registry_or_completed
  }

  order <- full_plan$order[full_plan$order %in% remaining]
  module_names <- full_plan$table[["module"]]
  rows <- match(order, module_names)
  table <- data.table::copy(full_plan$table[rows])

  waves <- if (!is.null(registry)) {
    execution_wave_map(registry, order)
  } else {
    full_plan$waves[order]
  }
  if (nrow(table)) {
    table[, wave := unname(waves[module])]
  }

  structure(
    list(order = order, waves = waves, table = table),
    class = "PopgenVCFExecutionPlan"
  )
}
