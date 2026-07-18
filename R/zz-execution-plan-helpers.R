checkpoint_remaining_plan <- function(full_plan, registry_or_completed, remaining = NULL) {
  if (is.null(remaining)) {
    completed <- registry_or_completed
    remaining <- setdiff(full_plan$order, completed)
  }
  order <- full_plan$order[full_plan$order %in% remaining]
  table <- data.table::copy(full_plan$table[match(order, full_plan$table$module)])
  waves <- full_plan$waves[order]
  structure(
    list(order = order, waves = waves, table = table),
    class = "PopgenVCFExecutionPlan"
  )
}
