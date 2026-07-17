# Correct direct-policy selection after all regeneration helpers are loaded.
#
# This replacement preserves the public contract while selecting the most
# restrictive policy from the matching dependency rows, rather than trying to
# recover a name from `which.max()` itself.
manuscript_regeneration_table <- function(x) {
  deps <- regeneration_dependencies(x$dependencies)
  changes <- regeneration_changes(x$changes)
  sections <- sort(unique(deps$section_id))
  result <- data.table::data.table(
    section_id = sections,
    state = "unaffected",
    reason = "No changed dependency",
    source_changes = ""
  )

  changed <- sort(changes$dependency_id)
  direct <- deps[dependency_type == "input" & dependency_id %in% changed]
  policy_rank <- c(regenerate = 1L, manual_review = 2L, blocked = 3L)

  for (section in sort(unique(direct$section_id))) {
    rows <- direct[section_id == section]
    chosen <- rows$policy[[which.max(policy_rank[rows$policy])]]
    state <- switch(
      chosen,
      regenerate = "affected",
      manual_review = "manual_review",
      blocked = "blocked"
    )
    ids <- sort(unique(rows$dependency_id))
    result[section_id == section, `:=`(
      state = state,
      reason = paste0("Direct changed input: ", paste(ids, collapse = ", ")),
      source_changes = paste(ids, collapse = ";")
    )]
  }

  section_edges <- deps[dependency_type == "section"]
  changed_state <- function(value) value %in% c("affected", "manual_review", "blocked")
  state_rank <- c(unaffected = 0L, affected = 1L, manual_review = 2L, blocked = 3L)

  repeat {
    previous <- result$state
    for (i in seq_len(nrow(section_edges))) {
      upstream <- section_edges$dependency_id[[i]]
      downstream <- section_edges$section_id[[i]]
      upstream_row <- result[section_id == upstream]
      if (!nrow(upstream_row) || !changed_state(upstream_row$state[[1L]])) next

      propagated <- switch(
        section_edges$policy[[i]],
        regenerate = "affected",
        manual_review = "manual_review",
        blocked = "blocked"
      )
      current <- result[section_id == downstream, state]
      if (state_rank[[propagated]] >= state_rank[[current]]) {
        result[section_id == downstream, `:=`(
          state = propagated,
          reason = paste0("Depends on changed section: ", upstream),
          source_changes = upstream_row$source_changes[[1L]]
        )]
      }
    }
    if (identical(previous, result$state)) break
  }

  data.table::setorderv(result, "section_id")
  result
}
