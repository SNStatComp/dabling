# R/utils_hierarchy.R
compute_hierarchy_aggregation <- function(schema_dt) {
  sch <- data.table::copy(schema_dt)
  
  # Ensure required cols
  if (!"id" %in% names(sch)) stop("schema_dt must contain column 'id'")
  if (!"enriched_text" %in% names(sch)) stop("schema_dt must contain column 'enriched_text'")
  if (!"parent_id" %in% names(sch)) sch[, parent_id := NA_character_]
  
  # Normalize types
  sch[, `:=`(
    id        = as.character(id),
    parent_id = as.character(parent_id)
  )]
  
  # Empty/NA parents as NA_character_
  sch[is.na(parent_id) | !nzchar(parent_id), parent_id := NA_character_]
  
  # Guard: if some parent_id doesn't exist as id, treat as root
  valid_ids <- sch$id
  sch[!is.na(parent_id) & !(parent_id %in% valid_ids), parent_id := NA_character_]
  
  # Compute depth (root depth = 0L), with simple cycle guard
  parent_map <- setNames(sch$parent_id, sch$id)
  depth_of <- function(node) {
    steps <- 0L
    seen  <- character()
    cur   <- node
    while (TRUE) {
      p <- parent_map[[cur]]
      if (is.null(p) || is.na(p) || !nzchar(p)) break
      if (p %in% seen) {        # cycle detected
        steps <- steps + 1L
        break
      }
      seen  <- c(seen, cur)
      cur   <- p
      steps <- steps + 1L
      if (steps > 10000L) break # hard safety cap
    }
    steps
  }
  
  sch[, depth := vapply(id, depth_of, integer(1L))]
  
  # Start with own enriched_text
  sch[, agg_enriched_text := enriched_text]
  
  # Aggregate children up to parents from deepest to root
  # For each parent, append all children's current agg_enriched_text
  depths <- sort(unique(sch$depth), decreasing = TRUE)
  for (d in depths) {
    # take nodes at this depth; push their agg up one level
    cur_nodes <- sch[depth == d]
    if (!nrow(cur_nodes)) next
    # Map parent -> paste(child_agg)
    up <- cur_nodes[!is.na(parent_id),
                    .(child_concat = paste(agg_enriched_text, collapse = " ")),
                    by = parent_id]
    if (nrow(up)) {
      for (i in seq_len(nrow(up))) {
        pid <- up$parent_id[i]
        add <- up$child_concat[i]
        sch[id == pid, agg_enriched_text := paste(agg_enriched_text, add)]
      }
    }
  }
  
  sch[]
}

