# Each analysis uses its prepared neighborhoods. Entries retain the exact ordered
# construction inputs: response missingness, strata and adjusted weights therefore
# should not accidentally share an incompatible neighborhood.
localmean_context <- function(local_nbh = 15) {
  context <- new.env(parent = emptyenv())
  context$nbh <- local_nbh
  context$neighborhoods <- list()
  context$group_contexts <- list()
  context$warning_keys <- character()
  context
}

localmean_group_context <- function(context, key) {
  if (is.null(context)) return(NULL)
  key <- as.character(key)
  if (is.null(context$group_contexts[[key]])) {
    context$group_contexts[[key]] <- localmean_context(context$nbh)
  }
  context$group_contexts[[key]]
}

localmean_weight_internal <- function(x, y, prb, local_nbh = 15, local_context = NULL) {
  if (is.null(local_context)) return(localmean_weight(x, y, prb, nbh = local_nbh))
  if (!identical(as.numeric(local_nbh), as.numeric(local_context$nbh))) {
    stop("Internal local_nbh does not match the neighborhood context.", call. = FALSE)
  }
  inputs <- list(x = as.numeric(x), y = as.numeric(y), prb = as.numeric(prb))
  for (entry in local_context$neighborhoods) {
    if (identical(entry$inputs, inputs)) return(entry$weights)
  }
  weights <- localmean_weight(x, y, prb, nbh = local_nbh)
  local_context$neighborhoods[[length(local_context$neighborhoods) + 1L]] <-
    list(inputs = inputs, weights = weights)
  weights
}
