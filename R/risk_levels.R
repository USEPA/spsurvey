# Resolve the two risk labels against the response factors used for estimation.
risk_levels <- function(categories, variables, responses, role, greg) {
  argument <- paste0(role, "_levels")
  variables_argument <- paste0("vars_", role)
  default <- is.null(categories)
  if (default) categories <- rep(list(c("Poor", "Good")), length(variables))

  invalid <- function(message) list(levels = categories, errors = message)
  if (!is.list(categories)) {
    return(invalid(paste("Argument", argument, "must be a list.")))
  }
  if (length(categories) != length(variables)) {
    return(invalid(paste("Argument", argument, "must be the same length as argument", variables_argument)))
  }
  if (any(lengths(categories) != 2L)) {
    return(invalid(paste("Each element of argument", argument, "must contain only two values.")))
  }
  if (!all(vapply(categories, is.character, logical(1)))) {
    return(invalid(paste("Each element of argument", argument, "must contain character values.")))
  }
  if (is.null(names(categories))) names(categories) <- variables
  if (anyDuplicated(names(categories)) || !setequal(names(categories), variables)) {
    return(invalid(paste("Names for the", argument, "list must match the variables in", variables_argument)))
  }
  categories <- categories[variables]
  errors <- character()
  for (variable in intersect(variables, names(responses))) {
    response <- responses[[variable]]
    # An entirely missing response with default labels still uses the
    # existing GREG no-response result.
    if (greg && default && all(is.na(response))) next
    unknown <- setdiff(categories[[variable]], levels(response))
    if (length(unknown)) {
      errors <- c(errors, paste0(argument, ' for "', variable,
        '" contains undeclared categories: ', paste(shQuote(unknown), collapse = ", "), "."))
    }
  }
  list(levels = categories, errors = errors)
}

risk_margin_label <- function(responses) {
  categories <- unique(unlist(lapply(responses, levels), use.names = FALSE))
  if (!"Sum" %in% categories) return("Sum")
  tail(make.unique(c(categories, "Sum")), 1L)
}

risk_addmargins <- function(x, margin_label) {
  result <- addmargins(x)
  if (margin_label != "Sum") {
    labels <- dimnames(result)
    for (i in seq_along(labels)) labels[[i]][length(labels[[i]])] <- margin_label
    dimnames(result) <- labels
  }
  result
}
