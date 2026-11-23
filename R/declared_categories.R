declared_categories <- function(data, variables, reserve_total = FALSE,
                                reject_missing = FALSE) {
  variables <- intersect(as.character(unlist(variables)), names(data))
  categories <- setNames(vector("list", length(variables)), variables)
  for (variable in variables) {
    x <- data[[variable]]
    observed <- !is.na(x)
    levels <- if (is.factor(x)) levels(x) else levels(factor(x))
    if (reserve_total && "Total" %in% levels) {
      stop(sprintf('Response "%s" has a category named "Total", which is reserved for the all-categories row. Rename the category.', variable),
        call. = FALSE)
    }
    if (!any(observed)) {
      if (isTRUE(reject_missing) || (identical(reject_missing, "factor") && is.factor(x) && length(levels))) {
        stop(sprintf('"%s" has no non-missing values.', variable), call. = FALSE)
      }
      next
    }
    categories[[variable]] <- levels
  }
  Filter(Negate(is.null), categories)
}

category_columns <- function(data, categories, protected = NULL, will_pad = NULL) {
  variables <- names(categories)
  columns <- setNames(variables, variables)
  for (variable in variables) {
    x <- factor(as.character(data[[variable]]), levels = categories[[variable]])
    if (variable %in% protected) {
      columns[[variable]] <- tail(make.unique(c(names(data), ".spsurvey_category")), 1)
    }
    data[[columns[[variable]]]] <- x
  }
  list(data = data, columns = columns)
}

category_column_names <- function(variables, columns) {
  matches <- match(variables, names(columns), nomatch = 0L)
  variables[matches > 0L] <- unname(columns[matches[matches > 0L]])
  variables
}

category_original_names <- function(variables, columns) {
  matches <- match(variables, unname(columns), nomatch = 0L)
  variables[matches > 0L] <- names(columns)[matches[matches > 0L]]
  variables
}

category_popsize_columns <- function(popsize) {
  if (is.data.frame(popsize)) names(popsize)[-ncol(popsize)]
  else if (is.array(popsize)) names(dimnames(popsize))
  else names(popsize)
}
