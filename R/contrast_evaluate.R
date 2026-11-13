contrast_check <- function(contrasts, subpops, data) {
  named_list <- function(x) is.list(x) && !is.data.frame(x) && length(x) > 0L &&
    !is.null(names(x)) && !anyNA(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))
  if (!named_list(contrasts) || !setequal(names(contrasts), subpops)) {
    stop("contrasts must be a named list with one element for every subpops variable.", call. = FALSE)
  }
  for (type in subpops) {
    if (!named_list(contrasts[[type]])) {
      stop("contrasts$", type, " must be a nonempty list with unique contrast names.", call. = FALSE)
    }
    for (name in names(contrasts[[type]])) {
      x <- contrasts[[type]][[name]]
      label <- paste0("Contrast ", type, ": ", name)
      if (is.numeric(x) && is.null(dim(x))) {
        if (!length(x) || is.null(names(x)) || anyNA(names(x)) ||
            any(!nzchar(names(x))) || anyDuplicated(names(x)) || any(!is.finite(x))) {
          stop(label, " requires finite coefficients with unique domain names.", call. = FALSE)
        }
        variables <- names(x)
      } else if (is.call(x) || is.symbol(x)) {
        # all.vars excludes function-call heads, so log(A) refers to A only.
        # Do not let misspelled variables resolve in the calling environment.
        variables <- all.vars(x)
      } else stop(label, " must be a named numeric vector or quoted expression.", call. = FALSE)
      unknown <- setdiff(variables, levels(data[[type]]))
      if (length(unknown)) stop(label, " names unknown domains: ",
        paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
  }
}

contrast_evaluate <- function(stat, specification, conf, warn_df, labels) {
  if (is.numeric(specification) && all(specification == 0)) {
    stat <- greg_stat(0, matrix(0), ".zero")
    specification <- c(.zero = 0)
  }
  needed <- if (is.numeric(specification)) names(specification)[specification != 0] else all.vars(specification)
  if (!length(needed)) needed <- names(stats::coef(stat))[1]
  # Subset explicitly: an unavailable domain with coefficient zero must not
  # contaminate an unrelated contrast through 0 * NA.
  idx <- match(needed, names(stats::coef(stat)))
  reduced <- greg_stat(stats::coef(stat)[idx], stats::vcov(stat)[idx, idx, drop = FALSE],
    needed, attr(stat, "statistic"))
  if (is.numeric(specification)) {
    specification <- specification[names(specification) %in% needed]
    if (!length(specification)) specification <- stats::setNames(0, needed)
  }
  values <- rep(NA_real_, 5)
  names(values) <- c("Estimate", "StdError", "MarginofError",
    paste0("LCB", conf, "Pct"), paste0("UCB", conf, "Pct"))
  if (all(is.finite(stats::coef(reduced))) && all(is.finite(stats::vcov(reduced)))) {
    result <- tryCatch(suppressWarnings(survey::svycontrast(reduced, specification)),
      error = function(e) stop("Contrast ", labels[1], ": ", labels[2],
        ": ", conditionMessage(e), call. = FALSE))
    estimate <- as.numeric(stats::coef(result))
    variance <- as.numeric(stats::vcov(result))
    if (length(estimate) == 1L && length(variance) == 1L &&
        is.finite(estimate) && is.finite(variance) &&
        variance >= -1e-10 * max(1, abs(stats::vcov(reduced)))) {
      # Remove only a negative variance within the stated roundoff tolerance.
      se <- sqrt(if (variance < 0) 0 else variance)
      margin <- stats::qnorm(.5 + conf / 200) * se
      values[] <- c(estimate, se, margin, estimate - margin, estimate + margin)
    }
  }
  if (anyNA(values)) warn_df <- contrast_warning(warn_df,
    "The contrast value or its uncertainty is unavailable.",
    "The affected contrast was set to NA.", labels)
  list(values = values, warn_df = warn_df)
}
