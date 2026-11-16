contrast_pairs <- function(subpops, data, original) {
  contrasts <- stats::setNames(vector("list", length(subpops)), subpops)
  for (type in subpops) {
    # Input validation may recreate factors. Preserve the user's pair direction
    # without changing the order of the existing numerical joint objects.
    lev <- if (is.factor(original[[type]])) levels(original[[type]]) else levels(data[[type]])
    lev <- lev[lev %in% data[[type]]]
    if (length(lev) < 2L) {
      stop("Subpopulation variable ", type, " must have at least two represented levels.", call. = FALSE)
    }
    pairs <- utils::combn(lev, 2, simplify = FALSE)
    contrasts[[type]] <- lapply(pairs, function(pair) stats::setNames(c(1, -1), pair))
    names(contrasts[[type]]) <- make.unique(vapply(pairs, paste, character(1), collapse = " - "), sep = "..")
  }
  contrasts
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
