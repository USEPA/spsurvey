# Construct one response's domain columns on the original design rows. The
# population case uses the response mask without an artificial grouping column.
contrast_contributions <- function(prepared, response, type, statistic) {
  design <- prepared$design
  data <- design$variables
  domains <- if (is.null(type)) NA_character_ else levels(data[[type]])
  ratio <- statistic != "Total"
  d <- as.numeric(weights(design))
  observed <- !is.na(response)
  estimate <- rep(NA_real_, length(domains))
  counts <- integer(length(domains))
  z <- matrix(0, nrow(data), length(domains), dimnames = list(NULL, domains))
  for (j in seq_along(domains)) {
    v <- as.numeric(observed & if (is.null(type)) TRUE else data[[type]] %in% domains[j])
    counts[j] <- sum(v)
    if (!counts[j]) next
    q <- response
    q[v == 0] <- 0
    ctx <- if (is.null(type)) prepared$greg$population else
      prepared$greg$contexts[[type]][[domains[j]]]
    a <- if (is.null(ctx)) d else ctx$calibrated_weights
    denominator <- if (ratio) sum(a * v) else 1
    if (!is.finite(denominator) || denominator <= 0) next
    estimate[j] <- sum(a * q) / denominator
    u <- if (ratio) q - estimate[j] * v else q
    z[, j] <- if (is.null(ctx)) d * u / denominator else
      as.numeric(greg_residual_contributions(ctx, u)) / denominator
  }
  list(estimate = estimate, counts = counts, contributions = z, observed = observed)
}

# Count responses, not positive category indicators. This check is shared by
# the wrapper and the constructor; each caller chooses its joint block.
contrast_domain_fallback <- function(prepared, observed, type, domains) {
  if (is.null(prepared$greg) && prepared$vartype == "Local" &&
      is.null(prepared$design_names$clusterID) && !is.null(type)) {
    design <- prepared$design
    for (idx in split(which(observed), design$strata[[1]][observed], drop = TRUE)) {
      n <- table(factor(design$variables[[type]][idx], levels = domains))
      if (any(n > 0 & n < 4)) {
        return("A domain has fewer than four responses in a participating stratum.")
      }
    }
  }
  NULL
}
