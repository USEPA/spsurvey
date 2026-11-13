# One response/category and one statistic at a time. Every column refers to
# the same sampled rows, even when domains use different calibration totals.
contrast_joint <- function(prepared, response, type, indicator, statistic,
                            subset_local, local_nbh, local_context,
                            category = NULL) {
  design <- prepared$design
  dn <- prepared$design_names
  data <- design$variables
  domains <- levels(data[[type]])
  ratio <- statistic != "Total"
  d <- as.numeric(weights(design))
  observed <- !is.na(response)
  estimate <- rep(NA_real_, length(domains))
  counts <- integer(length(domains))
  z <- matrix(0, nrow(data), length(domains), dimnames = list(NULL, domains))
  warn_df <- prepared$warn_df
  label <- paste(c(indicator, category, statistic), collapse = ": ")
  for (j in seq_along(domains)) {
    v <- as.numeric(observed & data[[type]] %in% domains[j])
    counts[j] <- sum(v)
    if (!counts[j]) next
    q <- response
    q[v == 0] <- 0
    ctx <- prepared$greg$contexts[[type]][[domains[j]]]
    a <- if (is.null(ctx)) d else ctx$calibrated_weights
    denominator <- if (ratio) sum(a * v) else 1
    if (!is.finite(denominator) || denominator <= 0) next
    estimate[j] <- sum(a * q) / denominator
    u <- if (ratio) q - estimate[j] * v else q
    z[, j] <- if (is.null(ctx)) d * u / denominator else
      as.numeric(greg_residual_contributions(ctx, u)) / denominator
  }
  available <- is.finite(estimate)
  covariance <- matrix(NA_real_, length(domains), length(domains))
  if (any(available)) {
    if (prepared$vartype == "Local" && subset_local && is.null(prepared$greg)) {
      # Use the standard direct marginal routines, including their fallbacks.
      # This diagonal matrix is the explicitly requested independence approximation.
      if (!is.null(category)) {
        marginal <- prepared$subset_categories
        marginal <- marginal[marginal$Category == category, ]
        marginal <- marginal[match(domains[available], marginal$Subpopulation), ]
        se <- if (ratio) marginal$StdError.P / 100 else marginal$StdError.U
      } else {
        column <- tail(make.unique(c(names(data), ".contrast_response")), 1)
        design$variables[[column]] <- response
        marginal <- if (ratio) mean_localmean else total_localmean
        result <- marginal(type, domains, length(domains), which(available), column,
          design, dn, estimate, 1, !is.null(warn_df), warn_df,
          TRUE, local_nbh, local_context)
        se <- result$stderr[available]
        warn_df <- result$warn_df
      }
      covariance[available, available] <- diag(se^2, nrow = sum(available))
    } else {
      fallback <- NULL
      if (is.null(prepared$greg) && prepared$vartype == "Local" && is.null(dn$clusterID)) {
        # The direct marginal routines require four domain responses in each
        # participating stratum. If any domain fails that rule, retain one
        # coherent survey matrix rather than mixing fallback diagonals.
        for (idx in split(which(observed), design$strata[[1]][observed], drop = TRUE)) {
          n <- table(factor(data[[type]][idx], levels = domains[available]))
          if (any(n > 0 & n < 4)) {
            fallback <- "A domain has fewer than four responses in a participating stratum."
            break
          }
        }
      }
      result <- contrast_covariance(design, z[, available, drop = FALSE],
        if (is.null(prepared$greg)) observed else rep(TRUE, nrow(data)),
        prepared$vartype, dn, warn_df, c(type, NA_character_, label),
        local_nbh, local_context, fallback)
      covariance[available, available] <- result$covariance
      warn_df <- result$warn_df
    }
  }
  if (any(!available)) warn_df <- contrast_warning(warn_df,
    paste("Unavailable domain estimates:", paste(domains[!available], collapse = ", ")),
    "Contrasts involving these domains were set to NA.", c(type, NA_character_, label))
  if (statistic == "Proportion") {
    estimate <- 100 * estimate
    covariance <- 10000 * covariance
  }
  dimnames(covariance) <- list(domains, domains)
  stat <- greg_stat(estimate, covariance, domains, statistic = tolower(statistic))
  attr(stat, "nResp") <- stats::setNames(counts, domains)
  attr(stat, "subset_independence") <- prepared$vartype == "Local" &&
    subset_local && is.null(prepared$greg)
  list(stat = stat, warn_df = warn_df)
}

contrast_covariance <- function(design, z, observed, vartype, dn, warn_df,
                                 labels, local_nbh, local_context, fallback = NULL) {
  # Z already contains the analysis weights. Let survey apply all design/FPC/
  # PPS corrections to the unweighted contributions, without weighting twice.
  reference <- stats::vcov(survey::svytotal(z / as.numeric(weights(design)), design))
  if (vartype != "Local" || !is.null(fallback)) {
    if (!is.null(fallback)) warn_df <- contrast_warning(warn_df, fallback,
      "Survey covariance was used for all domains in this contrast calculation.", labels)
    if (!contrast_valid_covariance(reference)) {
      reference[,] <- NA_real_
      warn_df <- contrast_warning(warn_df, "The joint survey covariance is unavailable or invalid.",
        "Contrast uncertainty was set to NA.", labels)
    }
    return(list(covariance = reference, warn_df = warn_df))
  }
  rows <- which(observed)
  covariance <- matrix(0, ncol(z), ncol(z))
  failure <- NULL
  if (!is.null(dn$clusterID)) {
    if (length(unique(design$cluster[[1]][rows])) != length(unique(design$cluster[[1]]))) {
      failure <- "A sampled PSU has no usable responses."
    } else {
      part <- localmean_cluster_design(design, z, dn, local_nbh,
        local_context, "covariance", rows)
      covariance <- part$covariance
      if (length(part$notes)) warn_df <- localmean_cluster_warning(warn_df, part$notes, labels)
      if (is.null(covariance)) failure <- "The two-stage local covariance could not be calculated."
    }
  } else for (idx in split(rows, design$strata[[1]][rows], drop = TRUE)) {
    if (length(idx) < 4L) {
      failure <- "A sampling stratum has fewer than four responding sites."
      break
    }
    nb <- localmean_weight_internal(design$variables[[dn$xcoord]][idx],
      design$variables[[dn$ycoord]][idx], 1 / weights(design)[idx],
      local_nbh = local_nbh,
      local_context = localmean_group_context(local_context, paste0("stratum:", design$strata[[1]][idx[1]])))
    if (is.null(nb)) {
      failure <- "Local neighborhood construction failed."
      break
    }
    covariance <- covariance + localmean_cov(z[idx, , drop = FALSE], nb)
  }
  if (is.null(failure) && !contrast_valid_covariance(covariance)) {
    failure <- "The joint local covariance was not finite and positive semidefinite."
  }
  if (!is.null(failure)) {
    covariance <- reference
    warn_df <- contrast_warning(warn_df, failure,
      "Survey covariance was used for all domains in this contrast calculation.", labels)
  }
  if (!contrast_valid_covariance(covariance)) {
    covariance[,] <- NA_real_
    warn_df <- contrast_warning(warn_df, "The joint survey covariance is unavailable or invalid.",
      "Contrast uncertainty was set to NA.", labels)
  }
  list(covariance = covariance, warn_df = warn_df)
}

contrast_valid_covariance <- function(v) {
  all(is.finite(v)) && min(eigen(v, symmetric = TRUE, only.values = TRUE)$values) >=
    -1e-10 * max(1, abs(v))
}
