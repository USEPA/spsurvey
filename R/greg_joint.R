# A joint vector can use distinct domain calibration contexts. Its linearized
# columns must be combined before computing covariance, including outside-
# domain contributions from every population-calibrated context.
greg_residual_contributions <- function(context, q) {
  d <- context$original_weights
  qr.resid(context$qr, as.matrix(q) * sqrt(d)) / sqrt(d) * context$calibrated_weights
}

greg_joint_covariance <- function(context, z, vartype, design_names, warn_df = NULL) {
  base <- context$base
  # survey supplies every nonlocal design correction, including FPC and PPS.
  covariance <- stats::vcov(survey::svytotal(z / context$original_weights, base))
  if (vartype != "Local") return(list(covariance = covariance, warn_df = warn_df))
  local <- matrix(0, ncol(z), ncol(z))
  fallback <- NULL
  for (idx in split(seq_len(nrow(z)), base$strata[[1]], drop = TRUE)) {
    if (length(idx) < 4L) {
      fallback <- "A sampling stratum has fewer than four sites."
      break
    }
    nb <- localmean_weight(base$variables[[design_names$xcoord]][idx],
      base$variables[[design_names$ycoord]][idx], 1 / context$original_weights[idx])
    if (is.null(nb)) {
      fallback <- "Local neighborhood construction failed."
      break
    }
    local <- local + localmean_cov(z[idx, , drop = FALSE], nb)
  }
  if (is.null(fallback) && (any(!is.finite(local)) ||
      min(eigen(local, symmetric = TRUE, only.values = TRUE)$values) < -1e-10 * max(1, abs(local)))) {
    fallback <- "The joint local covariance is not finite and positive semidefinite."
  }
  if (is.null(fallback)) covariance <- local else {
    warn_df <- greg_warning(warn_df, fallback,
      "Survey's residual-based covariance was used for the entire joint estimate.")
  }
  list(covariance = covariance, warn_df = warn_df)
}

greg_joint_total <- function(contexts, responses, vartype, design_names, warn_df = NULL) {
  estimate <- rep(0, ncol(responses[[1]]))
  z <- matrix(0, nrow(responses[[1]]), length(estimate))
  for (j in seq_along(contexts)) {
    q <- responses[[j]]
    q[is.na(q)] <- 0
    if (any(!is.finite(q))) stop("GREG joint responses must be finite or missing.", call. = FALSE)
    estimate <- estimate + as.numeric(stats::coef(survey::svytotal(q, contexts[[j]]$design)))
    z <- z + greg_residual_contributions(contexts[[j]], q)
  }
  covariance <- greg_joint_covariance(contexts[[1]], z, vartype, design_names, warn_df)
  list(stat = greg_stat(estimate, covariance$covariance, colnames(responses[[1]])),
    contributions = z, warn_df = covariance$warn_df)
}

# This internal survey-design adapter lets survey own nonlinear tests while
# svytotal/svymean supply the appropriate joint calibration covariance.
greg_as_design <- function(base, contexts, masks, vartype, design_names) {
  base$prob <- 1 / Reduce(`+`, Map(function(ctx, mask) ctx$calibrated_weights * mask, contexts, masks))
  base$postStrata <- NULL
  base$greg <- list(contexts = contexts, masks = masks, vartype = vartype, design_names = design_names)
  class(base) <- c("greg_design", class(base))
  base
}

greg_model_response <- function(x, design) {
  if (inherits(x, "formula")) x <- stats::model.frame(x, design$variables, na.action = stats::na.pass)
  if (is.data.frame(x)) {
    x <- do.call(cbind, lapply(names(x), function(name) {
      column <- x[[name]]
      if (is.factor(column)) {
        out <- outer(as.character(column), levels(column), `==`) * 1
        colnames(out) <- paste0(name, levels(column))
      } else {
        out <- as.matrix(column)
        if (is.null(colnames(out))) colnames(out) <- name
      }
      out
    }))
  }
  as.matrix(x)
}

#' @export
#' @method svytotal greg_design
#' @importFrom survey svytotal
svytotal.greg_design <- function(x, design, na.rm = FALSE, ...) {
  q <- greg_model_response(x, design)
  complete <- stats::complete.cases(q)
  if (!na.rm && any(!complete & Reduce(`|`, design$greg$masks))) {
    return(greg_stat(rep(NA_real_, ncol(q)), matrix(NA_real_, ncol(q), ncol(q)), colnames(q)))
  }
  q[!complete, ] <- 0
  responses <- lapply(design$greg$masks, function(mask) q * mask)
  ans <- greg_joint_total(design$greg$contexts, responses, design$greg$vartype, design$greg$design_names)
  greg_publish_warnings(ans$warn_df)
  ans$stat
}

#' @export
#' @method svymean greg_design
#' @importFrom survey svymean
svymean.greg_design <- function(x, design, na.rm = FALSE, ...) {
  q <- greg_model_response(x, design)
  complete <- stats::complete.cases(q)
  names <- colnames(q)
  q <- cbind(q, .denominator = as.numeric(complete))
  q[!complete, ] <- NA_real_
  totals <- svytotal.greg_design(q, design, na.rm = na.rm)
  k <- length(totals)
  denominator <- as.numeric(totals[k])
  estimate <- as.numeric(totals[-k]) / denominator
  jacobian <- cbind(diag(k - 1), -estimate) / denominator
  greg_stat(estimate, jacobian %*% stats::vcov(totals) %*% t(jacobian), names)
}
