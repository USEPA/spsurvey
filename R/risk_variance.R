# Clamp only roundoff in a scalar delta-method variance g' V g.
risk_variance <- function(variance, gradient, covariance) {
  if (is.finite(variance) && variance < 0) {
    scale <- sum(abs(outer(as.numeric(gradient), as.numeric(gradient)) * covariance))
    if (is.finite(scale) && -variance <= 100 * .Machine$double.eps * scale) {
      variance[] <- 0
    }
  }
  variance
}

# Keep survey's point estimate and delta method; repair its scalar variance
# before SE/confint take a square root. No change to the covariance matrix.
risk_contrast <- function(stat, expression) {
  result <- survey::svycontrast(stat, expression)
  variance <- stats::vcov(result)
  if (is.finite(variance) && variance < 0) {
    value <- eval(stats::deriv(expression, names(stats::coef(stat))),
      as.list(stats::coef(stat)))
    attr(result, "var") <- risk_variance(variance, attr(value, "gradient"),
      stats::vcov(stat))
  }
  result
}
