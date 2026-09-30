greg_publish_warnings <- function(warn_df) {
  if (!is.null(warn_df) && nrow(warn_df)) {
    warn_df <<- warn_df
    message("GREG analysis produced warning messages; use warnprnt() to inspect warn_df.")
  }
}

greg_stat <- function(estimate, covariance, names = base::names(estimate)) {
  structure(as.numeric(estimate), names = names, var = covariance,
    statistic = "total", class = "svystat")
}

greg_risk <- function(data, responses, stressors, response_levels, stressor_levels,
                      subpops, greg, design_names, vartype, conf, kind) {
  rows <- list()
  warn_df <- greg$warn_df
  critical <- stats::qnorm(.5 + conf / 200)
  labels <- c("RespPoor_StressPoor", "RespPoor_StressGood",
    "RespGood_StressPoor", "RespGood_StressGood")
  expression <- switch(kind,
    relrisk = quote(log(t11 / (t11 + t21)) - log(t12 / (t12 + t22))),
    diffrisk = quote(t11 / (t11 + t21) - t12 / (t12 + t22)),
    attrisk = quote(log((t11 + t12 + t21 + t22) * t12 / ((t11 + t12) * (t12 + t22)))))
  for (type in subpops) for (domain in levels(data[[type]])) {
    for (response in responses) for (stressor in stressors) {
      r <- data[[response]]
      s <- data[[stressor]]
      rl <- response_levels[[response]]
      sl <- stressor_levels[[stressor]]
      keep <- data[[type]] %in% domain & !is.na(r) & !is.na(s)
      q <- cbind(r == rl[1] & s == sl[1], r == rl[1] & s == sl[2],
        r == rl[2] & s == sl[1], r == rl[2] & s == sl[2]) * 1
      est <- greg_estimate(greg$contexts[[type]][[domain]], q, keep, FALSE,
        vartype, design_names, conf, warn_df, c(type, domain, response))
      warn_df <- est$warn_df
      cells <- est$estimate
      total <- sum(cells)
      risks <- cells[1:2] / (cells[1:2] + cells[3:4])
      valid <- all(is.finite(cells)) && all(cells >= 0) &&
        all(cells[1:2] + cells[3:4] > 0)
      if (kind == "relrisk") valid <- valid && all(cells[1:2] > 0)
      if (kind == "attrisk") valid <- valid && cells[2] > 0 && sum(cells[1:2]) > 0
      estimate <- se <- lower <- upper <- NA_real_
      if (valid) {
        transformed <- risk_contrast(greg_stat(cells, est$covariance, c("t11", "t12", "t21", "t22")), expression)
        value <- as.numeric(stats::coef(transformed))
        se <- as.numeric(survey::SE(transformed))
        ci <- as.numeric(stats::confint(transformed, level = conf / 100))
        if (kind == "relrisk") {
          estimate <- exp(value)
          lower <- exp(ci[1])
          upper <- exp(ci[2])
        } else if (kind == "attrisk") {
          estimate <- 1 - exp(value)
          lower <- 1 - exp(ci[2])
          upper <- 1 - exp(ci[1])
        } else {
          estimate <- value
          lower <- ci[1]
          upper <- ci[2]
        }
      } else {
        warn_df <- greg_warning(warn_df,
          "Calibrated risk cells do not support the requested ratio or logarithmic transformation.",
          "Risk inference was set to NA; cell totals and proportions were retained.",
          c(type, domain, response))
      }
      row <- data.frame(Type = type, Subpopulation = domain, Response = response,
        Stressor = stressor, nResp = sum(keep), Estimate = estimate)
      if (kind == "relrisk") {
        row$Estimate_num <- risks[1]
        row$Estimate_denom <- risks[2]
      } else if (kind == "diffrisk") {
        row$Estimate_StressPoor <- risks[1]
        row$Estimate_StressGood <- risks[2]
      }
      suffix <- if (kind == "diffrisk") "" else "_log"
      row[[paste0("StdError", suffix)]] <- se
      row[[paste0("MarginofError", suffix)]] <- critical * se
      row[[paste0("LCB", conf, "Pct")]] <- lower
      row[[paste0("UCB", conf, "Pct")]] <- upper
      row$WeightTotal <- total
      row[paste0("Count_", labels)] <- as.list(colSums(q[keep, , drop = FALSE]))
      row[paste0("Prop_", labels)] <- as.list(cells / total)
      rows[[length(rows) + 1L]] <- row
    }
  }
  greg_publish_warnings(warn_df)
  do.call(rbind, rows)
}
