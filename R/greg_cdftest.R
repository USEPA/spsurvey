greg_cdftest <- function(data, vars, subpops, greg, base, design_names,
                         vartype, testname, nclass) {
  rows <- list()
  warn_df <- greg$warn_df
  for (type in subpops) {
    domains <- levels(data[[type]])
    if (length(domains) < 2L) {
      warn_df <- greg_warning(warn_df, "CDF testing requires at least two domains.",
        "No comparison was produced.", c(type, NA, NA))
      next
    }
    pairs <- combn(domains, 2, simplify = FALSE)
    for (variable in vars) {
      y <- data[[variable]]
      observed <- sort(y[is.finite(y)])
      bounds <- unique(observed[pmax(1L, floor(seq(length(observed) / nclass,
        length(observed), length.out = nclass)))])
      if (length(bounds) < 2L) {
        warn_df <- greg_warning(warn_df, "Too few distinct CDF bins for testing.",
          "No comparison was produced.", c(type, NA, variable))
        next
      }
      for (pair in pairs) {
        masks <- lapply(pair, function(domain) data[[type]] %in% domain)
        contexts <- greg$contexts[[type]][pair]
        design <- greg_as_design(base, contexts, masks, vartype, design_names)
        group <- rep(NA_character_, nrow(data))
        for (j in 1:2) group[masks[[j]]] <- pair[j]
        design$variables$.greg_group <- factor(group, levels = pair)
        design$variables$.greg_bin <- cut(y, c(-Inf, bounds))
        result <- tryCatch(survey::svychisq(~.greg_group + .greg_bin, design,
          statistic = testname), error = function(e) e)
        if (inherits(result, "error") || !is.finite(result$p.value)) {
          reason <- if (inherits(result, "error")) conditionMessage(result) else "Undefined test covariance or degrees of freedom."
          warn_df <- greg_warning(warn_df, reason,
            "The GREG CDF comparison was set to NA.", c(type, paste(pair, collapse = " / "), variable))
          value <- df1 <- df2 <- p <- NA_real_
        } else {
          value <- as.numeric(result$statistic)
          df1 <- as.numeric(result$parameter[1])
          df2 <- as.numeric(result$parameter[2])
          p <- result$p.value
        }
        rows[[length(rows) + 1L]] <- data.frame(Type = type,
          Subpopulation_1 = pair[1], Subpopulation_2 = pair[2], Indicator = variable,
          statistic = value, Degrees_of_Freedom_1 = df1,
          Degrees_of_Freedom_2 = df2, p_Value = p)
      }
    }
  }
  greg_publish_warnings(warn_df)
  out <- do.call(rbind, rows)
  if (is.null(out)) return(out)
  names(out)[5] <- switch(testname, Wald = "Wald Statistic",
    adjWald = "Adjusted Wald Statistic", Chisq = "Rao-Scott First Order Statistic",
    F = "Rao-Scott Second Order Statistic")
  if (testname == "Chisq") {
    out <- out[, -7]
    names(out)[6] <- "Degrees_of_Freedom"
  }
  rownames(out) <- NULL
  out
}
