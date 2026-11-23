declared_category_fixture <- function() {
  set.seed(5)
  n <- 48L
  data.frame(site = seq_len(n), weight = rep(c(2, 3), each = n / 2),
    domain = factor(rep(c("West", "East"), each = n / 2)),
    x = runif(n), y = runif(n),
    response = factor(rep(c("Poor", "Good"), each = 2, length.out = n),
      levels = c("Poor", "Absent", "Good")),
    stressor = factor(rep(c("Poor", "Good"), length.out = n)))
}

declared_category_risk_counts <- function(fun) {
  d <- declared_category_fixture()
  d$weight <- 2
  levels(d$stressor) <- c(levels(d$stressor), "Absent")
  d$domain[1:2] <- NA
  d$response[3] <- NA
  d$stressor[4] <- NA
  for (observed in c(FALSE, TRUE)) {
    data <- d
    if (observed) {
      data$response[5:8] <- "Absent"
      data$stressor[5:8] <- "Absent"
    }
    for (vt in c("Local", "SRS")) for (greg in c(FALSE, TRUE)) {
      ans <- suppressMessages(fun(data, vars_response = "response", vars_stressor = "stressor",
        subpops = "domain", siteID = "site", xcoord = "x", ycoord = "y", vartype = vt,
        formula = if (greg) ~1, popsize = if (greg) c("(Intercept)" = 96)))
      expect_equal(setNames(ans$nResp, ans$Subpopulation)[c("East", "West")],
        c(East = 24, West = 20))
      expect_equal(setNames(ans$WeightTotal, ans$Subpopulation)[c("East", "West")],
        c(East = 48, West = if (greg && observed) 32 else 40))
    }
  }
  for (column in c("response", "stressor")) {
    data <- d
    data[[column]][] <- NA
    expect_error(fun(data, "response", "stressor", vartype = "SRS"),
      paste0('"', column, '" has no non-missing values.'), fixed = TRUE)
  }
}

declared_category_risk_sum <- function(fun) {
  d <- declared_category_fixture()
  for (role in c("response", "stressor")) for (observed in c(FALSE, TRUE)) {
    for (extra in c(FALSE, TRUE)) for (vt in c("Local", "SRS")) {
      data <- d
      levels(data[[role]]) <- c(levels(data[[role]]), "Sum", if (extra) "Sum.1")
      if (observed) data[[role]][seq_len(4)] <- "Sum"
      other <- data
      levels(other[[role]])[levels(other[[role]]) == "Sum"] <- "Other"
      for (selected in c(FALSE, TRUE)) for (stratified in c(FALSE, TRUE)) {
        args <- list(dframe = data, vars_response = "response", vars_stressor = "stressor",
          subpops = "domain", All_Sites = TRUE, siteID = "site",
          xcoord = "x", ycoord = "y", vartype = vt,
          stratumID = if (stratified) "domain")
        if (selected) args[[paste0(role, "_levels")]] <- setNames(list(c("Sum", "Good")), role)
        expected <- args
        expected$dframe <- other
        if (selected) expected[[paste0(role, "_levels")]] <- setNames(list(c("Other", "Good")), role)
        a <- suppressMessages(suppressWarnings(do.call(fun, args)))
        b <- suppressMessages(suppressWarnings(do.call(fun, expected)))
        expect_equal(a, b, tolerance = 1e-12)
      }
    }
  }
}

declared_category_risk_boundaries <- function(fun) {
  d <- declared_category_fixture()
  d$response <- droplevels(d$response)
  controls <- function(data, greg, explicit, all_sites = TRUE) {
    args <- list(dframe = data, vars_response = "response", vars_stressor = "stressor",
      subpops = "domain", All_Sites = all_sites, siteID = "site", vartype = "SRS",
      formula = if (greg) ~1, popsize = if (greg) c("(Intercept)" = sum(data$weight)))
    if (explicit) {
      args$response_levels <- list(response = c("Poor", "Good"))
      args$stressor_levels <- list(stressor = c("Poor", "Good"))
    }
    assign("warn_df", NULL, envir = globalenv())
    result <- suppressMessages(do.call(fun, args))
    list(result = result, warnings = get("warn_df", globalenv()))
  }
  for (boundary in c("no exposed", "no unexposed", "no adverse", "no adverse unexposed")) {
    alter <- function(data, rows) {
      if (boundary == "no exposed") data$stressor[rows] <- "Good"
      if (boundary == "no unexposed") data$stressor[rows] <- "Poor"
      if (boundary == "no adverse") data$response[rows] <- "Good"
      if (boundary == "no adverse unexposed") data$response[rows] <- data$stressor[rows]
      data
    }
    global <- alter(d, rep(TRUE, nrow(d)))
    domain <- alter(d, d$domain == "West")
    west <- function(x) {
      if (is.null(x)) return(NULL)
      y <- x[x[[if ("Subpopulation" %in% names(x)) "Subpopulation" else "subpop"]] %in%
        "West", , drop = FALSE]
      rownames(y) <- NULL
      if (nrow(y)) y else NULL
    }
    for (greg in c(FALSE, TRUE)) {
      default <- controls(global, greg, FALSE)
      explicit <- controls(global, greg, TRUE)
      expected <- controls(domain, greg, TRUE)
      expect_equal(default, explicit)
      expect_setequal(default$result$Subpopulation, c("West", "East", "All Sites"))
      # The shared row can inherit a numeric column from a different
      # domain with a defined estimate.
      compare <- function(x) lapply(x, function(column) {
        if (is.logical(column) && all(is.na(column))) as.double(column) else column
      })
      expect_equal(compare(west(default$result)), compare(west(expected$result)))
      expect_equal(west(default$warnings), west(expected$warnings))
      # Running the same data under a single named domain isolates the
      # All Sites estimate and its boundary warning.
      one_domain <- global
      one_domain$domain <- factor("Joined")
      combined <- controls(one_domain, greg, TRUE, all_sites = FALSE)
      identifiers <- c("Type", "Subpopulation")
      full <- default$result[default$result$Subpopulation == "All Sites", ]
      rownames(full) <- NULL
      expect_equal(compare(full[setdiff(names(full), identifiers)]),
        compare(combined$result[setdiff(names(full), identifiers)]))
    }
  }
  missing_label <- d
  levels(missing_label$response)[levels(missing_label$response) == "Poor"] <- "Other"
  for (greg in c(FALSE, TRUE)) {
    expect_error(controls(missing_label, greg, FALSE),
      'response_levels for "response" contains undeclared categories:.*Poor')
    expect_error(controls(missing_label, greg, TRUE),
      'response_levels for "response" contains undeclared categories:.*Poor')
  }
}
