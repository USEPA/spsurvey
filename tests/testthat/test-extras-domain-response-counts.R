skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

test_that("subpopulation response counts do not depend on neighborhood subsetting", {
  set.seed(31)
  n <- 120
  d <- data.frame(siteID = paste0("s", seq_len(n)), weight = runif(n, 5, 20),
    xcoord = runif(n), ycoord = runif(n))
  d$value <- d$xcoord + rnorm(n)
  d$value[sample(n, 8)] <- NA
  d$region <- factor(ifelse(d$xcoord < .35, "West", "East"))
  d$stratum <- factor(ifelse(d$ycoord < .5, "S1", "S2"))
  d$response <- factor(ifelse(d$xcoord + rnorm(n) > .5, "Poor", "Good"))
  d$response[sample(n, 5)] <- NA
  d$stressor <- factor(ifelse(d$ycoord + rnorm(n) > .5, "Poor", "Good"))
  panel <- rbind(transform(d, survey = "one"),
    transform(d, survey = "two", value = value + rnorm(n, .2, .3)))
  panel <- panel[-sample(which(panel$survey == "two"), 10), ]
  base <- list(siteID = "siteID", weight = "weight", xcoord = "xcoord",
    ycoord = "ycoord", subpops = "region")
  counts <- function(x) {
    if (is.data.frame(x)) {
      cols <- grep("^nResp", names(x), value = TRUE)
      if (length(cols)) {
        return(list(x[, c(intersect(c("Type", "Subpopulation"), names(x)), cols), drop = FALSE]))
      }
      return(list())
    }
    if (is.list(x)) return(unlist(lapply(x, counts), recursive = FALSE))
    list()
  }
  cases <- list(
    change_analysis = list(dframe = panel, vars_cont = "value", vars_cat = "response",
      test = c("mean", "total", "median"), surveyID = "survey"),
    cont_analysis = list(dframe = d, vars = "value"),
    cat_analysis = list(dframe = d, vars = "response"),
    attrisk_analysis = list(dframe = d, vars_response = "response", vars_stressor = "stressor"))
  for (strat in c(FALSE, TRUE)) for (fun in names(cases)) {
    a <- c(base, cases[[fun]])
    if (strat) a$stratumID <- "stratum"
    run <- function(extra = list()) {
      suppressWarnings(suppressMessages(do.call(fun, c(a, extra))))
    }
    full <- run(list(subset_local = FALSE))
    subset <- run(list(subset_local = TRUE))
    expect_gt(length(counts(full)), 0)
    expect_identical(counts(full), counts(subset), info = paste(fun, strat))
    if (fun == "change_analysis") {
      # Check defaults against the raw data.
      default <- run()
      expect_identical(counts(default), counts(full))
      for (stat in c("contsum_mean", "contsum_total")) {
        actual <- default[[stat]]
        expect_equal(nrow(actual), nlevels(d$region))
        for (s in seq_along(c("one", "two"))) {
          occasion <- c("one", "two")[s]
          expected <- vapply(as.character(actual$Subpopulation), function(domain) {
            sum(panel$region == domain & panel$survey == occasion & !is.na(panel$value))
          }, integer(1))
          expect_equal(actual[[paste0("nResp_", s)]], unname(expected),
            info = paste(stat, strat, occasion))
        }
      }
    }
  }
})
