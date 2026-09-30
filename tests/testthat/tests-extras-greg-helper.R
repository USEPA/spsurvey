# Small deterministic fixtures shared by the extras tests.
greg_data <- function() {
  set.seed(5)
  n <- 32L
  data.frame(siteID = paste0("s", seq_len(n)), weight = runif(n, 3, 7),
    xcoord = runif(n), ycoord = runif(n), x = rnorm(n),
    y = rnorm(n) + seq_len(n) / 8,
    region = factor(rep(c("North", "South"), each = n / 2)),
    stratum = factor(rep(c("A", "B"), n / 2)),
    category = factor(rep(c("Good", "Poor"), n / 2)))
}

greg_args <- function(d = greg_data(), vartype = "local") {
  list(dframe = d, vars = "y", subpops = "region", formula = ~x,
    popsize = c("(Intercept)" = 160, x = 25),
    xcoord = "xcoord", ycoord = "ycoord", vartype = vartype,
    statistics = c("Mean", "Total"))
}
