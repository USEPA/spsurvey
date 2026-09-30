# Tests that create plots source this file in their own environment and do not store Rplots.pdf.
withr::local_options(list(device = function(...) grDevices::pdf(file = NULL, ...)),
  .local_envir = testthat::teardown_env())
withr::local_pdf(NULL, .local_envir = testthat::teardown_env())
