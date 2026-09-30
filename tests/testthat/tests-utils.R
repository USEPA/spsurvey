# Exercise plotting without writing Rplots.pdf. Keep the fileless device open
# for the whole suite, then close it and restore the caller's device option.
local_test_graphics <- function() {
  scope <- testthat::teardown_env()
  withr::local_options(
    list(device = function(...) grDevices::pdf(file = NULL, ...)),
    .local_envir = scope
  )
  grDevices::pdf(file = NULL)
  device <- grDevices::dev.cur()
  withr::defer({
    if (device %in% grDevices::dev.list()) grDevices::dev.off(device)
  }, envir = scope)
}
