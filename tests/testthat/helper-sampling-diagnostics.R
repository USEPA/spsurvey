local_sampling_diagnostic <- function(name, env = parent.frame()) {
  present <- exists(name, envir = .GlobalEnv, inherits = FALSE)
  previous <- if (present) get(name, envir = .GlobalEnv) else NULL
  withr::defer({
    if (present) {
      assign(name, previous, envir = .GlobalEnv)
    } else if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = name, envir = .GlobalEnv)
    }
  }, envir = env)
  assign(name, NULL, envir = .GlobalEnv)
}
