skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
            "set SPSURVEY_RUN_EXTRAS=true to run the extras suite")
skip_if(identical(Sys.info()[["sysname"]], "SunOS"))

mindis_frame <- function(x = c(0, 100, 200, 201)) {
  st_as_sf(data.frame(unit = seq_along(x), size = seq_along(x), x = x, y = 0),
           coords = c("x", "y"), crs = 5070)
}

test_that("both samplers return the updated pivotal draw without a last-retry warning", {
  local_sampling_diagnostic("warn_df")
  for (sampler in list(grts, irs)) {
    calls <- 0L
    pivotal <- function(pik, ...) {
      calls <<- calls + 1L
      pick <- if (calls == 1L) order(pik)[3:4] else order(pik)[c(1, 4)]
      as.numeric(seq_along(pik) %in% pick)
    }
    set.seed(5)
    result <- with_mocked_bindings(
      sampler(mindis_frame(), n_base = 2, aux_var = "size", mindis = 5,
              maxtry = 1, sp_balance = FALSE),
      UPpivotal = pivotal, .package = "spsurvey"
    )
    expect_equal(calls, 2L)
    expect_setequal(result$sites_base$unit, c(1L, 4L))
    expect_null(get("warn_df", envir = .GlobalEnv))
    expect_equal(result$sites_base$ip, result$sites_base$unit / 5)
    expect_equal(result$sites_base$wgt, 5 / result$sites_base$unit)
  }
})

test_that("retry probabilities retain separated sites and preserve the target size", {
  for (sampler in list(grts, irs)) {
    inputs <- list()
    pivotal <- function(pik, ...) {
      inputs[[length(inputs) + 1L]] <<- pik
      if (length(inputs) == 1L) {
        pick <- order(pik)[c(1, 5, 6)]
      } else {
        pick <- c(which(pik == 1), order(pik)[1:2])
      }
      as.numeric(seq_along(pik) %in% pick)
    }
    set.seed(5)
    result <- with_mocked_bindings(
      sampler(mindis_frame(c(0, 100, 200, 300, 400, 401)), n_base = 3,
              aux_var = "size", mindis = 5, maxtry = 2, sp_balance = FALSE),
      UPpivotal = pivotal, .package = "spsurvey"
    )
    expect_length(inputs, 2)
    expect_equal(sort(inputs[[2]]), c(.2, .3, .4, .5, .6, 1))
    expect_equal(sum(inputs[[2]]), 3)
    expect_true(all(inputs[[2]] >= 0 & inputs[[2]] <= 1))
    expect_setequal(result$sites_base$unit, 1:3)
  }
})

test_that("both distance helpers append one correctly labeled failure warning", {
  frame <- mindis_frame()
  frame$xcoord <- st_coordinates(frame)[, 1]
  frame$ycoord <- 0
  frame$idpts <- frame$unit
  frame$ip <- frame$ip_init <- frame$unit / 5
  prior <- data.frame(stratum = "None", func = I("earlier"), warning = "Earlier warning")
  for (name in c("grtspts_mindis", "irspts_mindis")) {
    calls <- 0L
    pivotal <- function(pik, ...) {
      calls <<- calls + 1L
      as.numeric(seq_along(pik) %in% order(pik)[3:4])
    }
    set.seed(5)
    result <- with_mocked_bindings(
      get(name)(5, frame, samplesize = 2, stratum = "None", maxtry = 2,
                legacy_option = FALSE, warn_ind = TRUE, warn_df = prior),
      UPpivotal = pivotal, .package = "spsurvey"
    )
    expect_equal(calls, 3L)
    expect_identical(result$warn_df[1, ], prior)
    expect_equal(nrow(result$warn_df), 2)
    expect_equal(as.character(result$warn_df$func[2]), name)
    expect_match(result$warn_df$warning[2], "not attained after 2")
    expect_setequal(result$sites$unit, 3:4)
  }
})

test_that("minimum distance preserves legacy sites and oversample accounting", {
  frame <- mindis_frame(c(0, 1, 100, 200, 300, 400))
  frame$size <- seq(1, 1.2, length.out = 6)
  frame$old <- c("old", "old", rep(NA_character_, 4))
  for (sampler in list(grts, irs)) {
    for (separate in c(FALSE, TRUE)) {
      args <- if (separate) list(sframe = frame[-(1:2), ], legacy_sites = frame[1:2, ]) else
        list(sframe = frame, legacy_var = "old")
      set.seed(5)
      result <- do.call(sampler, c(args, list(n_base = 4, n_over = 1,
        aux_var = "size", mindis = 5, maxtry = 2, sp_balance = FALSE)))
      expect_setequal(result$sites_legacy$unit, 1:2)
      expect_equal(NROW(result$sites_base), 2)
      expect_equal(NROW(result$sites_over), 1)
      selected <- rbind(result$sites_legacy, result$sites_base, result$sites_over)
      expect_equal(length(unique(selected$unit)), 5)
      distance <- as.matrix(st_distance(selected))
      units(distance) <- NULL
      diag(distance) <- Inf
      expect_true(all(distance[selected$unit > 2, ] >= 5))
      # With no capping, reported probabilities use the base sample size.
      expect_equal(selected$ip, 4 * frame$size[selected$unit] / sum(frame$size))
      expect_equal(selected$wgt, 1 / selected$ip)
    }
  }
})

test_that("distance boundaries and per-stratum limits remain supported", {
  local_sampling_diagnostic("warn_df")
  for (sampler in list(grts, irs)) {
    set.seed(5)
    single <- sampler(mindis_frame(), n_base = 1, mindis = 1000, sp_balance = FALSE)
    expect_equal(NROW(single$sites_base), 1)
    expect_null(get("warn_df", envir = .GlobalEnv))
    set.seed(5)
    census <- suppressMessages(sampler(mindis_frame(), n_base = 4, mindis = 1000,
                                      maxtry = 2, sp_balance = FALSE))
    expect_equal(NROW(census$sites_base), 4)
    expect_equal(nrow(get("warn_df", envir = .GlobalEnv)), 1)
    frame <- mindis_frame(c(0, 100, 1, 101))
    frame$group <- c("A", "A", "B", "B")
    assign("warn_df", NULL, envir = .GlobalEnv)
    set.seed(5)
    strata <- sampler(frame, n_base = c(A = 2, B = 2), stratum_var = "group",
                      mindis = 5, sp_balance = FALSE)
    expect_equal(NROW(strata$sites_base), 4)
    expect_null(get("warn_df", envir = .GlobalEnv))
  }
})

test_that("real pivotal sampling attains a feasible distance after retries", {
  for (sampler in list(grts, irs)) {
    set.seed(5)
    result <- sampler(NE_Lakes, n_base = 30, mindis = 10000, maxtry = 50, sp_balance = FALSE)
    distance <- as.matrix(st_distance(result$sites_base))
    units(distance) <- NULL
    expect_true(all(distance[upper.tri(distance)] >= 10000))
  }
})
