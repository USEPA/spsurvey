skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
            "set SPSURVEY_RUN_EXTRAS=true to run the extras suite")
skip_if(identical(Sys.info()[["sysname"]], "SunOS"))

sampling_aux_frame <- function() {
  frame <- NE_Lakes[1:20, ]
  frame$unit <- seq_len(nrow(frame))
  frame$size <- seq_len(nrow(frame))
  frame
}

test_that("invalid auxiliary values are rejected at the input boundary", {
  local_sampling_diagnostic("stop_df")
  invalid <- list(0, -1, NA_real_, NaN, Inf, -Inf, "bad", factor("bad"), 1 + 1i)
  for (sampler in list(grts, irs)) {
    for (bad in invalid) {
      frame <- sampling_aux_frame()
      frame$size <- rep(bad, nrow(frame))
      assign("stop_df", NULL, envir = .GlobalEnv)
      expect_error(suppressMessages(sampler(frame, n_base = 6, aux_var = "size")))
      diagnostics <- get("stop_df", envir = .GlobalEnv)
      expect_match(paste(diagnostics[[2]], collapse = " "), "sframe.*numeric|sframe.*finite")
      for (explicit in c(FALSE, TRUE)) {
        frame <- sampling_aux_frame()
        legacy <- frame[1:2, ]
        column <- if (explicit) "old_size" else "size"
        if (explicit) legacy$size <- NULL
        legacy[[column]] <- rep(bad, 2)
        assign("stop_df", NULL, envir = .GlobalEnv)
        expect_error(suppressMessages(sampler(frame[-(1:2), ], n_base = 6,
          aux_var = "size", legacy_sites = legacy,
          legacy_aux_var = if (explicit) column else NULL)))
        diagnostics <- get("stop_df", envir = .GlobalEnv)
        expect_true("legacy_aux_var" %in% diagnostics[[1]])
        expect_match(paste(diagnostics[[2]], collapse = " "), "legacy_sites.*numeric|legacy_sites.*finite")
      }
    }
  }
})

test_that("only the effective auxiliary column is required and diagnostics accumulate", {
  local_sampling_diagnostic("stop_df")
  frame <- sampling_aux_frame()
  legacy <- frame[1:2, ]
  legacy$old_size <- legacy$size
  legacy$size <- NULL
  for (sampler in list(grts, irs)) {
    set.seed(5)
    expect_s3_class(sampler(frame[-(1:2), ], n_base = 6, aux_var = "size",
      legacy_sites = legacy, legacy_aux_var = "old_size"), "sp_design")
    set.seed(5)
    expect_s3_class(sampler(frame[-(1:2), ], n_base = 6,
      legacy_sites = legacy, legacy_aux_var = "absent"), "sp_design")
    assign("stop_df", NULL, envir = .GlobalEnv)
    expect_error(suppressMessages(sampler(frame[-(1:2), ], n_base = 6,
      aux_var = "absent", legacy_sites = legacy, legacy_aux_var = "also_absent")))
    diagnostics <- get("stop_df", envir = .GlobalEnv)
    expect_equal(nrow(diagnostics), 2)
    expect_setequal(as.character(diagnostics[[1]]), c("aux_var", "legacy_aux_var"))
  }
})

test_that("auxiliary units do not change selections or reported probabilities", {
  for (sampler in list(grts, irs)) {
    for (column in c("size", "aux")) {
      frame <- sampling_aux_frame()
      frame[[column]] <- frame$size
      set.seed(5)
      reference <- sampler(frame, n_base = 6, n_over = 2, n_near = 1, aux_var = column)
      frame[[column]] <- units::set_units(frame[[column]], "ha", mode = "standard")
      set.seed(5)
      result <- sampler(frame, n_base = 6, n_over = 2, n_near = 1, aux_var = column)
      original_name <- if (column == "aux") "sframe_aux" else column
      for (component in c("sites_base", "sites_over", "sites_near")) {
        expect_identical(result[[component]]$unit, reference[[component]]$unit)
        expect_identical(result[[component]]$ip, reference[[component]]$ip)
        expect_identical(result[[component]]$wgt, reference[[component]]$wgt)
        expect_identical(result[[component]][[original_name]], frame[[column]][result[[component]]$unit])
        expect_false(inherits(result[[component]]$aux, "units"))
      }
    }
  }
})

test_that("legacy units preserve sampling and the numeric output column set", {
  for (sampler in list(grts, irs)) {
    for (frame_column in c("size", "aux")) {
      for (column in c("size", "old_size", "aux")) {
        frame <- sampling_aux_frame()
        frame[[frame_column]] <- frame$size
        legacy <- frame[1:2, ]
        legacy[[column]] <- legacy$size
        set.seed(5)
        reference <- sampler(frame[-(1:2), ], n_base = 6, n_over = 2,
          aux_var = frame_column, legacy_sites = legacy, legacy_aux_var = column)
        frame[[frame_column]] <- units::set_units(frame[[frame_column]], "ha", mode = "standard")
        legacy[[column]] <- units::set_units(legacy[[column]] * 10000, "m^2", mode = "standard")
        set.seed(5)
        result <- sampler(frame[-(1:2), ], n_base = 6, n_over = 2,
          aux_var = frame_column, legacy_sites = legacy, legacy_aux_var = column)
        for (component in c("sites_legacy", "sites_base", "sites_over")) {
          expect_identical(names(result[[component]]), names(reference[[component]]))
          expect_identical(result[[component]]$unit, reference[[component]]$unit)
          expect_equal(result[[component]]$ip, reference[[component]]$ip, tolerance = 1e-14)
          expect_equal(result[[component]]$wgt, reference[[component]]$wgt, tolerance = 1e-14)
        }
        out_name <- if (column == "aux") "sframe_aux" else column
        if (out_name %in% names(reference$sites_legacy)) {
          expect_identical(result$sites_legacy[[out_name]], legacy[[column]][result$sites_legacy$unit])
        } else {
          expect_false(out_name %in% names(result$sites_legacy))
        }
        frame_name <- if (frame_column == "aux") "sframe_aux" else frame_column
        expect_identical(result$sites_base[[frame_name]],
                         frame[[frame_column]][result$sites_base$unit])
      }
    }
  }
})

test_that("incompatible, mixed, and invalid units produce input diagnostics", {
  local_sampling_diagnostic("stop_df")
  for (sampler in list(grts, irs)) {
    frame <- sampling_aux_frame()
    legacy <- frame[1:2, ]
    frame$size <- units::set_units(frame$size, "ha", mode = "standard")
    for (kind in c("incompatible", "mixed", "zero", "negative", "missing", "infinite")) {
      legacy$size <- switch(kind,
        incompatible = units::set_units(1:2, "m", mode = "standard"),
        mixed = 1:2,
        zero = units::set_units(c(0, 1), "ha", mode = "standard"),
        negative = units::set_units(c(-1, 1), "ha", mode = "standard"),
        missing = units::set_units(c(NA_real_, 1), "ha", mode = "standard"),
        infinite = units::set_units(c(Inf, 1), "ha", mode = "standard"))
      assign("stop_df", NULL, envir = .GlobalEnv)
      expect_error(suppressMessages(sampler(frame[-(1:2), ], n_base = 6,
        aux_var = "size", legacy_sites = legacy)))
      expect_true("legacy_aux_var" %in% get("stop_df", envir = .GlobalEnv)[[1]])
    }
  }
})

test_that("converted legacy values must remain finite and positive", {
  local_sampling_diagnostic("stop_df")
  for (sampler in list(grts, irs)) {
    for (conversion in list(c("m", "km", "1e308"), c("degC", "K", "1"))) {
      frame <- sampling_aux_frame()
      legacy <- frame[1:2, ]
      frame$size <- units::set_units(frame$size, conversion[1], mode = "standard")
      legacy$size <- units::set_units(rep(as.numeric(conversion[3]), 2),
                                     conversion[2], mode = "standard")
      assign("stop_df", NULL, envir = .GlobalEnv)
      expect_error(suppressMessages(sampler(frame[-(1:2), ], n_base = 6,
        aux_var = "size", legacy_sites = legacy)))
      expect_match(paste(get("stop_df", envir = .GlobalEnv)[[2]], collapse = " "),
                   "after conversion")
    }
    frame <- sampling_aux_frame()
    legacy <- frame[1:2, ]
    legacy$size <- units::set_units(legacy$size, "ha", mode = "standard")
    expect_error(suppressMessages(sampler(frame[-(1:2), ], n_base = 6,
      aux_var = "size", legacy_sites = legacy)))
    expect_match(paste(get("stop_df", envir = .GlobalEnv)[[2]], collapse = " "),
                 "both have units or both have no units")
  }
})

test_that("units work for stratified in-frame legacy and continuous resources", {
  point <- sampling_aux_frame()
  point$group <- rep(c("A", "B"), each = 10)
  point$old <- c("old", rep(NA_character_, 9), "old", rep(NA_character_, 9))
  line <- st_sf(unit = 1:2, size = 1:2, geometry = st_sfc(
    st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
    st_linestring(matrix(c(0, 100, 100, 100), ncol = 2, byrow = TRUE)), crs = 5070))
  area <- st_sf(unit = 1:2, size = 1:2, geometry = st_sfc(
    st_polygon(list(matrix(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE))),
    st_polygon(list(matrix(c(200, 0, 300, 0, 300, 100, 200, 100, 200, 0), ncol = 2, byrow = TRUE))),
    crs = 5070))
  legacy <- st_as_sf(data.frame(unit = 3L, size = 1.5, x = 50, y = 50),
                    coords = c("x", "y"), crs = 5070)
  legacy_named <- legacy
  names(legacy_named)[names(legacy_named) == "size"] <- "old_size"
  legacy_reserved <- legacy
  names(legacy_reserved)[names(legacy_reserved) == "size"] <- "aux"
  cases <- list(
    list(sframe = point, n_base = c(A = 4, B = 4), stratum_var = "group", legacy_var = "old"),
    list(sframe = line, n_base = 6, legacy_sites = legacy),
    list(sframe = area, n_base = 6, legacy_sites = legacy),
    list(sframe = line, n_base = 6, legacy_sites = legacy_named, legacy_aux_var = "old_size"),
    list(sframe = area, n_base = 6, legacy_sites = legacy_reserved, legacy_aux_var = "aux")
  )
  for (sampler in list(grts, irs)) {
    for (args in cases) {
      args$aux_var <- "size"
      set.seed(5)
      reference <- do.call(sampler, args)
      args$sframe$size <- units::set_units(args$sframe$size, "ha", mode = "standard")
      legacy_column <- if (is.null(args$legacy_aux_var)) "size" else args$legacy_aux_var
      if (!is.null(args$legacy_sites)) {
        args$legacy_sites[[legacy_column]] <- units::set_units(
          args$legacy_sites[[legacy_column]] * 10000, "m^2", mode = "standard")
      }
      set.seed(5)
      result <- do.call(sampler, args)
      for (component in c("sites_base", "sites_legacy")) {
        expect_identical(names(result[[component]]), names(reference[[component]]))
        expect_identical(result[[component]]$unit, reference[[component]]$unit)
        expect_equal(result[[component]]$ip, reference[[component]]$ip)
        expect_equal(result[[component]]$wgt, reference[[component]]$wgt)
        out_name <- "size"
        if (component == "sites_legacy" && !is.null(args$legacy_sites)) {
          expected <- args$legacy_sites[[legacy_column]]
          out_name <- if (legacy_column == "aux") "legacy_sites_aux" else legacy_column
        } else {
          expected <- args$sframe$size[result[[component]]$unit]
        }
        expect_identical(result[[component]][[out_name]], expected)
        expect_false(inherits(result[[component]]$aux, "units"))
      }
      expect_equal(result$sp_balance, reference$sp_balance)
    }
  }
})

test_that("legacy probability adjustment preserves certainty and total sample size", {
  expect_equal(grtspts_ipleg(c(.2, .4, .6, .8, 1), c(TRUE, FALSE, FALSE, FALSE, FALSE)),
               c(1, 2 / 9, 1 / 3, 4 / 9, 1))
  expect_equal(grtspts_ipleg(rep(.5, 4), c(TRUE, TRUE, FALSE, FALSE)), c(1, 1, 0, 0))
  expect_equal(grtspts_ipleg(rep(1, 3), rep(TRUE, 3)), rep(1, 3))
})
