skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)
skip_if(identical(Sys.info()[["sysname"]], "SunOS"))

test_that("point capacity includes separately supplied legacy sites", {
  frame <- NE_Lakes[1:6, ]
  frame$unit <- 1:6
  for (sampler in list(grts, irs)) {
    for (sizes in list(c(4, 0), c(6, 0), c(3, 0), c(4, 2), c(3, 1))) {
      set.seed(5)
      result <- sampler(frame[4:6, ], n_base = sizes[1],
        n_over = if (sizes[2] == 0) NULL else sizes[2],
        legacy_sites = frame[1:3, ])
      expect_setequal(result$sites_legacy$unit, 1:3)
      expect_equal(NROW(result$sites_base), sizes[1] - 3)
      expect_equal(NROW(result$sites_over), sizes[2])
      expect_true(all(result$sites_base$unit %in% 4:6))
      expect_length(intersect(result$sites_base$unit, result$sites_over$unit), 0)
      expect_equal(result$sites_legacy$ip, rep(sizes[1] / 6, 3))
      expect_equal(result$sites_legacy$wgt, rep(6 / sizes[1], 3))
    }
    for (sizes in list(c(7, 0), c(4, 3), c(2, 0))) {
      expect_error(suppressMessages(sampler(frame[4:6, ], n_base = sizes[1],
        n_over = if (sizes[2] == 0) NULL else sizes[2], legacy_sites = frame[1:3, ])))
    }
    frame$old <- c(rep("old", 3), rep(NA_character_, 3))
    # Legacy rows already in the frame must not be counted twice.
    expect_error(suppressMessages(sampler(frame, n_base = 7, legacy_var = "old")))
    expect_error(suppressMessages(sampler(frame, n_base = 7)))
  }
})

test_that("point capacity counts legacy sites within their own strata", {
  frame <- NE_Lakes[1:12, ]
  frame$unit <- 1:12
  frame$group <- rep(c("A", "B"), each = 6)
  old <- c(1:3, 7:9)
  legacy <- frame[old, ]
  for (sampler in list(grts, irs)) {
    for (legacy_name in c("group", "legacy_group")) {
      names(legacy)[names(legacy) %in% c("group", "legacy_group")] <- legacy_name
      for (over in list(NULL, c(A = 1, B = 1), list(A = 1, B = 1))) {
        set.seed(5)
        result <- sampler(frame[-old, ], n_base = c(A = 4, B = 4), n_over = over,
          stratum_var = "group", legacy_sites = legacy, legacy_stratum_var = legacy_name)
        expect_setequal(result$sites_legacy$unit, old)
        expect_equal(NROW(result$sites_base), 2)
        expect_equal(NROW(result$sites_over), if (is.null(over)) 0 else 2)
      }
      expect_error(suppressMessages(sampler(frame[-old, ], n_base = c(A = 7, B = 3),
        stratum_var = "group", legacy_sites = legacy, legacy_stratum_var = legacy_name)))
      expect_error(suppressMessages(sampler(frame[-old, ], n_base = c(A = 4, B = 4),
        n_over = list(A = 3, B = 0), stratum_var = "group", legacy_sites = legacy,
        legacy_stratum_var = legacy_name)))
    }
    # Omitting legacy_stratum_var uses the frame's stratum column name.
    set.seed(5)
    result <- sampler(frame[-old, ], n_base = c(A = 4, B = 4),
      stratum_var = "group", legacy_sites = frame[old, ])
    expect_equal(NROW(result$sites_base), 2)
  }
})

test_that("GRTS legacy input methods give the same spatially ordered selection", {
  frame <- NE_Lakes[1:40, ]
  frame$unit <- seq_len(nrow(frame))
  frame$old <- c(rep("old", 4), rep(NA_character_, 36))
  frame$size <- seq(1, 1.5, length.out = 40)
  frame$category <- rep(c("A", "B"), 20)
  settings <- list(list(), list(n_over = 3), list(mindis = 1),
    list(aux_var = "size"), list(caty_var = "category", caty_n = c(A = 5, B = 5)))
  for (setting in settings) {
    set.seed(5)
    separate <- do.call(grts, c(list(sframe = frame[-(1:4), ], n_base = 10,
      legacy_sites = frame[1:4, ]), setting))
    set.seed(5)
    combined <- do.call(grts, c(list(sframe = frame, n_base = 10,
      legacy_var = "old"), setting))
    for (component in c("sites_legacy", "sites_base", "sites_over")) {
      # Compare the selected units in output order, together with their weights.
      for (column in c("unit", "ip", "wgt")) {
        expect_equal(separate[[component]][[column]], combined[[component]][[column]])
      }
    }
  }
})

test_that("GRTS addressing uses legacy geometry for all frame types", {
  point_frame <- NE_Lakes[1:20, ]
  line_frame <- st_sf(geometry = st_sfc(st_linestring(matrix(
    c(0, 0, 100, 100), ncol = 2, byrow = TRUE)), crs = 5070))
  area_frame <- st_sf(geometry = st_sfc(st_polygon(list(matrix(
    c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE))), crs = 5070))
  legacy <- st_sf(geometry = st_sfc(st_point(c(20, 20)), st_point(c(80, 80)), crs = 5070))
  frames <- list(point_frame[-(1:2), ], line_frame, area_frame)
  original_address <- get_address
  for (i in seq_along(frames)) {
    old <- if (i == 1) point_frame[1:2, ] else legacy
    seen <- NULL
    record_address <- function(x, y, rand = TRUE) {
      seen <<- cbind(x, y)
      original_address(x, y, rand)
    }
    set.seed(5)
    result <- with_mocked_bindings(
      grts(frames[[i]], n_base = 6, legacy_sites = old, mindis = 1),
      get_address = record_address, .package = "spsurvey"
    )
    expect_false(is.null(seen))
    expect_true(all(is.finite(seen)))
    expect_equal(unname(seen[1:2, ]), unname(st_coordinates(old)[, c("X", "Y")]))
    expect_equal(NROW(result$sites_legacy), 2)
    expect_equal(NROW(result$sites_base), 4)
  }
})

test_that("IRS permits oversampling when all base sites are legacy", {
  frame <- NE_Lakes[1:20, ]
  frame$unit <- seq_len(nrow(frame))
  frame$old <- c(rep("old", 3), rep(NA_character_, 17))
  legacy <- frame[1:3, ]
  legacy$legacy_note <- "retained"
  for (separate in c(TRUE, FALSE)) {
    args <- if (separate) list(sframe = frame[-(1:3), ], legacy_sites = legacy) else
      list(sframe = frame, legacy_var = "old")
    set.seed(5)
    result <- do.call(irs, c(args, list(n_base = 3, n_over = 2)))
    expect_setequal(result$sites_legacy$unit, 1:3)
    expect_equal(NROW(result$sites_base), 0)
    expect_equal(NROW(result$sites_over), 2)
    expect_true(all(result$sites_over$unit %in% 4:20))
    expect_equal(result$sites_over$ip, rep(3 / 20, 2))
    expect_equal(result$sites_over$wgt, rep(20 / 3, 2))
    # Legacy-only metadata must not leak into the public oversample information.
    expect_false("legacy_note" %in% names(result$sites_over))
  }
})

test_that("missing legacy strata are rejected with an input diagnostic", {
  frame <- NE_Lakes[1:40, ]
  frame$group <- rep(c("A", "B"), each = 20)
  legacy <- frame[1:4, ]
  legacy$group[1] <- NA
  for (sampler in list(grts, irs)) {
    for (column in c("group", "legacy_group")) {
      names(legacy)[names(legacy) %in% c("group", "legacy_group")] <- column
      args <- list(sframe = frame[-(1:4), ], n_base = c(A = 6, B = 5),
        stratum_var = "group", legacy_sites = legacy)
      if (column != "group") args$legacy_stratum_var <- column
      expect_error(suppressMessages(do.call(sampler, args)))
      expect_message(stopprnt(), "Stratum membership must not be missing")
    }
  }
})

test_that("single-point MULTIPOINT legacy features preserve sites and weights", {
  frame <- NE_Lakes[1:20, ]
  frame$unit <- seq_len(nrow(frame))
  legacy <- frame[1:3, ]
  multipoint <- st_cast(legacy, "MULTIPOINT")
  mixed <- rbind(legacy[1, ], multipoint[2:3, ])
  for (sampler in list(grts, irs)) {
    set.seed(5)
    reference <- sampler(frame[-(1:3), ], n_base = 6, n_over = 2, legacy_sites = legacy)
    for (old in list(multipoint, mixed)) {
      set.seed(5)
      result <- sampler(frame[-(1:3), ], n_base = 6, n_over = 2, legacy_sites = old)
      for (component in c("sites_legacy", "sites_base", "sites_over")) {
        expect_identical(result[[component]], reference[[component]])
      }
    }
    # The frame itself can also store one point per MULTIPOINT feature.
    set.seed(5)
    result <- suppressMessages(sampler(st_cast(frame[-(1:3), ], "MULTIPOINT"),
      n_base = 6, n_over = 2, legacy_sites = multipoint))
    for (component in c("sites_legacy", "sites_base", "sites_over")) {
      for (column in c("unit", "ip", "wgt")) {
        expect_equal(result[[component]][[column]], reference[[component]][[column]])
      }
    }
    for (geometry in list(
      st_multipoint(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      st_point(), st_multipoint(matrix(numeric(), ncol = 2)),
      st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
    )) {
      invalid <- legacy
      st_geometry(invalid)[1] <- st_sfc(geometry, crs = st_crs(legacy))
      expect_error(suppressMessages(sampler(frame[-(1:3), ], n_base = 6, legacy_sites = invalid)))
      expect_message(stopprnt(), "exactly one nonempty point per site")
    }
  }
})

test_that("zero oversampling returns the same sites as no oversampling", {
  frame <- NE_Lakes[1:40, ]
  frame$unit <- seq_len(nrow(frame))
  frame$group <- rep(c("A", "B"), each = 20)
  frame$old <- c(rep("old", 3), rep(NA_character_, 37))
  cases <- list(
    list(sframe = frame, n_base = 6),
    list(sframe = frame, n_base = 6, legacy_var = "old", n_near = 1),
    list(sframe = frame[-(1:3), ], n_base = 6, legacy_sites = frame[1:3, ]),
    list(sframe = frame, n_base = c(A = 6, B = 5), stratum_var = "group"),
    list(sframe = frame, n_base = 6, caty_var = "group", caty_n = c(A = 3, B = 3))
  )
  for (sampler in list(grts, irs)) {
    for (args in cases) {
      set.seed(5)
      reference <- do.call(sampler, args)
      oversamples <- if (!is.null(args$stratum_var)) {
        list(0, c(A = 0, B = 0), list(A = 0, B = 0))
      } else if (!is.null(args$caty_var)) {
        list(0, c(A = 0, B = 0))
      } else list(0)
      for (over in oversamples) {
        set.seed(5)
        result <- do.call(sampler, c(args, list(n_over = over)))
        expect_null(result$sites_over)
        for (component in c("sites_legacy", "sites_base", "sites_near", "sp_balance")) {
          expect_identical(result[[component]], reference[[component]])
        }
      }
    }
    # Zero in one stratum must not suppress a positive oversample elsewhere.
    for (over in list(c(A = 0, B = 2), list(A = 0, B = 2))) {
      set.seed(5)
      result <- sampler(frame, n_base = c(A = 6, B = 5), stratum_var = "group", n_over = over)
      expect_equal(NROW(result$sites_over), 2)
      expect_true(all(result$sites_over$stratum == "B"))
      expect_false(anyNA(result$sites_over$siteID))
    }
    expect_error(suppressMessages(sampler(frame, n_base = 6, n_over = -1)))
  }
})
