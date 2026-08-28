###############################################################################
# Function: sp_balance (exported)
# Programmer: Michael Dumelle
# Date: December 03, 2020
# Last Revised: August 28, 2026
#
#' Calculate spatial balance metrics
#'
#' This function measures the spatial balance (with respect to the
#' sampling frame) of design sites using
#' Voronoi polygons (Dirichlet tessellations). \code{sp_balance()} is
#' generic: for design objects returned by \code{grts()} or \code{irs()}
#' (class \code{sp_design}), \code{sp_balance.sp_design()} return the spatial
#' balance already computed and stored during sampling using the design's
#' true inclusion probabilities (here, \code{sframe} and \code{ip} are not needed).
#' For any other \code{sf} object,
#' \code{sp_balance.default()} computes spatial balance as described below.
#'
#' @param object An object returned by \code{grts()} or \code{irs()}
#'   or and \code{sf} object containing some design sites,.
#'
#' @param ... Additional arguments passed to the method for \code{object}'s
#'   class.
#'
#' @param metrics A character vector of spatial balance metrics:
#' \describe{
#'   \item{\code{pielou}}{ Pielou's Evenness Index (the default). This statistic
#'     can take on a value between zero and one.}
#'   \item{\code{simpsons}}{ Simpsons Evenness Index. This statistic
#'     can take on a value between zero and logarithm of the sample size.}
#'   \item{\code{rmse}}{ Root-Mean-Squared Error. This statistic
#'     can take on a value between zero and infinity.}
#'   \item{\code{mse}}{ Mean-Squared Error. This statistic
#'     can take on a value between zero and infinity.}
#'   \item{\code{mae}}{ Mean-Absolute Error. This statistic
#'     can take on a value between zero and infinity.}
#'   \item{\code{medae}}{ Median-Absolute Error. This statistic
#'     can take on a value between zero and infinity.}
#'   \item{\code{chisq}}{ Chi-Squared Loss. This statistic
#'     can take on a value between zero and infinity.}
#'  }
#'   All spatial balance metrics have a lower bound of zero, which indicates perfect
#'   spatial balance. As the metric value increases, the spatial balance decreases.
#'
#' @param extents Should the extent (total units) within each Voronoi polygon
#'   be returned? Defaults to \code{FALSE}.
#'
#' @return A data frame with columns providing the stratum (\code{stratum}),
#'   spatial balance metric (\code{metric}), and spatial balance (\code{value}).
#'   If \code{extents} is \code{TRUE}, a list with elements \code{metrics}
#'   (the data frame of metrics) and \code{extents} (an \code{sf} object of per-site
#'   Voronoi polygon extents).
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sample <- grts(NE_Lakes, 30)
#' sp_balance(sample)
#' strata_n <- c(low = 25, high = 30)
#' sample_strat <- grts(NE_Lakes, n_base = strata_n, stratum_var = "ELEV_CAT")
#' sp_balance(sample_strat, metrics = "rmse")
#' }
sp_balance <- function(object, ...) {
  UseMethod("sp_balance")
}

#' @rdname sp_balance
#'
#' @param sframe The sampling frame as an \code{sf} object. The coordinate
#'   system for \code{sframe} must be one where distance for coordinates is meaningful.
#'
#' @param stratum_var The name of the stratum variable in \code{object}
#' and \code{sframe}. If \code{NULL} (the default), no strata is assumed.
#' If a single character vector is provided, it is assumed this is the
#' name of the stratum variable in \code{object} and \code{sframe}. If
#' a two-dimensional character vector is provided, one element must be
#' named "object" and corresponds to the name of the stratum variable
#' in \code{object}, while the other element must be named "sframe" and
#' corresponds to the name of the stratum variable in \code{sframe}.
#'
#' @param ip A character string giving the name of the column in
#' \code{sframe} that holds each row's inclusion probability. If not
#' provided, an equal probability design is assumed (within strata).
#'
#' @importFrom stats median
#'
#' @method sp_balance default
#' @export
sp_balance.default <- function(object, sframe, stratum_var = NULL, ip = NULL,
                                metrics = "pielou", extents = FALSE, ...) {
  if (inherits(object, "sp_design")) {
    stop("object must be an sf object, not output from grts() or irs(). Instead 1) use sp_balance() directly on the design object; 2) use object$sites_legacy, object$sites_base, object$sites_over, or object$sites_near; or 3) use sp_rbind().")
  }

  # find system info
  on_solaris <- Sys.info()[["sysname"]] == "SunOS"
  if (on_solaris) {
    stop("sp_balance() is not supported on Solaris.")
  }

  if (is.null(stratum_var)) {
    object$stratum_var <- "None"
    sframe$stratum_var <- "None"
    stratum_var <- "stratum_var"
  }

  if (length(stratum_var) == 1) {
    stratum_var <- c(object = stratum_var, sframe = stratum_var)
  } else if (length(stratum_var) == 2) {
    if (!all(names(stratum_var) %in% c("object", "sframe"))) {
      stop("names of stratum_var must be object and sframe")
    }
  } else {
    stop("length of stratum_var must be zero, one, or two")
  }

  object_levels <- sort(unique(object[[stratum_var["object"]]]))
  sframe_levels <- sort(unique(sframe[[stratum_var["sframe"]]]))

  if (!all.equal(object_levels, sframe_levels)) {
    stop("stratum_var levels for object and sframe must be the same")
  }

  object_split <- split(object, object[[stratum_var["object"]]])
  sframe_split <- split(sframe, sframe[[stratum_var["sframe"]]])

  output <- lapply(object_levels, function(x) calculate_sp_balance(object_split[[x]], sframe_split[[x]], ip, metrics, extents))
  names(output) <- object_levels
  metrics_dfs <- lapply(object_levels, function(x) cbind(stratum = x, data.frame(metric = metrics, value = output[[x]]$values)))
  metrics <- do.call("rbind", metrics_dfs)
  metrics <- metrics[order(metrics$metric), , drop = FALSE]
  row.names(metrics) <- NULL
  if (extents) {
    extent_sf_split <- lapply(names(output), function(x) {
      object_split[[x]]$extent <- output[[x]]$extent
      object_split[[x]][c(stratum_var["object"], "extent")]
    })
    extents <- do.call("rbind", extent_sf_split)
    names(extents)[[which(names(extents) == stratum_var["object"])]] <- "stratum"
    return(list(metrics = metrics, extents = extents))
  } else {
    return(metrics)
  }
}

#' @rdname sp_balance
#'
#' @details For \code{sp_balance.sp_design()}, spatial balance is computed
#' for the legacy and base sites together (the site set \code{n_base} and
#' the design weights correspond to -- see \code{?grts}), using original
#' (not legacy-adjusted) inclusion probabilities.
#'
#' @method sp_balance sp_design
#' @export
sp_balance.sp_design <- function(object, metrics = "pielou", extents = FALSE, ...) {
  bal <- object$sp_balance
  if (is.null(bal)) {
    stop("object has no stored spatial balance. Either sp_balance = FALSE was used when the design was created, or spatial balance could not be computed for this design (see any warning messages from grts()/irs()). Use sp_balance.default() to compute spatial balance manually instead, e.g. sp_balance.default(sp_rbind(object, c('Legacy', 'Base')), sframe).")
  }

  bad_metrics <- setdiff(metrics, spsurvey_balance_metrics)
  if (length(bad_metrics) > 0) {
    stop("an invalid metric was provided")
  }

  out <- bal$metrics[bal$metrics$metric %in% metrics, , drop = FALSE]
  row.names(out) <- NULL

  if (!extents) {
    return(out)
  }

  sites <- sp_rbind(object, c("Legacy", "Base"))
  sites <- sites[order(sites$stratum), , drop = FALSE]
  sites$extent <- unname(bal$extents[sites$siteID])
  extents_sf <- sites[, c("stratum", "extent")]
  row.names(extents_sf) <- NULL

  list(metrics = out, extents = extents_sf)
}

#' All spatial balance metric names, in the order \code{sp_balance()}
#' computes and validates against.
#'
#' @noRd
spsurvey_balance_metrics <- c("pielou", "simpsons", "mse", "rmse", "mae", "medae", "chisq")

#' Row-wise geometric extent (count, length, or area)
#'
#' Returns each row's own extent: \code{1} for \code{POINT}/\code{MULTIPOINT}
#' geometry, length for \code{LINESTRING}/\code{MULTILINESTRING}, and area for
#' \code{POLYGON}/\code{MULTIPOLYGON}.
#'
#' @param x An \code{sf} object with \code{POINT}/\code{MULTIPOINT},
#'   \code{LINESTRING}/\code{MULTILINESTRING}, or \code{POLYGON}/
#'   \code{MULTIPOLYGON} geometry.
#'
#' @return A numeric vector the length of \code{x}.
#'
#' @noRd
sp_balance_dens <- function(x) {
  geom_type <- st_geometry_type(x)
  dens <- rep(1, length(geom_type))
  is_line <- geom_type %in% c("LINESTRING", "MULTILINESTRING")
  is_poly <- geom_type %in% c("POLYGON", "MULTIPOLYGON")
  if (any(is_line)) {
    dens[is_line] <- as.numeric(st_length(x[is_line, ]))
  }
  if (any(is_poly)) {
    dens[is_poly] <- as.numeric(st_area(x[is_poly, ]))
  }
  dens
}

#' Calculate spatial balance metrics for a single stratum
#'
#' This function computes the Voronoi-polygon-based spatial balance metrics
#' (Stevens & Olsen 2004) for one stratum's worth of design
#' sites (\code{object_split}) against the sampling frame (\code{sframe_split}).
#' For each sample site, it builds that site's Voronoi polygon (the region of
#' the frame closer to it than to any other sample site), sums the frame's
#' inclusion probability within that polygon to get the site's total
#' inclusion probability \eqn{v_i} (which has expectation 1 under a
#' perfectly balanced design), and passes the resulting proportions to
#' \code{calculate_metric} for each requested metric.
#'
#' @param object_split The design sites (as an \code{sf} object) for a single
#'   stratum.
#'
#' @param sframe_split The sampling frame (as an \code{sf} object), subset to
#'   the same stratum.
#'
#' @param ip Name of the inclusion probability column in \code{sframe_split},
#'   or \code{NULL} to assume equal probability.
#'
#' @param metrics Character vector of spatial balance metric names to
#'   compute (see \code{sp_balance}).
#'
#' @param extents Logical value; if \code{TRUE}, also return the total frame
#'   extent (count, length, or area) within each Voronoi polygon.
#'
#' @return A list with element \code{values} (a named vector of metric
#'   estimates) and, if \code{extents} is \code{TRUE}, element \code{extent}.
#'
#' @noRd
calculate_sp_balance <- function(object_split, sframe_split, ip, metrics, extents) {
  # need to calculate the density of each row in sframe_split
  sframe_split$dens <- sp_balance_dens(sframe_split)

  # these inclusion probabilities must be provided for the entire sampling frame
  if (is.null(ip)) {
    sframe_split$ip <- nrow(object_split) * (sframe_split$dens / sum(sframe_split$dens))
  } else {
    sframe_split$ip <- sframe_split[[ip]]
  }

  # finding the sframe bounding box, this needs to be reordered to
  # use in the Dirichlet Tesselation
  pop_bbox <- st_bbox(sframe_split)[c("xmin", "xmax", "ymin", "ymax")]

  # working with the object sites

  ## take the object sites coordinates
  samp_coords <- st_coordinates(object_split)
  ## name them X and Y
  colnames(samp_coords) <- c("X", "Y")
  ## isolate X
  samp_xcoord <- samp_coords[, "X"]
  ## isolate Y
  samp_ycoord <- samp_coords[, "Y"]
  ## recover the object size
  n <- nrow(object_split)

  # spatial balance with respect to the sframe
  tiles <- tile.list(deldir(x = samp_xcoord, y = samp_ycoord, rw = pop_bbox))
  ## using lapply instead of a loop
  sftess <- lapply(tiles, get_sftess)
  sftess <- st_sfc(sftess, crs = st_crs(sframe_split))
  sftess <- st_sf(poly = 1:n, geometry = sftess)
  # next part required because st_intersection won't include points if they are on the
  # exact boundary of a polygon (Voronoi polygon has this occur when a site is along the
  # bounding box)
  buffer_dist <- (pop_bbox["xmax"] - pop_bbox["xmin"]) * (pop_bbox["ymax"] - pop_bbox["ymin"]) / 1e12
  sftess <- suppressWarnings(st_intersection(st_make_valid(sframe_split), st_buffer(sftess, buffer_dist)))


  # scaling the inclusion probabilities by the amount of the geometry
  # in the respective tesselation -- polydens can be at most one -- if the
  # row of sframe is contained fully in the respective row of sframe
  # polydens can be less than one -- when the row of sframe is not contained
  # fully in the respective row of sframe
  sftess$polydens <- sp_balance_dens(sftess)
  sftess$adjip <- sftess$ip * sftess$polydens / sftess$dens

  ## summing over each polygon
  propextent <- with(sftess, tapply(adjip, poly, sum))
  ## setting NA's equal to zero
  propextent[is.na(propextent)] <- 0

  # making proportions and expected quantities
  proportions <- propextent / sum(propextent) # which equals n
  expected_proportions <- 1 / n


  # metrics
  values <- vapply(metrics, calculate_metric, double(1), proportions, expected_proportions)

  # returning the results when stored by the user
  output <- list(values = values)
  if (extents) {
    output$extent <- with(sftess, tapply(polydens, poly, sum))
  }
  output
}

#' Convert a deldir tile into an sf polygon
#'
#' \code{deldir::tile.list()} returns each Voronoi tile as a list of x/y
#' vertex coordinate vectors; this function closes the ring (reversing and
#' repeating the first vertex) and wraps it as an \code{sf} \code{POLYGON}.
#'
#' @param tile A single tile element from \code{deldir::tile.list()}, with
#'   components \code{x} and \code{y}.
#'
#' @return An \code{sfg} \code{POLYGON} object for the tile.
#'
#' @noRd
get_sftess <- function(tile) {
  ## finding the number of points in the bounding polygon
  npol <- length(tile$x)

  ## creating and returning the appropriate polygon after binding coords
  st_polygon(list(cbind(
    c(tile$x[1], tile$x[npol:1]),
    c(tile$y[1], tile$y[npol:1])
  )))
}


#' Dispatch to a single named spatial balance metric function
#'
#' @param metric Character value naming one metric (see \code{sp_balance}
#'   for the available choices).
#'
#' @param proportions Vector of each sample site's share of total inclusion
#'   probability within its Voronoi polygon (sums to 1).
#'
#' @param expected_proportions The value each element of \code{proportions}
#'   would take under perfect spatial balance, \code{1 / n}.
#'
#' @return A single named numeric value: the requested metric's estimate.
#'
#' @noRd
calculate_metric <- function(metric, proportions, expected_proportions) {
  switch(metric,
    pielou = calculate_pielou(proportions, expected_proportions),
    simpsons = calculate_simpsons(proportions, expected_proportions),
    rmse = calculate_rmse(proportions, expected_proportions),
    mse = calculate_mse(proportions, expected_proportions),
    mae = calculate_mae(proportions, expected_proportions),
    medae = calculate_medae(proportions, expected_proportions),
    chisq = calculate_chisq(proportions, expected_proportions),
    stop("an invalid metric was provided")
  )
}

#' Pielou's evenness index
#'
#' \eqn{PEI = 1 + \sum_i proportions_i \ln(proportions_i) / \ln(n)}, where
#' \code{n = 1 / expected_proportions}. Bounded in [0, 1]; 0 indicates
#' perfect spatial balance.
#'
#' @param proportions Vector of each sample site's share of total inclusion
#'   probability within its Voronoi polygon (sums to 1).
#'
#' @param expected_proportions The value each element of \code{proportions}
#'   would take under perfect spatial balance, \code{1 / n}.
#'
#' @return A single named numeric value, the Pielou's evenness index estimate.
#'
#' @noRd
calculate_pielou <- function(proportions, expected_proportions) {
  pielou <- 1 + sum(proportions * log(proportions)) / log(1 / expected_proportions) # 1/E(p) = n$
  names(pielou) <- "pielou"
  pielou
}

#' Simpson's evenness index
#'
#' @inheritParams calculate_pielou
#'
#' @return A single named numeric value, the Simpson's evenness index
#'   estimate.
#'
#' @noRd
calculate_simpsons <- function(proportions, expected_proportions) {
  simpsons <- sum(proportions^2) - expected_proportions
  names(simpsons) <- "simpsons"
  simpsons
}

#' Root-mean-squared error of the Voronoi proportions from their expectation
#'
#' @inheritParams calculate_pielou
#'
#' @return A single named numeric value, the RMSE estimate.
#'
#' @noRd
calculate_rmse <- function(proportions, expected_proportions) {
  sqr_dev <- (proportions - expected_proportions)^2
  mse <- sum(sqr_dev) / length(sqr_dev)
  rmse <- sqrt(mse)
  names(rmse) <- "rmse"
  rmse
}

#' Mean-squared error of the Voronoi proportions from their expectation
#'
#' @inheritParams calculate_pielou
#'
#' @return A single named numeric value, the MSE estimate.
#'
#' @noRd
calculate_mse <- function(proportions, expected_proportions) {
  sqr_dev <- (proportions - expected_proportions)^2
  mse <- sum(sqr_dev) / length(sqr_dev)
  names(mse) <- "mse"
  mse
}

#' Mean absolute relative error of the Voronoi proportions from their
#' expectation
#'
#' @inheritParams calculate_pielou
#'
#' @return A single named numeric value, the MAE estimate.
#'
#' @noRd
calculate_mae <- function(proportions, expected_proportions) {
  mae <- mean(abs(proportions - expected_proportions) / expected_proportions)
  names(mae) <- "mae"
  mae
}

#' Median absolute relative error of the Voronoi proportions from their
#' expectation
#'
#' @inheritParams calculate_pielou
#'
#' @return A single named numeric value, the MEDAE estimate.
#'
#' @noRd
calculate_medae <- function(proportions, expected_proportions) {
  medae <- median(abs(proportions - expected_proportions) / expected_proportions)
  names(medae) <- "medae"
  medae
}

#' Chi-squared loss of the Voronoi proportions from their expectation
#'
#' @inheritParams calculate_pielou
#'
#' @return A single named numeric value, the chi-squared loss estimate.
#'
#' @noRd
calculate_chisq <- function(proportions, expected_proportions) {
  chisq <- sum((proportions - expected_proportions)^2 / expected_proportions)
  names(chisq) <- "chisq"
  chisq
}
