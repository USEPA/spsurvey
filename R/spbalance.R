###############################################################################
# Function: spbalance (exported)
# Programmer: Michael Dumelle
# Date: December 03, 2020
# Last Revised: December 03, 2020
#
#' Calculate spatial balance metrics
#'
#' This function computes the spatial balance of a design with respect
#' to the sample frame (\code{sframe}) using Dirichlet Tesselations, measuring the
#' extent to which a object is a miniature of \code{sframe}. This function is
#' applicable for unstratified or stratified designs with equal selection
#' probabilities.
#'
#' @param object An \code{sf} object containig the sampled sites.
#'
#' @param sframe An \code{sframe} or \code{sf} object containing the sample frame.
#'
#' @param stratum_var The name of the stratum variable in \code{object}
#' and \code{sframe}. If \code{NULL} (the default), no strata is assumed.
#' If a single character vector is provided, it is assumed this is the
#' name of the stratum variable in \code{object} and \code{sframe}. If
#' a two-dimensional character vector is provided, one element must be
#' named "object" and corresponds to the name of the stratum variable
#' in \code{object}, while the other element must be named "sframe" and
#' correponds to the name of the stratum variable in \code{sframe}.
#'
#' @param ip Inclusion probabilities associated with each row of \code{sframe}.
#' If these are not provided, an equal probability design is assumed (within
#' strata).
#'
#' @param metrics A character vector of spatial balance metrics:
#' \itemize{
#'   \item{pielou - }{pielou evenness index (the default)}
#'   \item{simpsons - }{simpsons evenness index}
#'   \item{chisq - }{chi-squared loss}
#'   \item{abserr - }{absolute error loss}
#'  }
#'
#' @param extents Should the total extent within each dirichlet
#' tesselation be returned? Defaults to FALSE.
#'
#' @return A list having names equal to each strata.
#' @export
#'
#' @examples
#' \dontrun{
#' sample <- grts(NE_Lakes, 30)
#' spbalance(sample, NE_Lakes)
#' }
#' ###############################################################################
spbalance <- function(object, sframe, stratum_var = NULL, ip = NULL, metrics = "pielou", extents = FALSE) {
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

  output <- lapply(object_levels, function(x) calculate_spbalance(object_split[[x]], sframe_split[[x]], ip, metrics, extents))
  names(output) <- object_levels
  metrics_dfs <- lapply(object_levels, function(x) cbind(stratum = x, data.frame(metric = metrics, value = output[[x]]$values)))
  metrics <- do.call("rbind", metrics_dfs)
  metrics <- metrics[order(metrics$metric), , drop = FALSE]
  row.names(metrics) <- NULL
  if (extents) {
    extent_sf_split <- lapply(names(output), function(x) {
      object_split[[x]]$extent <- output[[x]]$extent
      object_split[[x]][c("stratum", "extent")]
    })
    extents <- do.call("rbind", extent_sf_split)
    return(list(metrics = metrics, extents = extents))
  } else {
    return(metrics)
  }
}

calculate_spbalance <- function(object_split, sframe_split, ip, metrics, extents) {
  # need to calculate the density of each row in sframe_split
  if (all(st_geometry_type(sframe_split) %in% c("POINT", "MULTIPOINT"))) {
    sframe_split$dens <- 1
  } else if (all(st_geometry_type(sframe_split) %in% c("LINESTRING", "MULTILINESTRING"))) {
    sframe_split$dens <- as.numeric(st_length(sframe_split))
  } else if (all(st_geometry_type(sframe_split) %in% c("POLYGON", "MULTIPOLYGON"))) {
    sframe_split$dens <- as.numeric(st_area(sframe_split))
  }

  # these inclusion probabilities must be provided for the entire sample frame
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
  sftess <- suppressWarnings(st_intersection(st_make_valid(sframe_split), sftess))


  # scaling the inclusion probabilities by the amount of the geometry
  # in the respective tesselation -- polydens can be at most one -- if the
  # row of sframe is contained fully in the respective row of sframe
  # polydens can be less than one -- when the row of sframe is not contained
  # fully in the respective row of sframe
  # for points, sframe geometry must be point or multipoint
  if (all(st_geometry_type(sframe_split) %in% c("POINT", "MULTIPOINT"))) {
    ## storing a dummy variable to index counts by
    sftess$adjip <- sftess$ip
  } else if (all(st_geometry_type(sframe_split) %in% c("LINESTRING", "MULTILINESTRING"))) {
    sftess$polydens <- as.numeric(st_length(sftess))
    sftess$adjip <- sftess$ip * sftess$polydens / sftess$dens
  } else if (all(st_geometry_type(sframe_split) %in% c("POLYGON", "MULTIPOLYGON"))) {
    sftess$polydens <- as.numeric(st_area(sftess))
    sftess$adjip <- sftess$ip * sftess$polydens / sftess$dens
  }

  ## summing over each polygon
  extent <- with(sftess, tapply(adjip, poly, sum))
  ## setting NA's equal to zero
  extent[is.na(extent)] <- 0

  # making proportions and expected quantities
  proportions <- extent / sum(extent)
  expected_proportions <- 1 / n


  # metrics
  values <- vapply(metrics, calculate_metric, double(1), proportions, expected_proportions)

  # returning the results when stored by the user
  output <- list(values = values)
  if (extents) {
    output$extent <- extent
  }
  output
}

get_sftess <- function(tile) {
  ## finding the number of points in the bounding polygon
  npol <- length(tile$x)

  ## creating and returning the appropriate polygon after binding coords
  st_polygon(list(cbind(
    c(tile$x[1], tile$x[npol:1]),
    c(tile$y[1], tile$y[npol:1])
  )))
}


calculate_metric <- function(metric, proportions, expected_proportions) {
  switch(metric,
    pielou = calculate_pielou(proportions, expected_proportions),
    chisq = calculate_chisq(proportions, expected_proportions),
    abserr = calculate_abserr(proportions, expected_proportions),
    simpsons = calculate_simpsons(proportions, expected_proportions),
    stop("an invalid metric was provided")
  )
}

calculate_pielou <- function(proportions, expected_proportions) {
  pielou <- 1 + sum(proportions * log(proportions)) / log(1 / expected_proportions) # 1/E(p) = n$
  names(pielou) <- "pielou"
  return(pielou)
}

calculate_chisq <- function(proportions, expected_proportions) {
  chisq <- sum((proportions - expected_proportions)^2 / expected_proportions)
  names(chisq) <- "chisq"
  return(chisq)
}

calculate_abserr <- function(proportions, expected_proportions) {
  abserr <- sum(abs(proportions - expected_proportions) / expected_proportions)
  names(abserr) <- "abserr"
  return(abserr)
}

calculate_simpsons <- function(proportions, expected_proportions) {
  simpsons <- sum(proportions^2) - expected_proportions
  names(simpsons) <- "simpsons"
  return(simpsons)
}
