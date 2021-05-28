###############################################################################
# Function: grts_stratum (not exported)
# Programmers: Tony Olsen
# Date: January 22, 2021
#'
#' For a single stratum, select a spatially balanced sample using generalized random
#' tessellation stratified algorithm. For a point sample frame, the selection is a one-step
#' process. For linear and area sample frames, a two-step process is used. First a systematic
#' point sample is selected from either linear or area sample frame to create a dense point
#' sample. Then the grts algorithm is applied to the systematic point sample. The selection
#' in all cases is based on the survey design specification for the stratum.
#'
#' @param stratum Character value for the stratum name.
#'
#' @param dsgn List of components that specify the survey design. Includes all strata.
#'   See grts for contents of dsgn.
#'
#' @param sframe Sample frame as an sf object. If the design is stratified,
#'   unequal probability, proportional probability or has legacy sites, then sample frame
#'   must include variables that identify the stratum; category, auxiliary and legacy variables
#'   for unequal selection; or that identify elements that are legacy sites.
#'   The coordinate system for sframe must be one where distance for coordinates is meaningful.
#'
#' @param sf_type The sample frame geometry type: point, linear or area
#'
#' @param wgt_units The units for weights if different from units in the \code{sf} object. Default
#'   is \code{NULL}. Unit conversion uses \code{set_unit}s in \code{sf} package.
#'
#' @param pt_density For linear and area sample frame, the point density for the systematic
#'   sample. Must be in units of the sframe sf.object. Default is NULL.
#'
#' @param legacy_option Logical variable that when TRUE legacy sites are to be included
#'   in the survey design. Default is FALSE
#'
#' @param legacy_sites An sf object of legacy sites to be included in the survey design.
#'
#' @param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
#'
#' @param warn_ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn_df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return rslts A list consisting of an \code{sf} object for base sites, an \code{sf} object of
#'   \code{n_over} sites (\code{NULL} if none) an \code{sf} object of \code{n_near} sites (\code{NULL} if none) where
#'   the \code{sf} objects containing the sites selected that meet the survey design requirements
#'   and \code{warn_ind} - logical value for warning indicator and \code{warn_df} - a data frame
#'   for warning messages.
#'
#' @section Other functions required:
#'   \describe{
#'     \item{\code{\link{grtspts_ip}}}{Calculates inclusion probabilities}
#'     \item{\code{\link{grtspts_ipleg}}}{Calculates inclusion probabilities with legacy points}
#'     \item{\code{\link{get_address}}}{Creates hierarchical order for points}
#'     \item{\code{\link{UPpivotal}}}{Selects sample using pivotal method}
#'     \item{\code{\link{rho}}}{Orders sites in reverse hierarchical order}
#'     \item{\code{\link{grtspts_mindis}}}{selects a grts sample with minimum distance between
#'       points}
#'     }
#'
#' @author Tony Olsen \email{olsen.tony@@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
#' test_sample <- grts(stratum, dsgn = test_design, sframe = "test_sf", sf_type = "point")
#' }
#'
#' @noRd
###############################################################################

grts_stratum <- function(stratum, dsgn, sframe, sf_type, wgt_units = NULL, pt_density = NULL,
                         legacy_option = FALSE, legacy_sites = NULL, maxtry = 10,
                         warn_ind = FALSE, warn_df = NULL) {

  # Sample sizes required
  n_base <- dsgn[["n_base"]][[stratum]]
  n_over <- sum(dsgn[["n_over"]][[stratum]], na.rm = TRUE)
  if (is.null(n_over)) {
    n_over <- 0
  }
  n_near <- dsgn[["n_near"]][[stratum]]
  if (is.null(n_near)) {
    n_near <- 0
  }
  n_total <- n_base + n_over
  
  # set number of legacy sites to 0
  n_legacy <- 0

  # subset sframe to stratum
  sftmp <- sframe[sframe$stratum == stratum, , drop = FALSE]

  # subset legacy_sites to stratum if present for linear and area option
  if (legacy_option == TRUE & sf_type != "sf_point") {
    legtmp <- legacy_sites[legacy_sites$stratum == stratum, , drop = FALSE]
    n_legacy <- nrow(legtmp)
  }

  # sf_type equals point
  if (sf_type == "sf_point") {
    ip_step1 <- 1
    sftmp$xcoord <- st_coordinates(sftmp)[, "X"]
    sftmp$ycoord <- st_coordinates(sftmp)[, "Y"]
    sftmp$idpts <- 10000 + 1:nrow(sftmp)
  }

  # sf_type equals linear
  if (sf_type == "sf_linear") {
    # determine sample size from pt_density and total length of sample frame in stratum
    stratum_len <- sum(st_length(sftmp))
    if (!is.null(wgt_units)) {
      stratum_len <- set_units(stratum_len, wgt_units, mode = "standard")
    }
    # set default equal to 10 population sites per requested sample site
    if (is.null(pt_density)) {
      popmatch <- 10
      pt_density <- ((n_base + n_over + n_near) * popmatch) / stratum_len
    }
    n_size <- as.integer(pt_density * stratum_len)
    sfpts <- st_sample(sftmp, size = n_size, type = "regular")
    sfpts <- st_as_sf(as.data.frame(sfpts), crs = st_crs(sftmp))
    sfpts <- st_cast(sfpts, to = "POINT")
    # drop features with no points
    sfpts <- sfpts[!st_is_empty(sfpts), ]
    # join sites with linear features
    sfpts <- st_cast(sfpts, to = "POINT")
    sftmp <- st_join(sfpts, sftmp, join = st_nearest_feature)
    names(sftmp)[names(sftmp) == "sfpts"] <- "geometry"
    sftmp$xcoord <- st_coordinates(sftmp)[, "X"]
    sftmp$ycoord <- st_coordinates(sftmp)[, "Y"]
    sftmp$idpts <- 10000 + 1:nrow(sftmp)
    # calculate step 1 inclusion probability based on realized sample size
    ip_step1 <- nrow(sftmp) / stratum_len
  }


  # sf_type equals area
  if (sf_type == "sf_area") {
    # determine sample size from pt_density and total area of sample frame in stratum
    stratum_area <- sum(st_area(sftmp))
    if (!is.null(wgt_units)) {
      stratum_area <- set_units(stratum_area, wgt_units, mode = "standard")
    }
    # set default equal to 10 population sites per requested sample site
    if (is.null(pt_density)) {
      popmatch <- 10
      pt_density <- ((n_base + n_over + n_near) * popmatch) / stratum_area
    }
    n_size <- as.integer(pt_density * stratum_area)
    sfpts <- st_sample(sftmp, size = n_size, type = "hexagonal")
    sfpts <- st_as_sf(as.data.frame(sfpts), crs = st_crs(sftmp))
    sfpts <- st_cast(sfpts, to = "POINT")
    # drop features with no points
    sfpts <- sfpts[!st_is_empty(sfpts), ]
    sftmp <- st_join(sfpts, sftmp)
    sftmp$xcoord <- st_coordinates(sftmp)[, "X"]
    sftmp$ycoord <- st_coordinates(sftmp)[, "Y"]
    sftmp$idpts <- 10000 + 1:nrow(sftmp)
    # calculate step 1 inclusion probability based on realized sample size
    ip_step1 <- nrow(sftmp) / stratum_area
  }

  # Determine number of elements in stratum
  Nstratum <- nrow(sftmp)

  # Determine if legacy sites are to be included for stratum design
  if (legacy_option == TRUE & sf_type != "sf_point") {
    tmp <- legtmp
    addtmp <- setdiff(names(tmp), names(sftmp))
    addleg <- setdiff(names(sftmp), names(tmp))
    sftmp[, addtmp] <- NA
    tmp[, addleg] <- NA
    sftmp <- rbind(tmp, sftmp)
    # Determine number of elements in stratum
    Nstratum <- nrow(sftmp)
  }

  # set legacy that is NA to FALSE
  if (legacy_option == TRUE) {
    sftmp$legacy <- ifelse(is.na(sftmp$legacy), FALSE, TRUE)
    tmp <- subset(sftmp, legacy == TRUE)
    n_legacy <- nrow(tmp)
  }

  # Step 2 site selection if linear or area; otherwise Step 1 for points.
  # determine overall sample size required from dsgn for stratum
  # account for n_over sample option if present
  if (dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    n_caty <- n_total
  } else {
    if (n_over == 0) {
      n_caty <- dsgn[["caty_n"]][[stratum]]
    } else {
      n_caty <- dsgn[["caty_n"]][[stratum]] + dsgn[["n_over"]][[stratum]]
    }
  }

  # If seltype is "equal" or "proportional", set caty to same as stratum
  if (dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    sftmp$caty <- sftmp$stratum
  }

  # compute inclusion probabilities
  ip <- grtspts_ip(
    type = dsgn[["seltype"]][stratum], n_base = n_caty,
    Nstratum = Nstratum, caty = sftmp$caty, aux = sftmp$aux,
    warn_ind = warn_ind, warn_df = warn_df
  )

  # save initial inclusion probabilities
  sftmp$ip_init <- ip$ip
  sftmp$ip <- ip$ip
  # accumulate warning messages if any
  if (ip$warn_ind) {
    warn_ind <- ip$warn_ind
    warn_df <- ip$warn_df
    warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
  }

  # If legacy sites, adjust inclusion probabilities to use
  # legacy inclusion probabilities
  if (legacy_option == TRUE) {
    sftmp$ip <- grtspts_ipleg(sftmp$ip_init, sftmp$legacy == TRUE)
    # accumulate warning messages if any
    if (ip$warn_ind) {
      warn_ind <- ip$warn_ind
      warn_df <- ip$warn_df
      warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
    }
  }

  # select sites if no minimum distance between sites
  if (is.null(dsgn[["mindis"]])) {
    sites <- sftmp[get_address(sftmp$xcoord, sftmp$ycoord, rand = TRUE), ]
    s <- UPpivotal(sites$ip)
    sites <- sites[round(s) == 1, ]
    sites <- rho(sites)
    sites$siteuse <- NA
    sites$replsite <- NA
    sites <- list(sites = sites, warn_ind = warn_ind, warn_df = warn_df)
  }
  # If minimum distance between sites, select sites
  if (!is.null(dsgn[["mindis"]])) {
    sites <- grtspts_mindis(dsgn[["mindis"]], sftmp,
      samplesize = n_total,
      stratum = stratum, maxtry = maxtry, legacy_option = legacy_option,
      legacy_var = dsgn[["legacy_var"]],
      warn_ind = warn_ind, warn_df = warn_df
    )
  }
  # check for warning messages
  warn_ind <- sites$warn_ind
  warn_df <- sites$warn_df
  if (warn_ind) {
    warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
  }

  # adjust inclusion probabilities when over sample sites present
  sites[["sites"]]$ip_init <- sites[["sites"]]$ip_init * n_base / n_total

  # Select replacement sites if n_near not NULL when do not have legacy sites
  if (legacy_option == FALSE) {
    if (!is.null(dsgn[["n_near"]][[stratum]])) {
      sites_near <- replace_near(dsgn[["n_near"]][[stratum]],
        sites = sites[["sites"]],
        sframe = sftmp
      )

      # Adjust inclusion probabilities for replacement sites if over sample sites present
      if (n_over != 0) {
        sites_near$ip_init <- sites_near$ip_init * n_base / n_total
      }
    }
  }

  # Select replacement sites if n_near not NULL when have legacy sites
  if (legacy_option == TRUE) {
    if (!is.null(dsgn[["n_near"]][[stratum]])) {
      keep <- sites[["sites"]][sites[["sites"]]$legacy == FALSE, "idpts", drop = TRUE]
      sites_near <- replace_near(dsgn[["n_near"]][[stratum]],
        sites = sites[["sites"]][sites[["sites"]]$legacy == FALSE, ],
        sframe = subset(sftmp, !(sftmp$idpts %in% keep))
      )

      # Adjust inclusion probabilities for replacement sites if over sample sites present
      if (n_over != 0) {
        sites_near$ip_init <- sites_near$ip_init * n_base / n_total
      }
    }
  }

  # Assign original inclusion probabilities to sites, create weights and drop legacy ip variable
  sites[["sites"]]$ip <- sites[["sites"]]$ip_init * ip_step1
  sites[["sites"]]$wgt <- 1 / sites[["sites"]]$ip
  tmp <- names(sites[["sites"]])
  sites[["sites"]] <- subset(sites[["sites"]],
    select = tmp[!(tmp %in% c("ip_init", "geometry"))]
  )

  # Do same for sites_near if any
  if (is.null(dsgn[["n_near"]][[stratum]])) {
    sites_near <- NULL
  }
  if (!is.null(dsgn[["n_near"]][[stratum]])) {
    sites_near$ip <- sites_near$ip_init * ip_step1
    sites_near$wgt <- 1 / sites_near$ip
    tmp <- names(sites_near)
    sites_near <- subset(sites_near, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  }

  # Split sites to have separate sites_base, sites_legacy and sites_over
  # save legacy sites if any and reduce sites_base to non legacy sites
  sites_legacy <- NULL
  if (legacy_option == TRUE) {
    sites_legacy <- sites[["sites"]][sites[["sites"]]$legacy == TRUE, ]
    sites[["sites"]] <- sites[["sites"]][sites[["sites"]]$legacy == FALSE, ]
    n_legacy <- nrow(sites_legacy)
  }
  
  # warning if n_base <= n_legacy
  if (n_base <= n_legacy) {
    warn_ind <- TRUE
    warn <- paste0("Number of base sites ", n_base, " is less than or equal to ",
                   n_legacy, " the number of legacy sites for stratum ", stratum)
    warn_df <- rbind(warn_df, data.frame(stratum = stratum, func = I("grts_stratum"),
                                         warning = warn))
  }

  # save base sites
  sites_base <- NULL
  if (n_base > n_legacy) {
    sites_base <- sites[["sites"]][1:(n_base - n_legacy), ]
  }

  # save n_over sample sites if any
  sites_over <- NULL
  if (n_over != 0) {
    sites_over <- sites[["sites"]][(n_base - n_legacy + 1):(n_total - n_legacy), ]
    sites_over$siteuse <- "Over"
  }

  # create list for output and return result
  rslts <- list(
    sites_base = sites_base, sites_legacy = sites_legacy,
    sites_over = sites_over, sites_near = sites_near,
    warn_ind = warn_ind, warn_df = warn_df
  )

  invisible(rslts)
}
