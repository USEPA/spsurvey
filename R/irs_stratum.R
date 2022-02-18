###############################################################################
# Function: irs_stratum (not exported)
# Programmers: Tony Olsen
# Date: January 22, 2021
#'
#' For a single stratum, select an independent random sample. For a point sample frame,
#' the selection is a one-step process. For linear and area sample frames, a two-step
#' process is used. First a systematic point sample is selected from either linear
#' or area sample frame to create a dense point sample. Then the irs algorithm is
#' applied to the systematic point sample. The selection in all cases is based on
#' the survey design specification for the stratum.
#'
#' @inheritParams grts_stratum
#'
#' @return rslts A list consisting of an \code{sf} object for base sites, an \code{sf} object of
#'   \code{n_over} sites (\code{NULL} if none) an \code{sf} object of \code{n_near} sites (\code{NULL} if none) where
#'   the \code{sf} objects containing the sites selected that meet the survey design requirements
#'   and \code{warn_ind} - logical value for warning indicator and \code{warn_df} - a data frame
#'   for warning messages.
#'
#' @author Tony Olsen email{olsen.tony@@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
#' test.sample <- irs(stratum, dsgn = test_design, sframe = "test_sf", sf_type = "point")
#' }
#'
#' @noRd
###############################################################################

irs_stratum <- function(stratum, dsgn, sframe, sf_type, wgt_units = NULL, pt_density = NULL,
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

  # find legacy site number for points if legacy_var provided
  if (legacy_option == TRUE & is.null(legacy_sites)) {
    n_legacy <- sum(!is.na(sftmp$legacy))
  }

  # subset legacy_sites to stratum if present for linear and area option
  if (legacy_option == TRUE & (sf_type != "sf_point" | ((sf_type == "sf_point") & !is.null(legacy_sites)))) {
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
      pt_density <- 10
    }
    n_size <- as.integer(ceiling(pmin(1e9, pt_density * (n_base + n_over))))
    sfpts <- st_sample(sftmp, size = n_size, type = "regular", exact = TRUE)
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
      pt_density <- 10
    }
    n_size <- as.integer(ceiling(pmin(1e9, pt_density * (n_base + n_over))))
    sfpts <- st_sample(sftmp, size = n_size, type = "hexagonal", exact = TRUE)
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
  if (legacy_option == TRUE & n_legacy > 0 & (sf_type != "sf_point" | ((sf_type == "sf_point") & !is.null(legacy_sites)))) {
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
  if (legacy_option == TRUE & n_legacy > 0) {
    sftmp$legacy <- ifelse(is.na(sftmp$legacy), FALSE, TRUE)
    tmp <- sftmp[sftmp$legacy == TRUE, , drop = FALSE]
    n_legacy <- nrow(tmp)
  }
  
  if (legacy_option == TRUE & n_legacy == 0) {
    sftmp$legacy <- FALSE
  }

  # check that number of legacy sites is less than or equal number of base sites
  # stop if not
  if (n_legacy > n_base) {
    cat("Number of legacy sites is greater than number of base sites in at least one\n")
    cat("stratum. Please check that all strata have fewer legacy sites than base sites.\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
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
      base_prop <- dsgn[["caty_n"]][[stratum]] / sum(dsgn[["caty_n"]][[stratum]])
      n_caty <- dsgn[["caty_n"]][[stratum]] + dsgn[["n_over"]][[stratum]] * base_prop
    }
  }

  # If seltype is "equal" or "proportional", set caty to same as stratum
  if (dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    sftmp$caty <- "None"
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
  if (legacy_option == TRUE & n_legacy > 0) {
    sftmp$ip <- grtspts_ipleg(sftmp$ip_init, sftmp$legacy == TRUE)
    # accumulate warning messages if any
    if (ip$warn_ind) {
      warn_ind <- ip$warn_ind
      warn_df <- ip$warn_df
      warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
    }
  }

  # select sites if no minimum distance between sites
  if (is.null(dsgn[["mindis"]][[stratum]])) {
    if (nrow(sftmp) <= n_total) {
      samp.id <- sftmp$idpts
    } else {
      # randomly shuffle site order
      sftmp <- sftmp[sample(1:nrow(sftmp)), ]
      s <- UPpivotal(sftmp$ip)
      samp.id <- sftmp$idpts[round(s) == 1]
    }
    # extract sites from sample frame
    sites <- sftmp[sftmp$idpts %in% samp.id, ]
    sites$siteuse <- NA
    sites$replsite <- NA
    sites <- list(sites = sites, warn_ind = warn_ind, warn_df = warn_df)
  }
  # If minimum distance between sites, select sites
  if (!is.null(dsgn[["mindis"]][[stratum]])) {
    sites <- irspts_mindis(dsgn[["mindis"]][[stratum]], sftmp,
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
  if (legacy_option == FALSE | n_legacy == 0) {
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
  if (legacy_option == TRUE & n_legacy > 0) {
    if (!is.null(dsgn[["n_near"]][[stratum]])) {
      keep <- sites[["sites"]][sites[["sites"]]$legacy %in% c(TRUE, FALSE), "idpts", drop = TRUE]
      sites_near <- replace_near(dsgn[["n_near"]][[stratum]],
        sites = sites[["sites"]][sites[["sites"]]$legacy %in% c(TRUE, FALSE), ],
        sframe = subset(sftmp, !(sftmp$idpts %in% keep))
      )

      # Adjust inclusion probabilities for replacement sites if over sample sites present
      if (n_over != 0) {
        sites_near$ip_init <- sites_near$ip_init * n_base / n_total
      }
    }
  }


  # Assign original inclusion probabilites to sites, create weights and drop legacy ip variable
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
    sites_near[, setdiff(names(legacy_sites), names(sites_near))] <- NA
  }

  # Split sites to have separate sites_base, sites_legacy and sites_over
  # save legacy sites if any and reduce sites_base to non legacy sites
  sites_legacy <- NULL
  if (legacy_option == TRUE & n_legacy > 0) {
    sites_legacy <- sites[["sites"]][sites[["sites"]]$legacy == TRUE, ]
    sites[["sites"]] <- sites[["sites"]][sites[["sites"]]$legacy == FALSE, ]
    n_legacy <- nrow(sites_legacy)
  }

  # save base sites
  sites_base <- NULL
  if (n_base > n_legacy) {
    sites_base <- sites[["sites"]][1:(n_base - n_legacy), ]
    sites_base[, setdiff(names(legacy_sites), names(sites_base))] <- NA
  }

  # save n_over sample sites if any
  sites_over <- NULL
  if (n_over != 0) {
    sites_over <- sites[["sites"]][(n_base - n_legacy + 1):(n_total - n_legacy), ]
    sites_over$siteuse <- "Over"
    sites_base[, setdiff(names(legacy_sites), names(sites_base))] <- NA
  }

  # if no legacy sites match in strata then put in appropriate column
  if (legacy_option == TRUE & n_legacy == 0) {
    sites_base$legacy <- FALSE
  }

  # create list for output and return result
  rslts <- list(
    sites_legacy = sites_legacy, sites_base = sites_base,
    sites_over = sites_over, sites_near = sites_near,
    warn_ind = warn_ind, warn_df = warn_df
  )

  invisible(rslts)
}
