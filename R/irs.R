###############################################################################
# Function: irs (exported)
# Programmers: Tony Olsen, Tom Kincaid
# Date: January 22, 2022
#' Select an Independent random sample (IRS)
#'
#' Select an independent random sample from a point, linear, or areal frame based on
#' a survey design specification. The survey design may be stratified and within each stratum
#' may be equal probability, unequal probability by categories or unequal probabilty
#' proportional to an auxillary variable. Additional options include selecting additional
#' sites as an "over" sample, selecting nearby replacement sites, including legacy sites,
#' and requiring minimum distance between sites.
#'
#' @inheritParams grts
#'
#' @return sites A list of three \code{sf} objects containing the base sites (\code{sites_base}),
#'   the \code{n_over} sites (\code{sites_over}) and the \code{n_near} sites (\code{sites_near}) selected
#'   that meet the survey design requirementse plus a design list object that documents
#'   the survey design used.
#'
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @keywords survey design
#'
#' @examples
#' \dontrun{
#' test.sample <- grts(sframe = "test_sf", n_base = 100)
#' }
#'
#' @export
###############################################################################

irs <- function(sframe, n_base, seltype = "equal", wgt_units = NULL,
                pt_density = NULL, caty_n = NULL, n_over = NULL, n_near = NULL,
                stratum_var = NULL, caty_var = NULL, aux_var = NULL,
                legacy_sites = NULL, legacy_stratum_var = NULL, legacy_var = NULL,
                mindis = NULL, DesignID = "Site", SiteBegin = 1, maxtry = 10) {

  if (inherits(sframe, c("tbl_df", "tbl"))) { # identify if tibble class elements are present
    class(sframe) <- setdiff(class(sframe), c("tbl_df", "tbl"))
    # remove tibble class for rownames warning
  }

  # Create warning indicator and data frame to collect all potential issues during
  # sample selection
  warn_ind <- FALSE
  warn_df <- data.frame(stratum = "Stratum", func = "Calling Function", warn = "Message")

  # Ensure that the geometry types for sframe are consistent
  temp <- st_geometry_type(sframe)
  tst <- all(temp %in% c("POINT", "MULTIPOINT")) |
    all(temp %in% c("LINESTRING", "MULTILINESTRING")) |
    all(temp %in% c("POLYGON", "MULTIPOLYGON"))
  if (!tst) {
    stop(paste("\nThe geometry types for the survey frame object passed to function irs: \n\"",
               unique(st_geometry_type(sframe)), "\" are not consistent.",
               sep = ""
    ))
  }

  # Drop m and z values to ensure no issues with grts functionality with sf object
  if (!is.null(st_m_range(sframe)) & !is.null(st_z_range(sframe))) {
    warn_ind <- TRUE
    warn_df$warn <- "\nThe survey frame object passed to function grts contains m or z values - they are being dropped to ensure functionality in grts."
    sframe <- st_zm(sframe)
  }

  # Determine type of sample frame: point, line, polygon
  if (all(temp %in% c("POINT", "MULTIPOINT"))) sf_type <- "sf_point"
  if (all(temp %in% c("LINESTRING", "MULTILINESTRING"))) sf_type <- "sf_linear"
  if (all(temp %in% c("POLYGON", "MULTIPOLYGON"))) sf_type <- "sf_area"

  if (all(is.null(legacy_sites), is.null(legacy_var))) {
    legacy_option <- FALSE
  } else {
    legacy_option <- TRUE
  }

  if (is.null(stratum_var)) {
    stratum <- NULL
  } else {
    stratum <- names(n_base)
  }

  # check input. If errors, dsgn_check will stop grtspts and report errors.
  dsgn_check(
    sframe, sf_type, legacy_sites, legacy_option, stratum, seltype, n_base, caty_n,
    n_over, n_near, stratum_var, caty_var, aux_var, legacy_var, mindis,
    DesignID, SiteBegin, maxtry
  )

  # preserve original sframe names
  sframe_names <- names(sframe)

  ## Create variables in sample frame if needed.
  # Create unique sample frame ID values
  sframe$id <- 1:nrow(sframe)

  # Assign stratum variable or create it if design not stratified and variable not provided.
  if (is.null(stratum_var)) {
    stratum_var <- "stratum"
    sframe$stratum <- "None"
    stratum <- c("None") # names of all strata
  } else {
    # ensure class for stratum variable is character and assign to stratum
    sframe$stratum <- as.character(sframe[[stratum_var]])
  }

  # set stratum, caty, aux and legacy variables in legacy_sites if needed
  # add idpts to legacy_sites
  if (legacy_option == TRUE & sf_type != "sf_point") {
    legacy_names <- names(legacy_sites)
    legacy_sites$idpts <- 1:nrow(legacy_sites)
    if (stratum[1] == "None") {
      legacy_sites$stratum <- "None"
    } else {
      legacy_sites$stratum <- as.character(legacy_sites[[legacy_stratum_var]])
    }
    if (!is.null(caty_var)) legacy_sites$caty <- as.character(legacy_sites[[caty_var]])
    if (!is.null(aux_var)) legacy_sites$aux <- legacy_sites[[aux_var]]
    if (is.null(legacy_var)) {
      legacy_sites$legacy <- TRUE
      legacy_var <- "legacy"
    } else {
      legacy_sites$legacy <- legacy_sites[[legacy_var]]
    }
  }

  ## Create a dsgn list object
  # variable assignments to dsgn list object
  dsgn <- list(
    stratum_var = stratum_var, caty_var = caty_var, aux_var = aux_var,
    legacy_option = legacy_option, legacy_var = legacy_var, stratum = stratum,
    wgt_units = wgt_units, seltype = NULL, n_base = NULL, caty_n = NULL,
    n_over = NULL, n_near = NULL, mindis = mindis
  )

  # seltype
  if (length(seltype) == length(stratum)) {
    dsgn$seltype <- seltype
    names(dsgn$seltype) <- stratum
  } else {
    tmp <- sapply(stratum, function(x, seltype) {
      x <- seltype
    }, seltype)
    names(tmp) <- stratum
    dsgn$seltype <- tmp
  }

  # n_base
  if (length(n_base) == length(stratum)) {
    dsgn$n_base <- n_base
    names(dsgn$n_base) <- stratum
  } else {
    tmp <- sapply(stratum, function(x, n_base) {
      x <- n_base
    }, n_base)
    names(tmp) <- stratum
    dsgn$n_base <- tmp
  }

  # caty_n
  if (is.list(caty_n)) {
    dsgn$caty_n <- caty_n
  } else {
    tmp <- lapply(stratum, function(x, caty_n) {
      x <- caty_n
    }, caty_n)
    names(tmp) <- stratum
    dsgn$caty_n <- tmp
  }

  # n_over
  if (!is.null(n_over)) {
    if (is.list(n_over)) {
      dsgn$n_over <- n_over
    } else {
      tmp <- lapply(stratum, function(x, n_over) {
        x <- n_over
      }, n_over)
      names(tmp) <- stratum
      dsgn$n_over <- tmp
    }
  }

  # n_near
  if (!is.null(n_near)) {
    tmp <- sapply(stratum, function(x, n_near) {
      x <- n_near
    }, n_near)
    names(tmp) <- stratum
    dsgn$n_near <- tmp
  }

  # legacy_option
  if (legacy_option == TRUE) {
    tmp <- sapply(stratum, function(x, legacy_option) {
      x <- legacy_option
    }, legacy_option)
    names(tmp) <- stratum
    dsgn$legacy_option <- tmp
  }

  ## select sites for each stratum
  rslts <- lapply(dsgn$stratum, irs_stratum,
                  dsgn = dsgn, sframe = sframe, sf_type = sf_type, wgt_units = wgt_units,
                  pt_density = pt_density, legacy_option = legacy_option,
                  legacy_sites = legacy_sites, maxtry = maxtry, warn_ind, warn_df
  )
  names(rslts) <- stratum


  # combine across strata
  sites_legacy <- NULL
  sites_base <- NULL
  sites_over <- NULL
  sites_near <- NULL
  warn_ind <- FALSE
  warn_df <- NULL
  for (i in 1:length(rslts)) {
    sites_legacy <- rbind(sites_legacy, rslts[[i]]$sites_legacy)
    sites_base <- rbind(sites_base, rslts[[i]]$sites_base)
    sites_over <- rbind(sites_over, rslts[[i]]$sites_over)
    sites_near <- rbind(sites_near, rslts[[i]]$sites_near)
    if (rslts[[i]]$warn_ind) {
      warn_ind <- TRUE
      warn_df <- rbind(warn_df, rslts[[i]]$warn_df)
    }
  }

  # Create a siteID for all sites
  ntot <- NROW(sites_legacy) + NROW(sites_base) + NROW(sites_over) + NROW(sites_near)
  siteID <- gsub(" ", "0", paste0(DesignID, "-", format(SiteBegin - 1 + 1:ntot, sep = "")))
  nlast <- 0

  # Create siteID for legacy sites if present using DesignID and SiteBegin
  if (!is.null(sites_legacy)) {
    row.names(sites_legacy) <- 1:nrow(sites_legacy)
    sites_legacy$siteID <- siteID[1:nrow(sites_legacy)]
    nlast <- nrow(sites_legacy)
    # set siteuse and replsite
    sites_legacy$siteuse <- "Legacy"
    sites_legacy$replsite <- "None"
  }

  # Create siteID for base sites using DesignID and SiteBegin
  row.names(sites_base) <- 1:nrow(sites_base)
  sites_base$siteID <- siteID[(nlast + 1):(nlast + nrow(sites_base))]
  nlast <- nlast + nrow(sites_base)
  # set siteuse and replsite for base sites
  sites_base$siteuse <- "Base"
  sites_base$replsite <- "None"

  # create siteID for n_over sites if any
  if (!is.null(n_over)) {
    row.names(sites_over) <- 1:nrow(sites_over)
    sites_over$siteID <- siteID[(nlast + 1):(nlast + nrow(sites_over))]
    nlast <- nlast + nrow(sites_over)
    # set siteuse and replsite for n_over sites
    sites_over$siteuse <- "Over"
    sites_over$replsite <- "Next"
  }

  # if n_near sample sites, assign base ids to the replacement sites. then add siteIDs
  if (!is.null(n_near)) {
    tst <- match(paste(sites_near$stratum, sites_near$replsite, sep = "_"),
                 paste(sites_base$stratum, sites_base$idpts, sep = "_"),
                 nomatch = 0
    )
    sites_near$replsite[tst > 0] <- sites_base$siteID[tst]
    tst <- match(paste(sites_near$stratum, sites_near$replsite, sep = "_"),
                 paste(sites_over$stratum, sites_over$idpts, sep = "_"),
                 nomatch = 0
    )
    sites_near$replsite[tst > 0] <- sites_over$siteID[tst]

    # sort by id so that sites_near in same order as sites in sites_base and sites_over
    sites_near <- sites_near[order(sites_near$replsite, sites_near$siteuse), ]
    row.names(sites_near) <- 1:nrow(sites_near)
    # assign siteIDs
    sites_near$siteID <- siteID[(nlast + 1):(nlast + nrow(sites_near))]
  }

  # Add lat/lon in WGS84
  if (!is.null(sites_legacy)) {
    sites_legacy$lon_WGS84 <- st_coordinates(st_transform(sites_legacy, crs = 4326))[, "X"]
    sites_legacy$lat_WGS84 <- st_coordinates(st_transform(sites_legacy, crs = 4326))[, "Y"]
  }
  if (!is.null(sites_base)) {
    sites_base$lon_WGS84 <- st_coordinates(st_transform(sites_base, crs = 4326))[, "X"]
    sites_base$lat_WGS84 <- st_coordinates(st_transform(sites_base, crs = 4326))[, "Y"]
  }
  if (!is.null(sites_over)) {
    sites_over$lon_WGS84 <- st_coordinates(st_transform(sites_over, crs = 4326))[, "X"]
    sites_over$lat_WGS84 <- st_coordinates(st_transform(sites_over, crs = 4326))[, "Y"]
  }
  if (!is.null(sites_near)) {
    sites_near$lon_WGS84 <- st_coordinates(st_transform(sites_near, crs = 4326))[, "X"]
    sites_near$lat_WGS84 <- st_coordinates(st_transform(sites_near, crs = 4326))[, "Y"]
  }


  # reorder sf object variables by first specifying design names excluding unique
  # feature ID id and idpts as they are internal
  dsgn_names <- c(
    "siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84",
    "stratum", "wgt", "ip", "caty", "aux"
  )
  # sites_legacy
  if (!is.null(sites_legacy)) {
    if (sf_type != "sf_point") {
      add_names <- dsgn_names[dsgn_names %in% names(sites_legacy)]
      sites_legacy <- subset(sites_legacy, select = c(add_names, legacy_names))
    }
    if (sf_type == "sf_point") {
      add_names <- dsgn_names[dsgn_names %in% names(sites_legacy)]
      sites_legacy <- subset(sites_legacy, select = c(add_names, sframe_names))
    }
  }

  # sites_base
  # check what design variables are present in sf objects and add if missing
  add_names <- dsgn_names[dsgn_names %in% names(sites_base)]
  sites_base <- subset(sites_base, select = c(add_names, sframe_names))

  # sites_over
  if (!is.null(sites_over)) {
    add_names <- dsgn_names[dsgn_names %in% names(sites_over)]
    sites_over <- subset(sites_over,
                         select = c(add_names, sframe_names)
    )
  }

  # sites_near
  if (!is.null(sites_near)) {
    add_names <- dsgn_names[dsgn_names %in% names(sites_near)]
    sites_near <- subset(sites_near,
                         select = c(add_names, sframe_names)
    )
  }

  # add function call to dsgn list
  dsgn <- c(list(Call = match.call()), dsgn)

  # create output list
  sites <- list(
    sites_base = sites_base, sites_over = sites_over, sites_near = sites_near,
    design = dsgn
  )

  # As necessary, output a message indicating that warning messages were generated
  # during execution of the program

  if (warn_ind) {
    warn_df <<- warn_df
    if (nrow(warn_df) == 1) {
      cat("During execution of the program, a warning message was generated. The warning \nmessage is stored in a data frame named 'warn_df'.  Enter the following command \nto view the warning message: warndsgn()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn_df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn_df'.  Enter the following \ncommand to view the warning messages: warndsgn() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # constructor for design class
  sites <- structure(sites, class = "spdesign")

  # return the survey design sf object
  invisible(sites)
}
