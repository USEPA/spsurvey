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
                legacy_sites = NULL, legacy_var = NULL, mindis = NULL,
                DesignID = "Site", SiteBegin = 1, maxtry = 10) {

  # Ensure that the geometry types for sframe are consistent

  temp <- st_geometry_type(sframe)
  tst <- all(temp %in% c("POINT", "MULTIPOINT")) |
    all(temp %in% c("LINESTRING", "MULTILINESTRING")) |
    all(temp %in% c("POLYGON", "MULTIPOLYGON"))
  if (!tst) {
    stop(paste("\nThe geometry types for the survey frame object passed to function grts: \n\"", unique(st_geometry_type(sframe)), "\" are not consistent.", sep = ""))
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

  # Create warning indicator and data frame to collect all potential issues during
  # sample selection
  warn_ind <- FALSE
  warn_df <- data.frame(stratum = "Stratum", func = "Calling Function", warn = "Message")

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

  # set caty, aux and legacy variables in sample frame if needed
  if (!is.null(caty_var)) sframe$caty <- as.character(sframe[[caty_var]])
  if (!is.null(aux_var)) sframe$aux <- sframe[[aux_var]]
  if (!is.null(legacy_var)) sframe$legacy <- sframe[[legacy_var]]

  # set stratum, caty, aux and legacy variables in legacy_sites if needed
  if (legacy_option == TRUE) {
    if (!is.null(stratum_var)) legacy_sites$stratum <- as.character(legacy_sites[[stratum_var]])
    if (!is.null(caty_var)) legacy_sites$caty <- as.character(legacy_sites[[caty_var]])
    if (!is.null(aux_var)) legacy_sites$aux <- legacy_sites[[aux_var]]
    if (!is.null(legacy_var)) legacy_sites$legacy <- legacy_sites[[legacy_var]]
  }

  ## Create a dsgn list object
  # variable assignments to dsgn list object
  dsgn <- list(
    stratum_var = stratum_var, caty_var = caty_var, aux_var = aux_var,
    legacy_option = legacy_option, legacy_var = legacy_var, stratum = stratum,
    wgt_units = wgt_units, seltype = NULL, n_base = NULL, caty_n = NULL, n_over = NULL,
    n_near = NULL, mindis = mindis
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
    dsgn = dsgn, sframe = sframe, sf_type = sf_type,
    pt_density = pt_density, legacy_option = legacy_option,
    legacy_sites = legacy_sites, maxtry = maxtry, warn_ind, warn_df
  )
  names(rslts) <- stratum


  # combine across strata
  sites_base <- NULL
  sites_over <- NULL
  sites_near <- NULL
  warn_ind <- FALSE
  warn_df <- NULL
  for (i in 1:length(rslts)) {
    sites_base <- rbind(sites_base, rslts[[i]]$sites_base)
    sites_over <- rbind(sites_over, rslts[[i]]$sites_over)
    sites_near <- rbind(sites_near, rslts[[i]]$sites_near)
    if (rslts[[i]]$warn_ind) {
      warn_ind <- TRUE
      warn_df <- rbind(warn_df, rslts[[i]]$warn_df)
    }
  }

  # Create siteID for base sites using DesignID and SiteBegin
  sites_base$siteID <- gsub(" ", "0", paste0(
    DesignID, "-",
    format(SiteBegin - 1 + 1:nrow(sites_base), sep = "")
  ))
  # create siteID for n_over sites if any
  if (!is.null(n_over)) {
    jnk <- max(nchar(sites_base$siteID))
    nlast <- max(as.numeric(substr(sites_base$siteID, nchar(DesignID) + 2, jnk)))
    sites_over$siteID <- gsub(
      " ", "0",
      paste0(DesignID, "-", format(nlast + 1:nrow(sites_over), sep = ""))
    )
  }

  # if n_near sample sites, assign base ids to the replacement sites. then add siteIDs
  if (!is.null(n_near)) {
    tst <- match(sites_near$replsite, sites_base$id, nomatch = 0)
    sites_near$replsite[tst > 0] <- sites_base$siteID[tst]
    tst <- match(sites_near$replsite, sites_over$id, nomatch = 0)
    sites_near$replsite[tst > 0] <- sites_over$siteID[tst]

    # sort by id so that sites_near in same order as sites in sites_base and sites_over
    sites_near <- sites_near[order(sites_near$replsite, sites_near$siteuse), ]
    # assign siteIDs
    jnk <- max(nchar(sites_base$siteID), nchar(sites_over$siteID), na.rm = TRUE)
    nlast <- max(
      as.numeric(substr(sites_base$siteID, nchar(DesignID) + 2, jnk)),
      as.numeric(substr(sites_over$siteID, nchar(DesignID) + 2, jnk))
    )
    sites_near$siteID <- gsub(
      " ", "0",
      paste0(DesignID, "-", format(nlast + 1:nrow(sites_near), sep = ""))
    )
  }

  # reorder sf object variables by first specifying design names excluding unique
  # feature ID id and idpts as they are internal
  dsgn_names <- c(
    "siteID", "replsite", "siteuse", "stratum", "wgt", "ip", "caty", "aux",
    "legacy"
  )
  # check what design variables are present in sf objects and add if missing
  tmp_names <- names(sites_base)
  add_names <- dsgn_names[!(dsgn_names %in% tmp_names)]
  if (!is.null(add_names)) {
    tmp <- matrix(NA,
      nrow = nrow(sites_base), ncol = length(add_names),
      dimnames = list(NULL, add_names)
    )
    sites_base <- cbind(sites_base, tmp)
    if (!is.null(sites_over)) {
      tmp <- matrix(NA,
        nrow = nrow(sites_over), ncol = length(add_names),
        dimnames = list(NULL, add_names)
      )
      sites_over <- cbind(sites_over, tmp)
    }
    if (!is.null(sites_near)) {
      tmp <- matrix(NA,
        nrow = nrow(sites_near), ncol = length(add_names),
        dimnames = list(NULL, add_names)
      )
      sites_near <- cbind(sites_near, tmp)
    }
  }
  # check if any dsgn_names occur in sframe names and drop in sframe names if duplicated
  sframe_names <- sframe_names[!(sframe_names %in% dsgn_names)]
  # use subset to reorder variables and drop internal variables and duplicated variables
  sites_base <- subset(sites_base, select = c(dsgn_names, sframe_names))
  if (!is.null(sites_over)) sites_over <- subset(sites_over, select = c(dsgn_names, sframe_names))
  if (!is.null(sites_near)) sites_near <- subset(sites_near, select = c(dsgn_names, sframe_names))

  # Change weight units to user specified if not NULL
  if (!is.null(wgt_units)) {
    # change sites_base weights
    sites_base$wgt <- set_units(sites_base$wgt, wgt_units)
    # change sites_over weights if sites_over present
    if (!is.null(sites_over)) {
      sites_over$wgt <- set_units(sites_over$wgt, wgt_units)
    }
    # change sites_near weights if sites_near present
    if (!is.null(sites_near)) {
      sites_near$wgt <- set_units(sites_near$wgt, wgt_units)
    }
  }

  # add function call to dsgn list
  dsgn <- c(list(Call = match.call()), dsgn)

  # create output list
  sites <- list(
    sites_base = sites_base, sites_over = sites_over, sites_near = sites_near,
    dsgn = dsgn
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
  sites <- structure(sites, class = "design")

  # return the survey design sf object
  invisible(sites)
}
