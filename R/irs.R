###############################################################################
# Function: irs (exported)
# Programmers: Tony Olsen, Tom Kincaid
# Date: January 22, 2022
#' Select an independent random sample (IRS)
#'
#' Select a sample that is not spatially balanced from a point (finite), linear / linestring (infinite),
#' or areal / polygon (infinite) sampling frame using the Independent Random Sampling (IRS) algorithm.
#' The IRS algorithm accommodates unstratified and
#' stratified sampling designs and allows for equal inclusion probabilities, unequal
#' inclusion probabilities according to a categorical variable, and inclusion
#' probabilities proportional to a positive auxiliary variable. Several additional
#' sampling options are included, such as including legacy (historical) sites,
#' requiring a minimum distance between sites, and selecting replacement sites.
#'
#' @inherit grts params details return author
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{grts}}}{ to select a sample that is spatially balanced}
#'  }
#'
#' @examples
#' \dontrun{
#' sample <- irs(NE_Lakes, n_base = 100)
#' strata_n <- c(low = 25, high = 30)
#' sample_strat <- irs(NE_Lakes, n_base = strata_n, stratum_var = "ELEV_CAT")
#' sample_over <- irs(NE_Lakes, n_base = 30, n_over = 5)
#' }
#' @export
###############################################################################
irs <- function(sframe, n_base, stratum_var = NULL, seltype = NULL, caty_var = NULL,
                caty_n = NULL, aux_var = NULL, legacy_var = NULL,
                legacy_sites = NULL, legacy_stratum_var = NULL,
                legacy_caty_var = NULL, legacy_aux_var = NULL, mindis = NULL,
                maxtry = 10, n_over = NULL, caty_n_over = NULL, n_near = NULL, wgt_units = NULL,
                pt_density = NULL, DesignID = "Site", SiteBegin = 1) {
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

  # set default seltype if not provided (based on specification of other variables)
  if (is.null(seltype)) {
    if (is.null(caty_var) & is.null(aux_var)) {
      seltype <- "equal"
    } else if (!is.null(caty_var)) {
      seltype <- "unequal"
    } else {
      seltype <- "proportional"
    }
  }

  # check input. If errors, dsgn_check will stop grtspts and report errors.
  dsgn_check(
    sframe = sframe, sf_type = sf_type, legacy_sites = legacy_sites,
    legacy_option = legacy_option, stratum = stratum, seltype = seltype,
    n_base = n_base, caty_n = caty_n, n_over = n_over, caty_n_over = caty_n_over, n_near = n_near,
    stratum_var = stratum_var, caty_var = caty_var, aux_var = aux_var,
    legacy_var = legacy_var, mindis = mindis, DesignID = DesignID,
    SiteBegin = SiteBegin, maxtry = maxtry
  )

  # preserve original sframe names
  sframe_names <- names(sframe)

  # preserve original legacy_sites names if needed
  if (!is.null(legacy_sites)) {
    legacy_sites_names <- names(legacy_sites)
  }
  
  # Find geometry column name
  geom_col_name <- attr(sframe, "sf_column")
  if (geom_col_name != "geometry") {
    # Force to geometry for other sf consistency
    names(sframe)[names(sframe) == geom_col_name] <- "geometry"
    st_geometry(sframe) <- "geometry"
  }


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
  # add idpts to legacy_sites
  if (legacy_option == TRUE & (sf_type != "sf_point" | ((sf_type == "sf_point") & !is.null(legacy_sites)))) {
    legacy_names <- names(legacy_sites)
    legacy_sites$idpts <- 1:nrow(legacy_sites)
    if (stratum[1] == "None") {
      legacy_sites$stratum <- "None"
    } else {
      if (is.null(legacy_stratum_var)) {
        legacy_stratum_var <- stratum_var
      }
      legacy_sites$stratum <- as.character(legacy_sites[[legacy_stratum_var]])
    }
    if (!is.null(caty_var)) {
      if (is.null(legacy_caty_var)) {
        legacy_caty_var <- caty_var
      }
      legacy_sites$caty <- as.character(legacy_sites[[legacy_caty_var]])
    }
    if (!is.null(aux_var)) {
      if (is.null(legacy_aux_var)) {
        legacy_aux_var <- aux_var
      }
      legacy_sites$aux <- legacy_sites[[legacy_aux_var]]
    }
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
      n_over <- lapply(n_over, function(x) if (all(x == 0)) NULL else x)
      dsgn$n_over <- n_over
    } else if (!is.null(names(n_over)) && all(sort(stratum) == sort(names(n_over)))) {
      tmp <- lapply(stratum, function(x, n_over) {
        x <- n_over[[x]]
      }, n_over)
      names(tmp) <- stratum
      dsgn$n_over <- tmp
    } else {
      tmp <- lapply(stratum, function(x, n_over) {
        x <- n_over
      }, n_over)
      names(tmp) <- stratum
      dsgn$n_over <- tmp
    }
  }

  # caty_n_over
  if (!is.null(caty_n_over)) {
    if (is.list(caty_n_over)) {
      tmp <- lapply(stratum, function(x, caty_n_over) {
        caty_n_over[[x]]
      }, caty_n_over)
      names(tmp) <- stratum
      dsgn$n_over <- tmp
    } else {
      tmp <- lapply(stratum, function(x, caty_n_over) {
        caty_n_over
      }, caty_n_over)
      names(tmp) <- stratum
      dsgn$n_over <- tmp
    }
  }

  # n_near
  if (!is.null(n_near)) {
    if (is.list(n_near)) {
      n_near <- lapply(n_near, function(x) if (all(x == 0)) NULL else x)
      dsgn$n_near <- n_near
    } else if (!is.null(names(n_near)) && all(sort(stratum) == sort(names(n_near)))) {
      tmp <- lapply(stratum, function(x, n_near) {
        x <- n_near[[x]]
      }, n_near)
      names(tmp) <- stratum
      dsgn$n_near <- tmp
    } else {
      tmp <- lapply(stratum, function(x, n_near) {
        x <- n_near
      }, n_near)
      names(tmp) <- stratum
      dsgn$n_near <- tmp
    }
  }

  # mindis
  if (!is.null(mindis)) {
    if (is.list(mindis)) {
      mindis <- lapply(mindis, function(x) if (all(x == 0)) NULL else x)
      dsgn$mindis <- mindis
    } else {
      tmp <- lapply(stratum, function(x, mindis) {
        x <- mindis
      }, mindis)
      names(tmp) <- stratum
      dsgn$mindis <- tmp
    }
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
    legacy_sites = legacy_sites, maxtry = maxtry,
    warn_ind = warn_ind, warn_df = warn_df
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
  if (!is.null(sites_base)) {
    row.names(sites_base) <- 1:nrow(sites_base)
    sites_base$siteID <- siteID[(nlast + 1):(nlast + nrow(sites_base))]
    nlast <- nlast + nrow(sites_base)
    # set siteuse and replsite for base sites
    sites_base$siteuse <- "Base"
    sites_base$replsite <- "None"
  }

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
  # reorder sf object variables by first specifying design names excluding unique
  # feature ID id and idpts as they are internal
  dsgn_names <- c(
    "siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84",
    "stratum", "wgt", "ip", "caty", "aux"
  )

  dsgn_names_extra <- c(dsgn_names, "xcoord", "ycoord", "idpts")
  
  if (geom_col_name != "geometry") {
    # sframe prefix if necessary
    if (geom_col_name %in% dsgn_names_extra) {
      new_geom_col_name <- paste("sframe", geom_col_name, sep = "_")
      sframe_names[sframe_names == geom_col_name] <- new_geom_col_name
      geom_col_name <- new_geom_col_name
    }
    
    # restore original column names
    if (!is.null(sites_legacy)) {
      names(sites_legacy)[names(sites_legacy) == "geometry"] <- geom_col_name
      st_geometry(sites_legacy) <- geom_col_name
      
    }
    
    if (!is.null(sites_base)) {
      names(sites_base)[names(sites_base) == "geometry"] <- geom_col_name
      st_geometry(sites_base) <- geom_col_name
    }
    
    if (!is.null(sites_over)) {
      names(sites_over)[names(sites_over) == "geometry"] <- geom_col_name
      st_geometry(sites_over) <- geom_col_name
    }
    
    if (!is.null(sites_near)) {
      names(sites_near)[names(sites_near) == "geometry"] <- geom_col_name
      st_geometry(sites_near) <- geom_col_name
    }
  }

  # sites_legacy
  if (!is.null(sites_legacy)) {
    if (sf_type != "sf_point") {
      add_names <- dsgn_names[dsgn_names %in% names(sites_legacy)]
      legacy_sites_names_good <- legacy_sites_names[!legacy_sites_names %in% dsgn_names_extra]
      if (all(legacy_sites_names %in% legacy_sites_names_good)) {
        sites_legacy <- subset(sites_legacy, select = c(add_names, legacy_sites_names))
      } else {
        legacy_sites_names_bad <- legacy_sites_names[legacy_sites_names %in% dsgn_names_extra]
        legacy_sites_temp <- legacy_sites[, legacy_sites_names_bad, drop = FALSE]
        temp_geometry_col <- which(names(legacy_sites_temp) == attr(sites_legacy, "sf_column"))
        legacy_sites_geometry_col <- which(names(legacy_sites) == attr(sites_legacy, "sf_column"))
        names(legacy_sites_temp)[-temp_geometry_col] <- paste("legacy_sites", names(legacy_sites_temp)[-temp_geometry_col], sep = "_")
        sites_legacy <- st_join(sites_legacy, legacy_sites_temp, join = st_nearest_feature)
        sites_legacy <- subset(sites_legacy, select = c(add_names, legacy_sites_names_good[-legacy_sites_geometry_col], names(legacy_sites_temp)))
        for (i in names(sites_legacy)) {
          if (i %in% c("legacy_sites_xcoord", "legacy_sites_ycoord", "legacy_sites_idpts")) {
            names(sites_legacy)[which(names(sites_legacy) == i)] <- substring(i, first = 14)
          }
        }
      }
    }
    if (sf_type == "sf_point") {
      add_names <- dsgn_names[dsgn_names %in% names(sites_legacy)]
      sframe_names_good <- sframe_names[!sframe_names %in% dsgn_names_extra]
      if (all(sframe_names %in% sframe_names_good)) {
        sites_legacy <- subset(sites_legacy, select = c(add_names, sframe_names))
      } else {
        sframe_names_bad <- sframe_names[sframe_names %in% dsgn_names_extra]
        sframe_temp <- sframe[, sframe_names_bad, drop = FALSE]
        temp_geometry_col <- which(names(sframe_temp) == attr(sites_legacy, "sf_column"))
        sframe_geometry_col <- which(names(sframe) == attr(sites_legacy, "sf_column"))
        names(sframe_temp)[-temp_geometry_col] <- paste("sframe", names(sframe_temp)[-temp_geometry_col], sep = "_")
        sites_legacy <- st_join(sites_legacy, sframe_temp, join = st_nearest_feature)
        sites_legacy <- subset(sites_legacy,
          select = c(add_names, sframe_names_good[-sframe_geometry_col], names(sframe_temp))
        )
        for (i in names(sites_legacy)) {
          if (i %in% c("sframe_xcoord", "sframe_ycoord", "sframe_idpts")) {
            names(sites_legacy)[which(names(sites_legacy) == i)] <- substring(i, first = 8)
          }
        }
      }
    }
  }

  # sites_base
  # check what design variables are present in sf objects and add if missing
  if (!is.null(sites_base)) {
    add_names <- dsgn_names[dsgn_names %in% names(sites_base)]
    sframe_names_good <- sframe_names[!sframe_names %in% dsgn_names_extra]
    if (all(sframe_names %in% sframe_names_good)) {
      sites_base <- subset(sites_base, select = c(add_names, sframe_names))
    } else {
      sframe_names_bad <- sframe_names[sframe_names %in% dsgn_names_extra]
      sframe_temp <- sframe[, sframe_names_bad, drop = FALSE]
      temp_geometry_col <- which(names(sframe_temp) == attr(sites_base, "sf_column"))
      sframe_geometry_col <- which(names(sframe) == attr(sites_base, "sf_column"))
      names(sframe_temp)[-temp_geometry_col] <- paste("sframe", names(sframe_temp)[-temp_geometry_col], sep = "_")
      sites_base <- st_join(sites_base, sframe_temp, join = st_nearest_feature)
      sites_base <- subset(sites_base, select = c(add_names, sframe_names_good[-sframe_geometry_col], names(sframe_temp)))
      for (i in names(sites_base)) {
        if (i %in% c("sframe_xcoord", "sframe_ycoord", "sframe_idpts")) {
          names(sites_base)[which(names(sites_base) == i)] <- substring(i, first = 8)
        }
      }
    }
  }

  # sites_over
  if (!is.null(sites_over)) {
    add_names <- dsgn_names[dsgn_names %in% names(sites_over)]
    sframe_names_good <- sframe_names[!sframe_names %in% dsgn_names_extra]
    if (all(sframe_names %in% sframe_names_good)) {
      sites_over <- subset(sites_over, select = c(add_names, sframe_names))
    } else {
      sframe_names_bad <- sframe_names[sframe_names %in% dsgn_names_extra]
      sframe_temp <- sframe[, sframe_names_bad, drop = FALSE]
      temp_geometry_col <- which(names(sframe_temp) == attr(sites_over, "sf_column"))
      sframe_geometry_col <- which(names(sframe) == attr(sites_over, "sf_column"))
      names(sframe_temp)[-temp_geometry_col] <- paste("sframe", names(sframe_temp)[-temp_geometry_col], sep = "_")
      sites_over <- st_join(sites_over, sframe_temp, join = st_nearest_feature)
      sites_over <- subset(sites_over, select = c(add_names, sframe_names_good[-sframe_geometry_col], names(sframe_temp)))
      for (i in names(sites_over)) {
        if (i %in% c("sframe_xcoord", "sframe_ycoord", "sframe_idpts")) {
          names(sites_over)[which(names(sites_over) == i)] <- substring(i, first = 8)
        }
      }
    }
  }

  # sites_near
  if (!is.null(sites_near)) {
    add_names <- dsgn_names[dsgn_names %in% names(sites_near)]
    sframe_names_good <- sframe_names[!sframe_names %in% dsgn_names_extra]
    if (all(sframe_names %in% sframe_names_good)) {
      sites_near <- subset(sites_near, select = c(add_names, sframe_names))
    } else {
      sframe_names_bad <- sframe_names[sframe_names %in% dsgn_names_extra]
      sframe_temp <- sframe[, sframe_names_bad, drop = FALSE]
      temp_geometry_col <- which(names(sframe_temp) == attr(sites_near, "sf_column"))
      sframe_geometry_col <- which(names(sframe) == attr(sites_near, "sf_column"))
      names(sframe_temp)[-temp_geometry_col] <- paste("sframe", names(sframe_temp)[-temp_geometry_col], sep = "_")
      sites_near <- st_join(sites_near, sframe_temp, join = st_nearest_feature)
      sites_near <- subset(sites_near, select = c(add_names, sframe_names_good[-sframe_geometry_col], names(sframe_temp)))
      for (i in names(sites_near)) {
        if (i %in% c("sframe_xcoord", "sframe_ycoord", "sframe_idpts")) {
          names(sites_near)[which(names(sites_near) == i)] <- substring(i, first = 8)
        }
      }
    }
  }

  # add function call to dsgn list
  # dsgn <- c(list(Call = match.call()), dsgn)
  if (is.null(caty_n_over)) {
    dsgn <- list(
      call = match.call(), stratum = dsgn$stratum, n_base = dsgn$n_base,
      seltype = dsgn$seltype, caty_n = dsgn$caty_n, legacy = dsgn$legacy_option,
      mindis = dsgn$mindis, n_over = dsgn$n_over, caty_n_over = NULL, n_near = dsgn$n_near
    )
  } else {
    caty_n_over <- dsgn$n_over
    n_over <- n_over
    dsgn <- list(
      call = match.call(), stratum = dsgn$stratum, n_base = dsgn$n_base,
      seltype = dsgn$seltype, caty_n = dsgn$caty_n, legacy = dsgn$legacy_option,
      mindis = dsgn$mindis, n_over = n_over, caty_n_over = caty_n_over, n_near = dsgn$n_near
    )
  }
  
  # create output list
  sites <- list(
    sites_legacy = sites_legacy, sites_base = sites_base,
    sites_over = sites_over, sites_near = sites_near,
    design = dsgn
  )

  # As necessary, output a message indicating that warning messages were generated
  # during execution of the program

  if (warn_ind) {
    warn_df <<- warn_df
    if (nrow(warn_df) == 1) {
      cat("During execution of the program, a warning message was generated. The warning \nmessage is stored in a data frame named 'warn_df'.  Enter the following command \nto view the warning message: warnprnt()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn_df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn_df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # constructor for design class
  sites <- structure(sites, class = "spdesign")

  # return the survey design sf object
  invisible(sites)
}
