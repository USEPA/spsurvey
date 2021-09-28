###############################################################################
# Function: grts (exported)
# Programmers: Tony Olsen, Tom Kincaid
# Date: January 22, 2021
#' Select a generalized random tessellation stratified (GRTS) sample
#'
#' Select a spatially balanced sample from a point (finite), linear (infinite),
#' or polygon / areal (infinite) sample frame using the Generalized Random Tessellation
#' Stratified (GRTS) algorithm. The GRTS algorithm accommodates unstratified and
#' stratified designs and allows for equal inclusion probabilities, unequal
#' inclusion probabilities according to a categorical variable, and inclusion
#' probabilities proportional to a positive auxiliary variable. Several additional
#' sampling options are included, such as including legacy (historical) sites,
#' requiring a minimum distance between sites, and selecting replacement sites.
#' For technical details, see Stevens and Olsen (2003).
#'
#'
#' @param sframe The sample frame as an \code{sf} object. The coordinate
#'   system for \code{sframe} must be one where distance for coordinates is meaningful.
#'
#' @param n_base The base sample size required. If the design is unstratified,
#'   this is a single numeric value. If the design is stratified, this is a named
#'   vector or list whose names represent each stratum and whose values represent each
#'   stratum's sample size. These names must match the values of the stratification
#'   variable represented by \code{stratum_var}.
#'
#' @param stratum_var A character string containing the name of the column from
#'   \code{sframe} that identifies stratum membership for each element in the frame.
#'   If stratum equals \code{NULL}, the design is unstratified and all elements in \code{sframe}
#'   are eligible to be selected in the sample. The default is \code{NULL}.
#'
#' @param seltype A character string or vector indicating the inclusion probability type,
#'   which must be one of following: \code{"equal"} for equal inclusion probabilities;
#'   \code{"unequal"} for unequal inclusion probabilities according to a categorical
#'   variable specified by \code{caty_var}; and \code{"proportional"} for inclusion
#'   probabilities proportional to a positive auxiliary variable. If the design is
#'   unstratified, \code{seltype} is a single character vector. If the design is stratified, \code{seltype} is a named vector
#'   whose names represent each stratum and whose values represent each stratum's
#'   inclusion probability type. \code{seltype}'s default value tries to match the
#'   intended inclusion probability type: If \code{caty_var} and \code{aux_var} are
#'   not specified, \code{seltype} is \code{"equal"}; if \code{caty_var} is specified,
#'   \code{seltype} is \code{"unequal"}; and if \code{aux_var} is specified, \code{seltype}
#'   is \code{"proportional"}.
#'
#' @param caty_var A character string containing the name of the column from
#'   \code{sframe} that represents the unequal probability variable.
#'
#' @param caty_n A character vector indicating the expected sample size for each
#'   level of \code{caty_var}, the unequal probability variable. If the design
#'   is unstratified, \code{caty_n} is a named vector whose names represent each
#'   level of \code{caty_var} and whose values represent each level's expected
#'   sample size. The sum of \code{caty_n} must equal \code{n_base}. If the design
#'   is stratified and the expected sample sizes are the same among strata, \code{caty_n} is
#'   a named vector whose names represent represent each
#'   level of \code{caty_var} and whose values represent each level's expected
#'   sample size -- these expected sample sizes are applied to all strata. The sum of
#'   \code{caty_n} must equal each stratum's value in \code{n_base}.
#'   If the design is stratified and the expected sample sizes differ among strata,
#'   \code{caty_n} is a list where each element  is named as a stratum in \code{n_base}.
#'   Each stratum's list element is a named vector whose
#'   names represent each level of \code{caty_var} and whose values represent each
#'   level's expected sample size (within the stratum). The sum of the values in each stratum's
#'   list element must equal that stratum's value in \code{n_base}.
#'
#' @param aux_var A character string containing the name of the column from
#'   \code{sframe} that represents the proportional (to size) inclusion probability
#'   variable (auxiliary variable). This auxiliary variable must be positive, and the resulting
#'   inclusion probabilities are proportional to the values of the auxiliary variable.
#'   Larger values of the auxiliary variable result in higher inclusion probabilities.
#'
#' @param legacy_var If \code{sframe} is a \code{POINT} or \code{MULTIPOINT} geometry (a finite sample frame),
#'   \code{legacy_var} is a character string containing the name of the column
#'   from \code{sframe} that represents the legacy site variable. For legacy sites, the values of the
#'   \code{legacy_var} column in \code{sframe} must contain character strings that
#'   act as a legacy site identifier. For non-legacy sites, the values of the
#'   \code{legacy_var} column in \code{sframe} must be \code{NA}.
#'
#' @param legacy_sites If \code{sframe} is a \code{LINESTRING}, \code{MULTILINESTRING},
#'   \code{POLYGON}, or \code{MULTIPOLYGON} geometry (an infinite sample frame),
#'   \code{legacy_sites} is an sf object with a \code{POINT} geometry representing the
#'   legacy sites.
#'
#' @param legacy_stratum_var A character string containing the name of the column from
#'   \code{legacy_sites} that identifies stratum membership for each element of \code{legacy_sites}.
#'   This argument is required when the design is stratified and its levels
#'   must be contained in the levels of the \code{stratum_var} variable. The default value of \code{legacy_stratum_var}
#'   is \code{stratum_var}, so \code{legacy_stratum_var} need only be specified explicitly when
#'   the name of the stratification variable in \code{legacy_sites} differs from \code{stratum_var}.
#'
#' @param legacy_caty_var A character string containing the name of the column from
#'   \code{legacy_sites} that identifies the unequal probability variable for each element of \code{legacy_sites}.
#'   This argument is required when the design uses unequal selection probabilities and its categories
#'   must be contained in the levels of the \code{caty_var} variable. The default value of \code{legacy_caty_var}
#'   is \code{caty_var}, so \code{legacy_caty_var} need only be specified explicitly when
#'   the name of the unequal probability variable in \code{legacy_sites} differs from \code{caty_var}.
#'
#' @param legacy_aux_var A character string containing the name of the column from
#'   \code{legacy_sites} that identifies the proportional probability variable for each element of \code{legacy_sites}.
#'   This argument is required when the design uses proportional selection probabilities and the values of the
#'   \code{legacy_aux_var} variable must be positive. The
#'   default value of \code{legacy_aux_var} is \code{aux_var}, so \code{legacy_aux_var} need only be specified explicitly
#'   when the name of the proportional probability variable in \code{legacy_sites} differs from \code{aux_var}.
#'
#' @param mindis A numeric value indicating the desired minimum distance between sampled
#'   sites. If design is stratified, then mindis is applied separately for each stratum.
#'   The units of \code{mindis} must match the units in \code{sframe}.
#'
#' @param maxtry The number of maximum attempts to apply the minimum distance algorithm to obtain
#'   the desired minimum distance between sites. Each iteration takes roughly as long as the
#'   standard GRTS algorithm. Successive iterations will always contain at least as many
#'   sites satisfying the minimum distance requirement as the previous iteration. The algorithm stops
#'   when the minimum distance requirement is met or there are \code{maxtry} iterations.
#'   The default number of maximum iterations is \code{10}.
#'
#' @param n_over If the design is unstratified and \code{seltype} is \code{"equal"} or \code{"proportional"},
#'   \code{n_over} is an integer specifying the number of reverse hierarchically
#'   ordered (rho) replacement sites desired.  If the design is unstratified and \code{seltype} is \code{"unequal"},
#'   then \code{n_over} is a named character vector whose names match the levels of
#'   \code{caty_var} and whose values are the expected rho replacement sample sizes for each
#'   level. If the design is stratified and \code{seltype} is \code{"equal"} or \code{"proportional"},
#'   \code{n_over} is a list whose names match the names of \code{n_base} and whose values
#'   indicate the number of rho replacement sites for each stratum. If replacement sites are not desired for a particular stratum, then the corresponding value in \code{n_over} should be \code{NULL}. If the design is stratified and
#'   \code{seltype} is \code{"unequal"}, \code{n_over} changes based on whether the expected
#'   rho replacement sample sizes change among strata. If \code{n_over} does not change among strata, \code{n_over}
#'   is a named vector whose names match the names of \code{caty_var} and whose values are the expected rho replacement
#'   sample sizes to be used for each stratum. If \code{n_over} changes among strata, \code{n_over} is a
#'   list whose names match the names of \code{n_base} and whose values
#'   indicate the number of rho replacement sites for each stratum.
#'   Each stratum's list element is a named vector whose names represent each level of \code{caty_var} and whose values represent each
#'   level's expected rho replacement sample sizes (within the stratum). If replacement sites are not desired for a particular stratum, then the corresponding value in \code{n_over} should be \code{NULL}.
#'
#' @param n_near If the design is unstratified, \code{n_near} is integer from \code{1}
#'   to \code{10} specifying the number of
#'   nearest neighbor replacement sites to be selected for each base site. If the design
#'   is stratified but the same number of nearest neighbor replacement sites is desired
#'   for each stratum,  \code{n_near} is integer from \code{1}
#'   to \code{10} specifying the number of
#'   nearest neighbor replacement sites to be selected for each base site. If the design is
#'   unstratified and a different number of nearest neighbor replacement sites is
#'   desired for each stratum, \code{n_near} is a list whose names represent strata and whose
#'   values is integer from \code{1}
#'   to \code{10} specifying the number of
#'   nearest neighbor replacement sites to be selected for each base site in the stratum. If replacement sites are not desired for a particular stratum, then the corresponding value in \code{n_over} should be \code{NULL}. For
#'   infinite sample frames, the distance between a site and its nearest neighbor
#'   depends on \code{pt_density}.
#'
#' @param wgt_units The units used to compute the survey design weights. These
#'   units must be standard units as defined by the \code{set_units()} function in
#'   the units package. The default units match the units of the sf object.
#'
#' @param pt_density A numeric value controlling the density of the GRTS approximation
#'   for infinite sample frames. The GRTS approximation for infinite sample
#'   frames vastly improves computational efficiency by generating many finite points and
#'   selecting a sample from the points. \code{pt_density} represents the density
#'   of finite points per unit to use in the approximation (and the units match
#'   the units of the sample frame. The default is a density
#'   such that the number of finite points used in the approximation equals 10
#'   times the sample size requested.
#'
#' @param DesignID A character string indicating the naming structure for each
#'   site's identifier selected in the sample, which is matched with \code{SiteBegin} and
#'   included as a variable in the
#'   sf object in the function's output. Default is "Site".
#'
#' @param SiteBegin A character string indicating the first number to use to match
#'   with \code{DesignID} while creating each site's identifier selected in the sample.
#'   Successive sites are given successive integers. The default starting number
#'   is \code{1} and the number of digits is equal to number of digits in
#'   \code{nbase + nover}.
#'   For example, if \code{nbase} is 50 and \code{nover} is 0, then the default
#'   site identifiers are \code{Site-01} to \code{Site-50}
#'
#' @details \code{n_base} is the number of sites used to calculate
#'   the sampling weights, which is typically the number of sites that will be used for population
#'   estimates. When a panel design is implemented, \code{n_base} is typically the
#'   number of sites in all panels that will be sampled in the same temporal period --
#'   \code{n_base} is not the total number of sites in all panels. The sum of \code{n_base} and
#'   \code{n_over} is equal to the total number of sites to be visited for all panels plus
#'   any replacement sites that may be required.
#'
#' @return A list with five elements:
#'   \itemize{
#'     \item \code{sites_legacy} An sf object containing legacy sites. This is
#'       \code{NULL} if legacy sites were not included in the sample.
#'     \item \code{sites_base} An sf object containing the base sites.
#'     \item \code{sites_over} An sf object containing the reverse hierarchically
#'       ordered replacement sites. This is \code{NULL} if no reverse hierarchically
#'       ordered replacement sites were included in the sample.
#'     \item \code{sites_near} An sf object containing the nearest neighbor
#'       replacement sites. This is \code{NULL} if no nearest neighbor replacement
#'       sites were included in the sample.
#'     \item \code{design} A list documenting the specifications of this design.
#'       This can be checked to verify your design ran as intended.
#'       \itemize{
#'         \item \code{Call} The original function call.
#'         \item \code{stratum} The unique strata. This equals \code{"None"} if
#'           the design was unstratified.
#'         \item \code{n_base} The base sample size per stratum.
#'         \item \code{seltype} The selection type per stratum.
#'         \item \code{caty_n} The expected sample sizes for each level of the
#'           unequal probability grouping variable per stratum. This equals
#'           \code{NULL} when \code{seltype} is not \code{"unequal"}.
#'         \item \code{legacy} A logical variable indicating whether legacy sites
#'           were included in the sample.
#'         \item \code{mindis} The minimum distance requirement desired. This
#'           equals \code{NULL} if there was no minimum distance requirement.
#'         \item \code{n_over} The reverse hierarchically ordered replacement
#'           site sample sizes per stratum. If \code{seltype} is \code{unequal},
#'           this represents the expected sample sizes. This is \code{NULL}
#'           if no reverse hierarchically ordered replacement sites were included
#'           in the sample.
#'         \item \code{n_near} The number of nearest neighbor replacement sites
#'           desired. This is \code{NULL} if no nearest neighbor replacement
#'           sites were included in the sample.
#'       }
#'   }
#'   When non-\code{NULL}, the \code{sites_legacy}, \code{sites_base},
#'   \code{sites_over}, and \code{sites_near} objects contain the original columns
#'   in \code{sframe} and include a few additional columns. These additional columns
#'   are
#'   \itemize{
#'     \item \code{siteID} A site identifier (as named using the \code{DesignID}
#'       and \code{SiteBegin} arguments to \code{grts()}).
#'     \item \code{siteuse} Whether the site is a legacy site (\code{Legacy}), base
#'       site (\code{Base}), reverse hierarchically ordered replacement site
#'       (\code{Over}), or nearest neighbor replacement site (\code{Near}).
#'     \item \code{replsite} The replacement site ordering. \code{replsite} is
#'       \code{None} if the site is not a replacement site, \code{Next} if it is
#'       the next reverse hierarchically ordered replacement site to use, or
#'       \code{Near_}, where the word following \code{_} indicates the ordering of sites closest to
#'       the originally sampled site.
#'     \item \code{lon_WGS84} Longitude coordinates using the WGS84 coordinate
#'       system (EPSG:4326).
#'     \item \code{lat_WGS84} Latitude coordinates using the WGS84 coordinate
#'       system (EPSG:4326).
#'     \item \code{stratum} A stratum indicator. \code{stratum} is \code{None}
#'       if the design was unstratified. If the design was \code{stratified},
#'       \code{stratum} indicates the stratum.
#'     \item \code{wgt} The survey design weight.
#'     \item \code{ip} The site's original inclusion probability (the reciprocal)
#'       of (\code{wgt}).
#'     \item \code{caty} An unequal probability grouping indicator. \code{caty}
#'       is \code{None} if the design did not use unequal inclusion probabilities.
#'       If the design did use unequal inclusion probabilities, \code{caty}
#'       indicates the unequal probability level.
#'     \item \code{aux} The auxiliary proportional probability variable. This
#'       column is only returned if \code{seltype} was \code{proportional} in the
#'       original design.
#'   }
#'
#' @author Tony Olsen \email{olsen.tony@@epa.gov}
#'
#' @keywords survey design
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{irs}}}{ to select a sample that is not spatially balanced}
#'  }
#'
#' @references
#' Stevens Jr., Don L. and Olsen, Anthony R. (2004). Spatially balanced sampling
#' of natural resources. \emph{Journal of the american Statistical association}, 99(465), 262-278.
#'
#' @examples
#' sample <- grts(NE_Lakes, n_base = 100)
#' strata_n <- c(low = 25, high = 30)
#' sample_strat <- grts(NE_Lakes, n_base = strata_n, stratum_var = "ELEV_CAT")
#' sample_over <- grts(NE_Lakes, n_base = 30, n_over = 5)
#' @export
################################################################################
grts <- function(sframe, n_base, stratum_var = NULL, seltype = NULL, caty_var = NULL,
                 caty_n = NULL, aux_var = NULL, legacy_var = NULL,
                 legacy_sites = NULL, legacy_stratum_var = NULL,
                 legacy_caty_var = NULL, legacy_aux_var = NULL, mindis = NULL,
                 maxtry = 10, n_over = NULL, n_near = NULL, wgt_units = NULL,
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
    stop(paste("\nThe geometry types for the survey frame object passed to function grts: \n\"",
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
    n_base = n_base, caty_n = caty_n, n_over = n_over, n_near = n_near,
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
  if (legacy_option == TRUE & sf_type != "sf_point") {
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
      legacy_sites$aux <- as.character(legacy_sites[[legacy_aux_var]])
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
    if (is.list(n_near)) {
      dsgn$n_near <- n_near
    } else {
      tmp <- sapply(stratum, function(x, n_near) {
        x <- n_near
      }, n_near)
      names(tmp) <- stratum
      dsgn$n_near <- tmp
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
  rslts <- lapply(dsgn$stratum, grts_stratum,
    dsgn = dsgn, sframe = sframe, sf_type = sf_type, wgt_units = wgt_units,
    pt_density = pt_density, legacy_option = legacy_option,
    legacy_sites = legacy_sites, maxtry = maxtry, warn_ind = warn_ind, warn_df = warn_df
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
  dsgn_names <- c(
    "siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84",
    "stratum", "wgt", "ip", "caty", "aux"
  )

  dsgn_names_extra <- c(dsgn_names, "xcoord", "ycoord", "idpts")

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
                               select = c(add_names, sframe_names_good[-sframe_geometry_col], names(sframe_temp)))
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
  dsgn <- list(
    call = match.call(), stratum = dsgn$stratum, n_base = dsgn$n_base,
    seltype = dsgn$seltype, caty_n = dsgn$caty_n, legacy = dsgn$legacy_option,
    mindis = dsgn$mindis, n_over = dsgn$n_over, n_near = dsgn$n_near
  )

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
