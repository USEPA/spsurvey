###############################################################################
# Function: compute_sp_balance_design, sp_balance_calc_safely (not exported)
# Programmer: Michael Dumelle
# Date: August 28, 2026
#'
#' Compute spatial balance eagerly for a grts()/irs() design, for the
#' legacy + base site set, using the frame-level inclusion probabilities
#' grts_stratum()/irs_stratum() already attached per stratum (see
#' balance_ip_density()). All seven metrics and the per-site extents are
#' computed and returned together.
#'
#' @param object The legacy + base site set for this design (an sf object
#'   with \code{$siteID}, \code{$stratum}, \code{$ip}).
#'
#' @param sframe_bal The per-stratum frame-level inclusion probabilities
#'   attached by \code{grts_stratum()}/\code{irs_stratum()}, combined across
#'   strata (an sf object with \code{$stratum}, \code{$ip}).
#'
#' @return A list with elements \code{metrics} (the full stratum/metric/value
#'   data frame for all seven metrics) and \code{extents} (a numeric vector
#'   of per-site Voronoi polygon extents, named by \code{siteID}).
#'
#' @noRd
###############################################################################
compute_sp_balance_design <- function(object, sframe_bal) {
  bal <- sp_balance.default(
    object, sframe_bal,
    stratum_var = "stratum", ip = "ip",
    metrics = spsurvey_balance_metrics, extents = TRUE
  )

  # sp_balance.default()'s extents rows are grouped by sort(unique(stratum))
  # and, within a stratum, in object's original row order (see its
  # object_levels/object_split); reproducing that same order here gives an
  # exact siteID alignment without depending on extents carrying siteID
  # itself.
  ordered_siteID <- unlist(lapply(
    sort(unique(object$stratum)),
    function(x) object$siteID[object$stratum == x]
  ))
  extents <- stats::setNames(bal$extents$extent, ordered_siteID)

  if (!all(is.finite(bal$metrics$value)) || !all(is.finite(extents))) {
    stop("spatial balance produced a non-finite result")
  }

  list(metrics = bal$metrics, extents = extents)
}

#' Compute spatial balance for a design
#'
#' Wraps \code{compute_sp_balance_design()} in pre-checks and a
#' \code{tryCatch} to avoid errors unrelated to the original function call.
#'
#' @param object The legacy + base site set for this design, or \code{NULL}
#'   if the design has neither.
#'
#' @param sframe_bal The per-stratum frame-level inclusion probabilities,
#'   combined across strata.
#'
#' @return A list with elements \code{result} (the value returned by
#'   \code{compute_sp_balance_design()}).
#'
#' @noRd
sp_balance_calc_safely <- function(object, sframe_bal) {
  if (is.null(object)) {
    return(list(result = NULL, warning = NULL))
  }

  warning_msg <- NULL
  result <- tryCatch(
    {
      on_solaris <- Sys.info()[["sysname"]] == "SunOS"
      if (on_solaris) {
        stop("spatial balance is not supported on Solaris")
      }
      if (is.na(st_crs(object)) || st_is_longlat(st_crs(object))) {
        stop("spatial balance requires a projected coordinate reference system")
      }
      strat_sizes <- table(object$stratum)
      if (any(strat_sizes < 2)) {
        stop("spatial balance requires at least two (legacy + base) sites per stratum")
      }
      compute_sp_balance_design(object, sframe_bal)
    },
    error = function(e) {
      warning_msg <<- paste0(
        "Spatial balance could not be computed and was not stored (",
        conditionMessage(e), "). Set sp_balance = FALSE to suppress ",
        "this attempt, or compute it externally with sp_balance.default()."
      )
      NULL
    }
  )

  list(result = result, warning = warning_msg)
}
