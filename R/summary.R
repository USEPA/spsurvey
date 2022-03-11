###############################################################################
# Function: summary functions (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Summarize sampling frames, design sites, and analysis data.
#'
#' @description
#' \code{summary()} summarizes sampling frames, design sites, and analysis data. The right-hand of the
#' formula specifies the variables (or factors) to
#' summarize by. If the left-hand side of the formula is empty, the
#' summary will be of the distributions of the right-hand side variables. If the left-hand side
#' of the formula contains a variable, the summary will be of the left-hand size variable
#' for each level of each right-hand side variable. Equivalent to \code{sp_summary()}; both
#' are currently maintained for backwards compatibility.
#'
#' @param object An object to summarize. When summarizing sampling frames,
#' an \code{sf}
#' object given the appropriate class using \code{sp_frame}.
#' When summarizing design sites, an object created by \code{grts()} or
#' \code{irs()} (which has class \code{sp_design}). When summarizing analysis data,
#' a data frame or an \code{sf} object given the appropriate class using \code{sp_frame}.
#'
#' @param formula A formula. One-sided formulas are used to summarize the
#' distribution of numeric or categorical variables. For one-sided formulas,
#' variable names are placed to the right of \code{~} (a right-hand side variable).
#' Two sided formulas are
#' used to summarize the distribution of a left-hand side variable
#' for each level of each right-hand side categorical variable in the formula.
#' Note that only for two-sided formulas are numeric right-hand side variables
#' coerced to a categorical variables. If an intercept
#' is included as a right-hand side variable (whether the formula is one-sided or
#' two-sided), the total will also be summarized. When summarizing sampling frames
#' or analysis data, the default formula is \code{~ 1}. When summarizing design sites,
#' \code{siteuse} should be used in the formula, and the default formula is
#' \code{~ siteuse}.
#'
#' @param onlyshow A string indicating the single level of the single right-hand side
#' variable for which a summary is requested. This argument is only used when
#' a single right-hand side variable is provided.
#'
#' @param siteuse A character vector indicating the design sites
#' for which summaries are requested in \code{object}. Defaults to computing summaries for
#' each non-\code{NULL} \code{sites_*} list in \code{object}.
#'
#' @param ... Additional arguments to pass to \code{sp_summary()}. If the left-hand
#' side of the formula is empty, the appropriate generic arguments are passed
#' to \code{summary.data.frame}. If the left-hand side of the formula is provided,
#' the appropriate generic arguments are passed to \code{summary.default}.
#'
#' @return If the left-hand side of the formula is empty, a named list
#' containing summaries of the count distribution for each right-hand side
#' variable is returned. If the left-hand side of the formula contains a
#' variable, a named list containing five number
#' summaries (numeric left-hand side) or tables (categorical or factor left
#' hand side) is returned for each right-hand side variable.
#'
#' @name summary
#' @method summary sp_frame
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' data("NE_Lakes")
#' summary(NE_Lakes, ELEV ~ 1)
#' summary(NE_Lakes, ~ ELEV_CAT * AREA_CAT)
#' sample <- grts(NE_Lakes, 100)
#' summary(sample, ~ ELEV_CAT * AREA_CAT)
#' }
summary.sp_frame <- function(object, formula = ~1, onlyshow = NULL, ...) {
  object <- sp_unframe(object)
  val <- sp_summary(object, formula, onlyshow, ...)
  new_val <- structure(val, class = c("summary.sp_frame", setdiff(class(val), "sp_summary.sp_frame")))
  new_val
}

#' @name summary
#' @method summary sp_design
#' @export
summary.sp_design <- function(object, formula = ~siteuse, siteuse = NULL, onlyshow = NULL, ...) {
  val <- sp_summary(object, formula, siteuse, onlyshow, ...)
  new_val <- structure(val, class = c("summary.sp_design", setdiff(class(val), "sp_summary.sp_design")))
  new_val
}