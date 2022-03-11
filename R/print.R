#' Print summaries of sampling frames, design sites, and analysis data.
#'
#' @name print
#' @method print summary.sp_frame
#'
#' @description \code{print()} prints summaries of sampling frames, design sites,
#'   and analysis data.
#' 
#' @param x An object output from \code{grts()}, \code{irs()}, \code{summary()}, or \code{sp_summary()}.
#' @param ... Additional print and summary arguments
#'
#' @details When \code{x} is output from \code{grts()} or \code{irs()}, \code{print()} returns
#'   a summary of site counts for each sites object (\code{sites_legacy}, \code{sites_base},
#'   \code{sites_near}, and \code{sites_over}). These site counts are tabled by strata and 
#'   unequal probability levels if applicable. When \code{x} is output from \code{summary()}
#'   or \code{sp_summary()}, relevant summaries by variables specified in a formula
#'   are returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary(sp_frame(NE_Lakes), AREA_CAT ~ ELEV_CAT)
#' samp <- grts(NE_Lakes, n_base = c(small = 15, large = 20), stratum_var = "AREA_CAT", n_over = 20)
#' samp
#' }
#' @noRd
print.summary.sp_frame <- function(x, ...) {
  
  if (inherits(x, "table")) {
    print.table(x, ...)
  }
  
  if (inherits(x, "list")) {
    for (i in seq_along(x)) {
      cat(names(x)[i], ": ", sep = "")
      cat("\n")
      print(x[[i]], ...)
      cat("\n")
    }
  }
}
#' @export
#' @noRd
print.sp_summary.sp_frame <- print.summary.sp_frame

#' @name print
#' @method print summary.sp_design
#' @export
#' @noRd
print.summary.sp_design <- function(x, ...) {
  print.summary.sp_frame(x, ...)
}

#' @export
#' @noRd
print.sp_summary.sp_design <- print.summary.sp_design

#' @name print
#' @method print sp_design
#' @export
#' @noRd
print.sp_design <- function(x, ...) {

  if (!is.null(x$design$stratum_var) & !is.null(x$design$caty_var)) {
    object <- summary(x, formula = siteuse ~ stratum * caty, ...)
  } else if (is.null(x$design$stratum_var) & !is.null(x$design$caty_var)) {
    object <- summary(x, formula = siteuse ~ caty, ...)
  } else if (!is.null(x$design$stratum_var) & is.null(x$design$caty_var)) {
    object <- summary(x, formula = siteuse ~ stratum, ...)
  } else {
    object <- summary(x, ...)
  }
  # if (is.null(x$stratum_var)) {
  #   object <- object[-which(names(object) == "stratum")]
  # }
  cat("Summary of Site Counts: ")
  cat("\n")
  cat("\n")
  print(object)
}