###############################################################################
# Function: sp_summary functions (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Summarize sampling frames, design sites, and analysis data.
#'
#' @description
#' \code{sp_summary()} summarizes sampling frames, design sites, and analysis data. The right-hand of the
#' formula specifies the variables (or factors) to
#' summarize by. If the left-hand side of the formula is empty, the
#' summary will be of the distributions of the right-hand side variables. If the left-hand side
#' of the formula contains a variable, the summary will be of the left-hand size variable
#' for each level of each right-hand side variable.
#'
#' @param object An object to summarize. When summarizing sampling frames,
#' or analysis data, a data frame or \code{sf}
#' object. When summarizing design sites, an object created by \code{grts()} or
#' \code{irs()} (which has class \code{spdesign}). When summarizing analysis data,
#' a data frame or an \code{sf} object.
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
#' @name sp_summary
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("NE_Lakes")
#' sp_summary(NE_Lakes, ELEV ~ 1)
#' sp_summary(NE_Lakes, ~ ELEV_CAT * AREA_CAT)
#' sample <- grts(NE_Lakes, 100)
#' sp_summary(sample, ~ ELEV_CAT * AREA_CAT)
#' }
sp_summary <- function(object, ...) {
  UseMethod("sp_summary", object)
}

#' @name sp_summary
#' @method sp_summary default
#' @export
sp_summary.default <- function(object, formula = ~1, onlyshow = NULL, ...) {

  # find system info
  on_solaris <- Sys.info()[["sysname"]] == "SunOS"
  if (on_solaris) {
    stop("sp_summary() is not supported on Solaris.")
  }

  # making formlist (utils.R)
  formlist <- make_formlist(formula, onlyshow, object)
  # making varsf (utils.R)
  varsf <- make_varsf(object, formlist)
  # accomodating an intercept
  if (formlist$intercept) {
    # adding "total" to varlabels
    formlist$varlabels <- c("total", formlist$varlabels)
    # making a factor called "total"
    varsf$total <- as.factor("total")
    # adding the factor to varsf
    varsf <- varsf[, c(formlist$response, formlist$varlabels), drop = FALSE]
  }
  # calling the appropriate summary based on formula (right-hand side vs two sided)
  if (is.null(formlist$response)) {
    output <- cat_summary(formlist, varsf, ...)
    output
  } else {
    output <- cont_summary(formlist, varsf, ...)
    output
  }
}

#' @name sp_summary
#' @method sp_summary spdesign
#' @export
sp_summary.spdesign <- function(object, formula = ~siteuse, siteuse = NULL, onlyshow = NULL, ...) {
  if ((is.null(siteuse) & (!is.null(object$sites_near))) | "Near" %in% siteuse) {
    object$sites_near$siteuse <- "Near"
  }

  # bind
  object <- sp_rbind(object, siteuse)

  if (is.null(siteuse)) {
    fac_levels <- c("Legacy", "Base", "Near", "Over")
    fac_levels_used <- fac_levels[fac_levels %in% unique(object$siteuse)]
    object$siteuse <- factor(object$siteuse, levels = fac_levels_used)
  } else {
    object$siteuse <- factor(object$siteuse, levels = siteuse)
  }

  output <- sp_summary.default(object, formula, onlyshow, ...)
  output
}

# Helpers -----------------------------------------------------------------

cat_summary <- function(formlist, varsf, ...) {
  dotlist <- list(...)
  if (!("maxsum" %in% names(dotlist))) {
    dotlist$maxsum <- 10
  }
  if ("sf" %in% class(varsf)) {
    varsf_nogeom <- st_drop_geometry(varsf)
  } else {
    varsf_nogeom <- varsf
  }

  if (!is.null(formlist$onlyshow)) {
    indexcol <- formlist$varlabels[length(formlist$varlabels)]
    varsf_nogeom <- varsf_nogeom[varsf_nogeom[[indexcol]] == formlist$onlyshow, indexcol, drop = FALSE]
    varsf_nogeom <- na.omit(varsf_nogeom)
    varsf_nogeom[[indexcol]] <- factor(varsf_nogeom[[indexcol]], levels = formlist$onlyshow)
  }
  output <- do.call("summary.data.frame", c(list(varsf_nogeom), dotlist))
}

cont_summary <- function(formlist, varsf, ...) {
  dotlist <- list(...)
  if ("sf" %in% class(varsf)) {
    varsf_nogeom <- st_drop_geometry(varsf)
  } else {
    varsf_nogeom <- varsf
  }
  if (!is.null(formlist$onlyshow)) {
    formlist$varlabels <- formlist$varlabels[length(formlist$varlabels)]
    varsf_nogeom <- varsf_nogeom[varsf_nogeom[[formlist$varlabels]] == formlist$onlyshow, c(formlist$response, formlist$varlabels), drop = FALSE]
    varsf_nogeom <- na.omit(varsf_nogeom)
    varsf_nogeom[[formlist$varlabels]] <- factor(varsf_nogeom[[formlist$varlabels]], levels = formlist$onlyshow)
  }
  output <- lapply(formlist$varlabels, function(x) {
    varlevels <- do.call(
      "tapply",
      c(
        list(
          X = varsf_nogeom[[formlist$response]],
          INDEX = varsf_nogeom[[x]],
          FUN = summary.default,
          simplify = FALSE
        ),
        dotlist
      )
    )
    do.call("rbind", varlevels)
  })
  names(output) <- paste(formlist$response, "by", formlist$varlabels, sep = " ")
  output
}
