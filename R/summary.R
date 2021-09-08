###############################################################################
# Function: summary.sframe and summary.design (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Summarize sample frames, samples, and design frames.
#'
#' @description
#' \code{summary()} summarizes sample frames, samples or design objects, depending on
#' which is provided. The right-hand of the
#' formula specifies the variables (or factors) to
#' summarize by. If the left-hand side of the formula is empty, the
#' summary will be of the distributions of the right-hand side variables. If the left-hand side
#' of the formula contains a variable, the summary will be of the left-hand size variable
#' for each level of each right-hand side variable.
#'
#' @param object A sample frame object having class \code{sframe} or a design object
#'  output from \code{grts()} or \code{irs()} having class \code{design}.
#'
#' @param formula A formula. Left-hand side variables can be numeric or
#' categorical (or factor) and right-hand side variables can be categorical
#' (or factor). Right-hand side variables that are numeric will be coerced
#' to a categorical (or factor) variable. If an intercept is included in the
#' right-hand side formula, the total will also be summarized.
#'
#' @param onlyshow A string indicating the single level of the single right-hand side
#' variable for which a summary is requested. This argument is only used when
#' a single right-hand side variable is provided.
#'
#' @param siteuse A character vector indicating the \code{design} sites
#' for which summaries are requested in \code{object}. Defaults to computing summaries for
#' each non-\code{NULL} \code{site_*} list in \code{object}.
#'
#' @param ... Additional arguments to pass to \code{summary()}. If the left-hand
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
#'
#' @method summary sframe
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @export
#'
#' @examples
#' NE_Lakes <- sframe(NE_Lakes)
#' summary(NE_Lakes, ELEV ~ 1)
#' summary(NE_Lakes, ~ ELEV_CAT * AREA_CAT)
#' sample <- grts(NE_Lakes, 100)
#' summary(sample, ~ ELEV_CAT * AREA_CAT)
summary.sframe <- function(object, formula, onlyshow = NULL, ...) {
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

#' @name summary
#' @method summary dframe
#' @export
summary.dframe <- function(object, formula, onlyshow = NULL, ...) {
  summary.sframe(object, formula, onlyshow, ...)
}

#' @name summary
#' @method summary spdesign
#' @export
summary.spdesign <- function(object, formula = ~siteuse, onlyshow = NULL, siteuse = NULL, ...) {
  if ((is.null(siteuse) & (!is.null(object$sites_near))) | "Near" %in% siteuse) {
    object$sites_near$siteuse <- "Near"
  }

  # bind
  object <- sprbind(object, siteuse)

  if (is.null(siteuse)) {
    fac_levels <- c("Legacy", "Base", "Near", "Over")
    fac_levels_used <- fac_levels[fac_levels %in% unique(object$siteuse)]
    object$siteuse <- factor(object$siteuse, levels = fac_levels_used)
  } else {
    object$siteuse <- factor(object$siteuse, levels = siteuse)
  }

  output <- summary.sframe(object, formula, onlyshow, ...)
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