###############################################################################
# Function: summary.sframe and summary.design (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Calculate summaries of design objects
#' 
#' @description 
#' \code{summary()} summarizes sample frames or design objects, depending on 
#' which is provided. For design objects, sites in the base sample and
#' replacement sites (if they exist) are summarized. The right hand of the 
#' formula specifies the categorical variables (or factors) you want to
#' summarize by. If the left hand side of the formula is empty, the
#' summary will be of the
#' count distribution of the right hand side variables. If the left hand side
#' contains a variable, the summary will be of the left hand size variable
#' grouped by each of the right hand variables.
#'
#' @param object A sample frame object having class \code{sframe} or a design object
#'  output from \code{grts()} or \code{irs()} having class \code{design}.
#' @param formula A formula. Left hand side variables can be numeric or
#' categorical (or factor) and right hand side variables can be categorical
#' (or factor). Right hand side variables that are numeric will be coerced
#' to a categorical (or factor) variable. If an intercept is included in the
#' right hand side formula, the total will also be summarized.
#' @param onlyshow A string indicating the level of the single right hand side
#' variable for which a summary is requested.
#' @param ... Additional arguments to pass to \code{summary()}. If the left hand
#' side of the formula is empty, the appropriate generic arguments are passed
#' to \code{summary.data.frame}. If the left hand side of the formula is provided, 
#' the appropriate generic arguments are passed to \code{summary.default}.
#'
#' @return If the left hand side of the formula is empty, a named list 
#' containing summaries of the count distribution for each right hand side
#' varaiable is returned. If the left hand side of the formula contains a
#' variable, a named list contianing five number
#' summaries (numeric left hand side) or tables (categorical or factor left
#' hand side) is returned for each right hand side variable.
#' 
#' @name summary
#' 
#' @method summary sframe
#' 
#' @export
#' 
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @examples
#' \dontrun{
#'    NE_lakes <- sframe(NE_lakes)
#'    summary(NE_lakes, ELEVATION ~ 1)
#'    summary(NE_lakes, ~ ELEVATION_CAT * AREA_HA_CAT)
#'    sample <- grts(NE_lakes, 100)
#'    summary(sample, ELEVATION ~ 1)
#'    summary(sample, ~ ELEVATION_CAT * AREA_HA_CAT)
#' }
###############################################################################
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
  # calling the appropriate summary based on formula (right hand side vs two sided)
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
#' @method summary design
#' @export
summary.design <- function(object, formula, onlyshow = NULL, ...) {
  
  # keep the sites sf objects from class design
  sites <- object[names(object) %in% c("sites_base", "sites_over", "sites_near")]
  
  # storing output if non-null
  output <- lapply(sites, function(x) {
      if (is.null(x)) {
        x  
      } else {
        summary.sframe(x, formula, onlyshow, ...)
      }
    }
  )
  # returning non-null outuput
  output <- output[!vapply(output, is.null, logical(1))]
  output
}

# Helpers -----------------------------------------------------------------

cat_summary <- function(formlist, varsf, ...) {
  dotlist <- list(...)
  if (!("maxsum" %in% names(dotlist))) {
    dotlist$maxsum <- 1e10
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
    varlevels <- do.call("tapply",
                         c(list(X = varsf_nogeom[[formlist$response]],
                                INDEX = varsf_nogeom[[x]],
                                FUN = summary.default),
                           dotlist))
    do.call("rbind", varlevels)
  })
  names(output) <- paste(formlist$response, "by", formlist$varlabels, sep = " ")
  output
}





