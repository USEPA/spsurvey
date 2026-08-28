# Helpers ---------------------------------------------------------------------

#' Parse a summary/plot formula into its component pieces
#'
#' This function parses the formula supplied to \code{summary.sp_frame},
#' \code{summary.sp_design}, \code{sp_plot}, etc. into the variable names,
#' term labels, and intercept/response indicators needed to build the
#' variable-by-variable summaries or plots.
#'
#' @param formula A formula, e.g. \code{~ AREA_CAT} or \code{AREA ~ AREA_CAT}.
#'
#' @param onlyshow Character vector of variable levels to show (optional). If
#'   missing, all levels are shown.
#'
#' @param object The \code{sf} object (or data frame) the formula is applied
#'   to; used by \code{terms()} to resolve variable names.
#'
#' @param remove_geom Logical. If \code{TRUE} (the default), the geometry
#'   column name is dropped from the right-hand-side term labels.
#'
#' @return A list with elements \code{varterms}, \code{allvars},
#'   \code{varlabels}, \code{intercept}, \code{response}, \code{varnames},
#'   \code{varnames_split}, and \code{onlyshow}.
#'
#' @noRd
make_formlist <- function(formula, onlyshow, object, remove_geom = TRUE) {
  # find all terms from the formula
  varterms <- terms(formula, data = object)
  # find all variable names
  allvars <- all.vars(varterms)
  # find all right hand side names
  varlabels <- attr(varterms, "term.labels")
  # remove geometry if present
  # this was taken to be logical so it could 1) show geometry summaries and 2) remove a bug that
  # caused summaries to fail silently if no geometry column was in the data
  # but remain unaltered for plots
  if (remove_geom) {
    varlabels <- varlabels[varlabels != attr(object, "sf_column")]
  }
  # find if intercept exists in the formula
  if (attr(varterms, "intercept") == 1) {
    intercept <- TRUE
  } else {
    intercept <- FALSE
  }
  # find if response exists in the formula
  if (attr(varterms, "response") == 1) {
    response <- allvars[1]
  } else {
    response <- NULL
  }
  # make a vector of names
  varnames <- c(response, varlabels)
  # make a list where names are split if they interact
  varnames_split <- strsplit(varnames, ":")
  # giving the list names
  names(varnames_split) <- varnames
  # this will be used if onlyshow is removed from summary
  if (missing(onlyshow)) {
    onlyshow <- NULL
  }
  # storing the output list
  formlist <- list(
    varterms = varterms,
    allvars = allvars,
    varlabels = varlabels,
    intercept = intercept,
    response = response,
    varnames = varnames,
    varnames_split = varnames_split,
    onlyshow = onlyshow
  )
}

#' Build a data frame/sf object of the variables named in a parsed formula
#'
#' Given the parsed formula information from \code{make_formlist}, this
#' function extracts (and, for interaction terms, combines via
#' \code{interaction()}) the relevant columns of \code{object} so that
#' downstream summary/plot code can operate on a single object containing
#' just the variables of interest.
#'
#' @param object The \code{sf} object (or data frame) supplied by the user.
#'
#' @param formlist The list returned by \code{make_formlist}.
#'
#' @return An object of the same general type as \code{object} (with
#'   geometry re-attached if \code{object} is \code{sf}) containing one
#'   column per variable/interaction named in \code{formlist$varnames_split}.
#'   If the formula has an intercept only (no response, no right-hand-side
#'   variables), \code{object} is returned unchanged.
#'
#' @noRd
make_varsf <- function(object, formlist) {
  # can possibly deprecate this in the future by making use of
  # model.frame and extracting the main effects and using them to make the interactions
  # only real advantage will be creating variables for use mid formula with numeric variables


  if (formlist$intercept && is.null(formlist$response) && length(formlist$varlabels) == 0) {
    return(object)
  } else {
    # store geometry
    if ("sf" %in% class(object)) {
      object_geometry <- st_geometry(object)
      object_df <- st_drop_geometry(object)
    } else {
      object_df <- object
    }

    formlist <- lapply(
      formlist$varnames_split,
      function(x) {
        if (length(x) == 1 && (is.numeric(object_df[[x]]) || is.list(object_df[[x]]))) {
          return(object_df[, x, drop = FALSE]) # return numeric if provided
        } else if (length(x) > 1 && any(vapply(x, function(y) is.list(object_df[[y]]), logical(1)))) {
          stop("summarizing list-columns interacted with other variables not supported")
        } else {
          return(interaction(object_df[, x, drop = FALSE], sep = ":")) # return factors
        }
      }
    )
    varsf <- as.data.frame(formlist, optional = TRUE) # without optional the : in name gets
    # converted to synctactic name with .
    if ("sf" %in% class(object)) {
      varsf <- st_as_sf(varsf, geometry = object_geometry)
    }
    return(varsf)
  }
}

#' Count the total number of levels across the variables in a formula
#'
#' Numeric variables and the response variable contribute 0 levels;
#' categorical (non-numeric) right-hand-side variables contribute one level
#' per unique, non-missing value. This total is used to size the number of
#' separate summaries/plots that must be produced.
#'
#' @param formlist The list returned by \code{make_formlist}.
#'
#' @param varsf The object returned by \code{make_varsf}.
#'
#' @return A single numeric value giving the total level count.
#'
#' @noRd
get_varlevels <- function(formlist, varsf) {
  varsf_nogeom <- st_drop_geometry(varsf)
  levels <- lapply(formlist$varnames, function(x) {
    if (is.numeric(varsf[[x]]) || (x %in% formlist$response)) { # & !(x %in% formlist$varlabels))) {
      levels <- 0
    } else {
      levels <- length(na.omit(unique(varsf[[x]])))
    }
  })
  levels <- sum(unlist(levels))
}

#' Re-order user-supplied per-level graphical arguments to match variable levels
#'
#' \code{varlevel_args} lets users pass \code{plot()} arguments (e.g. colors)
#' keyed by the levels of a categorical variable. Because those arguments can
#' be supplied in any order, this function merges them against the variable's
#' actual level values (row-by-row, via the original row order) so each
#' argument value lines up with the correct observation.
#'
#' @param varsf The object returned by \code{make_varsf}.
#'
#' @param varlevel_args A named list (one element per variable) of named
#'   lists/vectors of graphical parameters keyed by level.
#'
#' @return A named list, parallel to \code{varlevel_args}, where each element
#'   is a list of graphical parameter vectors reordered to match the row
#'   order of \code{varsf}.
#'
#' @noRd
make_varlevel_args_list <- function(varsf, varlevel_args) {
  varlevel_args_list <- lapply(names(varlevel_args), function(x) {
    vardf <- st_drop_geometry(varsf[x])
    vardf[[x]] <- as.character(vardf[[x]])
    colnames(vardf) <- "levels"
    vardf$index <- 1:nrow(vardf)
    varlevel_args_df <- as.data.frame(varlevel_args[[x]], stringsAsFactors = FALSE)
    varlevel_args_df <- merge(vardf, varlevel_args_df)
    varlevel_args_df <- varlevel_args_df[order(varlevel_args_df$index), , drop = FALSE]
    badcol <- which(colnames(varlevel_args_df) %in% c("levels", "index"))
    varlevel_args_df <- varlevel_args_df[, -badcol, drop = FALSE]
    varlevel_args_listval <- as.list(varlevel_args_df)
  })
  names(varlevel_args_list) <- names(varlevel_args)
  varlevel_args_list
}

#' Re-order user-supplied variable-level graphical arguments to match levels
#'
#' Analogous to \code{make_varlevel_args_list}, but for the \code{var_args}
#' argument, which nests per-level graphical parameters one level deeper
#' (keyed first by variable, then by level within that variable).
#'
#' @param varsf The object returned by \code{make_varsf}.
#'
#' @param var_args A named list (one element per variable) of named lists,
#'   each of which maps levels of that variable to graphical parameters.
#'
#' @return A named list, parallel to \code{var_args}, with each nested list
#'   of graphical parameter vectors reordered to match the row order of
#'   \code{varsf}.
#'
#' @noRd
make_var_args_list <- function(varsf, var_args) {
  var_args_list <- lapply(names(var_args), function(x) {
    var_args_listsub <- lapply(names(var_args[[x]]), function(y) {
      vardf <- st_drop_geometry(varsf[y])
      vardf[[y]] <- as.character(vardf[[y]])
      colnames(vardf) <- "levels"
      vardf$index <- 1:nrow(vardf)
      var_args_df <- as.data.frame(var_args[[x]][[y]], stringsAsFactors = FALSE)
      var_args_df <- merge(vardf, var_args_df)
      var_args_df <- var_args_df[order(var_args_df$index), , drop = FALSE]
      badcol <- which(colnames(var_args_df) %in% c("levels", "index"))
      var_args_df <- var_args_df[, -badcol, drop = FALSE]
      var_args_listsubval <- as.list(var_args_df)
    })
    names(var_args_listsub) <- names(var_args[[x]])
    var_args_listsub
  })
  names(var_args_list) <- names(var_args)
  var_args_list
}

#' Confirm the right-hand-side variable of a formula is categorical
#'
#' Some summaries/plots (e.g. two-sided formulas summarizing a continuous
#' left-hand-side variable by a categorical right-hand-side variable) require
#' the right-hand-side variable to be non-numeric. This function throws an
#' informative error if it is not.
#'
#' @param varsf The object returned by \code{make_varsf}.
#'
#' @param formlist The list returned by \code{make_formlist}.
#'
#' @return \code{NULL}, invisibly, if the right-hand-side variable is
#'   categorical; otherwise execution stops with an error.
#'
#' @noRd
check_rhs_cat <- function(varsf, formlist) {
  any_numeric <- vapply(varsf[[formlist$varlabels]], is.numeric(), logical(1))
  if (any_numeric) {
    stop("Right hand side of formula must only contain categorical variables")
  }
}

#' Fill in missing graphical parameters with sf's geometry-type defaults
#'
#' \code{sf}'s \code{plot()} method chooses default point/line/polygon
#' graphical parameters (\code{pch}, \code{col}, \code{type}, etc.) based on
#' geometry type. This function looks up those same defaults (one row per
#' geometry type in \code{varsf}) and substitutes them in for any \code{NA}
#' values in \code{list_args} so spsurvey's plots match sf's default look
#' unless the user has explicitly overridden a parameter.
#'
#' @param varsf An \code{sf} object whose geometry type(s) determine the
#'   defaults to use.
#'
#' @param list_args A named list of graphical parameters, some of whose
#'   values may be \code{NA} placeholders to be filled with defaults.
#'
#' @return \code{list_args}, with \code{NA} values replaced by the
#'   corresponding sf geometry-type default where applicable.
#'
#' @noRd
match_sf_defaults <- function(varsf, list_args) {
  sf_default_df <- data.frame(
    geometry = c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"),
    pch = c(1, 1, 1, 1, NA, NA),
    cex = c(1, 1, NA, NA, 1, 1),
    col = c(1, 1, 1, 1, NA, NA),
    bg = c(0, 0, NA, NA, NA, NA),
    lwd = c(1, 1, 1, 1, 1, 1),
    lty = c(1, 1, 1, 1, 1, 1),
    type = c("p", "p", "l", "l", NA, NA),
    border = c(NA, NA, NA, NA, 1, 1),
    rule = c(NA, NA, NA, NA, "evenodd", "evenodd"),
    stringsAsFactors = FALSE
  )
  sf_defaults <- merge(data.frame(geometry = as.character(st_geometry_type(varsf))), sf_default_df, sort = FALSE)
  # sf_defaults <- lapply(st_geometry_type(varsf), get_sf_defaults)
  # sf_defaults <- do.call("rbind", sf_defaults)


  names_list_args <- names(list_args)
  list_args <- lapply(names_list_args, function(x) {
    if (x %in% names(sf_defaults) && any(is.na(list_args[[x]]))) {
      list_args[[x]] <- ifelse(is.na(list_args[[x]]), sf_defaults[[x]], list_args[[x]])
    } else {
      list_args[[x]] <- list_args[[x]]
    }
  })
  names(list_args) <- names_list_args
  list_args
}

#' Custom questions asked by devtools::release() before a CRAN submission
#'
#' Used with devtools::release().
#'
#' @return A character vector of questions to ask before release.
#'
#' @noRd
release_questions <- function() {
  c(
    "Have you changed version numbers in DESCRIPTION"
  )
}

