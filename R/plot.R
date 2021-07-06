###############################################################################
# Function: plot (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Plot sample frame and design objects
#'
#' @description Plot sample frames and design objects.
#'
#' This function is largely built on \code{plot.sf()}, and all spsurvey plotting 
#' methods can supply additional arguments to \code{plot.sf()}. For more information, 
#' run \code{vignette(plotting, package = "spsurvey"}. For more information on
#' plotting in \code{sf}, run \code{?plot.sf()}.
#'
#' @param x object of class \code{sframe}, \code{spdesign}, or \code{dframe}.
#'
#' @param y ignored if \code{x} has class \code{sframe} or \code{dframe}; an 
#' object of class \code{sframe} if \code{x} has class \code{spdesign}.
#'
#' @param formula A formula. Left hand side variables can be numeric or
#' categorical (or factor) and right hand side variables can be categorical
#' (or factor). Right hand side variables that are numeric will be coerced to
#' a categorical (or factor) variable. If an intercept is included in the right
#' hand side formula, the total will also be summarized.
#'
#' @param siteuse A character vector of site types to include when plotting a
#' design object. Can only take on values \code{"sframe"} (sample frame),
#' \code{"Legacy"} (for legacy sites), \code{"Base"} (for base sites),
#' \code{"Over"} (for \code{n_over} replacement sites), and \code{"Near"}
#' (for \code{n_near} replacement sites). The order of sites represents the
#' layering in the plot (e.g. \code{siteuse = c("Base", "Legacy")} will plot
#' legacy sites on top of base sites. Defaults to all non-\code{NULL} elements
#' in \code{x} and \code{y} with plot order \code{"sframe"}, \code{"Near"},
#' \code{"Over"}, \code{"Base"}, \code{"Legacy"}.
#'
#' @param var_args A named list. The name of each list corresponds to a
#' right hand side variable in \code{formula}. Values in the list are composed of
#' graphical arguments that are to be passed to \strong{every} level of the
#' variable.
#'
#' @param varlevel_args A named list. The name of each list corresponds to a
#' right hand side variable in \code{formula}. The first name of each sublist
#' should be \code{"levels"} and contain all levels of the variable. Subsequent
#' names correspond to graphical arguments that are to be passed to
#' the specified levels (in order). Values must be specified
#' for every level of each graphical argument, but applicable \code{sf} defaults
#' will be matched by inputting the value \code{NA}.
#'
#' @param geom Should separate geometries for each level of the right hand
#' side \code{formula} variables be plotted? Defaults to \code{FALSE}.
#'
#' @param onlyshow A string indicating the level of the single right hand side
#' variable for which a summary is requested.
#'
#' @param fix_bbox Should the geometry bounding box be fixed across plots?
#' Defaults to \code{TRUE}. If \code{TRUE}, the bounding box will be the largest
#' possible given \code{x} and \code{y}.
#'
#' @param xcoord Name of x (east-west)-coordinate in \code{x} (only required if
#' \code{x} is not an sf object)
#'
#' @param ycoord Name of y (north-south)-coordinate in \code{x} (only required if
#' \code{x} is not an sf object)
#'
#' @param crs Projection code for \code{xcoord} and \code{ycoord} (only
#' required if \code{x} is not an sf object)
#'
#' @param ... Additional arguments to pass to \code{plot.sf()}.
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @name plot
#'
#' @method plot sframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot(NE_Lakes)
#' plot(NE_Lakes, formula = ~ELEVATION_CAT)
#' plot(NE_Lakes, formula = AREA_HA ~ ELEVATION_CAT)
#' sample <- grts(NE_Lakes, 30)
#' plot(sample, NE_Lakes)
#' }
#' ###############################################################################
plot.sframe <- function(x, y, formula = ~1, var_args = NULL, varlevel_args = NULL,
                        geom = FALSE, onlyshow = NULL, fix_bbox = TRUE, ...) {

  # setting old graphical parameter value
  oldpar <- par()
  # setting exit handler
  on.exit(par(ask = oldpar$ask), add = TRUE)

  # storing dotlist
  dot_list <- list(...)
  if (fix_bbox) {
    dot_list$xlim <- st_bbox(x)[c(1, 3)]
    dot_list$ylim <- st_bbox(x)[c(2, 4)]
  }


  # making variable list
  formlist <- make_formlist(formula, onlyshow, x)
  varsf <- make_varsf(x, formlist)
  # varsf <- na.omit(varsf)

  # plot geometry or response for ~ 1
  if (length(formlist$varlabels) == 0 && formlist$intercept) {
    if (is.null(formlist$response)) {
      if (!("main" %in% names(dot_list))) {
        dot_list$main <- paste(expression("~"), " ", "1", sep = "")
      }
      sfplot <- do.call("plot", c(list(st_geometry(x)), dot_list))
    } else {
      if (!("main" %in% names(dot_list))) {
        dot_list$main <- paste(formlist$response, " ", expression("~"), " ", "1", sep = "")
      }
      sfplot <- do.call("plot", c(list(varsf[formlist$response]), dot_list))
    }
  } else {
    if (is.null(formlist$response)) {
      # get varlevel_args list
      if (!is.null(varlevel_args)) {
        varlevel_args_list <- make_varlevel_args_list(varsf, varlevel_args)
      } else {
        varlevel_args_list <- varlevel_args
      }

      if (is.null(onlyshow)) {
        if (geom) {
          # turning on ask if necessary
          if (get_varlevels(formlist, varsf) > 1) {
            par(ask = TRUE)
          }
          sfplot <- lapply(formlist$varlabels, function(a) {
            varsf_split <- split(varsf[a], varsf[[a]])
            names_varsf_split <- names(varsf_split)
            varlevel_args_split <- split(as.data.frame(varlevel_args_list[[a]], stringsAsFactors = FALSE), varsf[[a]])
            lapply(names_varsf_split, function(b) {
              list_args <- c(var_args[[a]], varlevel_args_split[[b]], dot_list)
              if (!("main" %in% names(list_args))) {
                list_args$main <- paste(formlist$response, " ", expression("~"), " ", a, " (", b, ")", sep = "")
              }
              if (any(is.na(unlist(list_args)))) {
                list_args <- match_sf_defaults(varsf_split[[b]], list_args)
              }
              do.call("plot", c(list(st_geometry(varsf_split[[b]])), list_args))
            })
          })
          names(sfplot) <- formlist$varlabels
        } else {
          # turning on ask if necessary
          if (length(formlist$varlabels) > 1) {
            par(ask = TRUE)
          }
          sfplot <- lapply(formlist$varlabels, function(a) {
            list_args <- c(var_args[[a]], varlevel_args_list[[a]], dot_list)
            if (!("main" %in% names(list_args))) {
              list_args$main <- paste(" ", expression("~"), " ", a, sep = "")
            }
            if (any(is.na(unlist(list_args)))) {
              list_args <- match_sf_defaults(varsf[a], list_args)
            }
            do.call("plot", c(list(varsf[a]), list_args))
          })
          names(sfplot) <- formlist$varlabels
        }
      } else {
        varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$onlyshow, ]
        if (geom) {
          if (!("main" %in% names(dot_list))) {
            dot_list$main <- paste(" ", expression("~"), " ", formlist$varlabels, " (", formlist$onlyshow, ")", sep = "")
          }
          sfplot <- do.call("plot", c(list(st_geometry(varsf_sub[formlist$varlabels])), dot_list))
        } else {
          if (!("main" %in% names(dot_list))) {
            dot_list$main <- paste(" ", expression("~"), " ", formlist$varlabels, sep = "")
          }
          sfplot <- do.call("plot", c(list(varsf_sub[formlist$varlabels]), dot_list))
        }
      }
    } else {
      # get varlevel_args list
      if (!is.null(varlevel_args)) {
        varlevel_args_list <- make_varlevel_args_list(varsf, varlevel_args)
      } else {
        varlevel_args_list <- varlevel_args
      }

      if (is.null(onlyshow)) {
        if (get_varlevels(formlist, varsf) > 1) {
          par(ask = TRUE)
        }

        if (is.numeric(varsf[[formlist$response]])) {
          sfplot <- lapply(formlist$varlabels, function(a) {
            varsf_split <- split(varsf[, c(formlist$response, a)], varsf[[a]])
            names_varsf_split <- names(varsf_split)
            varlevel_args_split <- split(as.data.frame(varlevel_args_list[[a]], stringsAsFactors = FALSE), varsf[[a]])
            lapply(names_varsf_split, function(b) {
              list_args <- c(var_args[[a]], varlevel_args_split[[b]], dot_list)
              if (!("main" %in% names(list_args))) {
                list_args$main <- paste(formlist$response, " ", expression("~"), " ", a, " (", b, ")", sep = "")
              }
              if (any(is.na(unlist(list_args)))) {
                list_args <- match_sf_defaults(varsf_split[[b]], list_args)
              }
              sfplot <- do.call("plot", c(list(varsf_split[[b]][formlist$response]), list_args))
              sfplot <- list(sfplot)
              names(sfplot) <- b
              sfplot
            })
          })
          names(sfplot) <- formlist$varlabels
        } else {
          if (!is.null(var_args)) {
            var_args_list <- make_var_args_list(varsf, var_args)
          } else {
            var_args_list <- NULL
          }
          sfplot <- lapply(formlist$varlabels, function(a) {
            varsf_split <- split(varsf[, c(formlist$response, a)], varsf[[a]])
            names_varsf_split <- names(varsf_split)
            varlevel_args_split <- split(as.data.frame(varlevel_args_list[[a]], stringsAsFactors = FALSE), varsf[[a]])
            var_args_split <- split(as.data.frame(var_args_list[[a]][[formlist$response]], stringsAsFactors = FALSE), varsf[[a]])
            lapply(names_varsf_split, function(b) {
              list_args <- c(var_args_split[[b]], varlevel_args_split[[b]], dot_list)
              if (!("main" %in% names(list_args))) {
                list_args$main <- paste(formlist$response, " ", expression("~"), " ", a, " (", b, ")", sep = "")
              }
              if (any(is.na(unlist(list_args)))) {
                list_args <- match_sf_defaults(varsf_split[[b]], list_args)
              }
              sfplot <- do.call("plot", c(list(varsf_split[[b]][formlist$response]), list_args))
              sfplot <- list(sfplot)
              names(sfplot) <- b
              sfplot
            })
          })
          names(sfplot) <- formlist$varlabels
        }
      } else {
        varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$onlyshow, ]
        if (!is.null(var_args)) {
          var_args_list <- make_var_args_list(varsf, var_args)
          var_args_split <- split(as.data.frame(var_args_list[[formlist$varlabels]][[formlist$response]],
            stringsAsFactors = FALSE
          ), varsf[[formlist$varlabels]])
          var_args_split <- var_args_split[[formlist$onlyshow]]
        } else {
          var_args_list <- NULL
          var_args_split <- NULL
        }
        if (!("main" %in% names(dot_list))) {
          dot_list$main <- paste(formlist$response, " ", expression("~"), " ", formlist$varlabels, " (", formlist$onlyshow, ")", sep = "")
        }
        sfplot <- do.call("plot", c(list(varsf_sub[formlist$response]), var_args_split, dot_list))
      }
    }
  }
  invisible(sfplot)
}

#' @name plot
#'
#' @method plot spdesign
#'
#' @export
plot.spdesign <- function(x, y = NULL, formula = ~siteuse, siteuse = NULL,
                        var_args = NULL, varlevel_args = NULL, geom = FALSE, onlyshow = NULL,
                        fix_bbox = TRUE, ...) {

  if ((is.null(siteuse) & (!is.null(x$sites_near))) | "Near" %in% siteuse) {
    x$sites_near$siteuse <- "Near"
  }
  # bind
  x <- sprbind(x)
  # make formlists
  formlist_x <- make_formlist(formula, onlyshow, x)
  # make sframe
  varsf_x <- make_varsf(x, formlist_x)

  if (!is.null(y)) {
    y$siteuse <- "sframe"
    # make formlists
    formlist_y <- make_formlist(formula, onlyshow, y)
    # make sframe
    varsf_y <- make_varsf(y, formlist_y)
  } else {
    varsf_y <- NULL
  }

  new_varsf <- rbind(varsf_x, varsf_y)

  if (is.null(siteuse)) {
    fac_levels <- c("sframe", "Near", "Over", "Base", "Legacy")
    fac_levels_used <- fac_levels[fac_levels %in% unique(new_varsf$siteuse)]
    new_varsf$siteuse <- factor(new_varsf$siteuse, levels = fac_levels_used)
  } else {
    new_varsf$siteuse <- factor(new_varsf$siteuse, levels = siteuse)
  }

  # arrange by factor level
  ordered_varsf <- with(new_varsf, new_varsf[order(new_varsf$siteuse), , drop = FALSE])

  # make the plot
  plot.sframe(x = ordered_varsf, formula = formula, var_args = var_args, varlevel_args = varlevel_args, geom = geom, onlyshow = onlyshow, fix_bbox = fix_bbox, ...)
}

#' @name plot
#'
#' @method plot dframe
#'
#' @export
plot.dframe <- function(x, y = NULL, formula = ~1, var_args = NULL, varlevel_args = NULL,
                        geom = FALSE, onlyshow = NULL, fix_bbox = TRUE, xcoord, ycoord, crs, ...) {
  # coerce to sf
  if (!inherits(x, "sf")) {
    x <- st_as_sf(x, coords = c(xcoord, ycoord), crs = crs)
  }
  plot.sframe(x = x, formula = formula, var_args = var_args, varlevel_args = varlevel_args, geom = geom, onlyshow = onlyshow, fix_bbox = fix_bbox, ...)
}
