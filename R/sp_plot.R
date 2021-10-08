###############################################################################
# Function: sp_plot (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Plot sampling frames, design sites, and analysis data.
#'
#' This function plots sampling frames, design sites, and analysis data.
#' If the left-hand side of the formula is empty, plots
#' are of the distributions of the right-hand side variables. If the left-hand side
#' of the variable contains a variable, plots are of the left-hand size variable
#' for each level of each right-hand side variable.
#' This function is largely built on \code{plot.sf()}, and all spsurvey plotting
#' methods can supply additional arguments to \code{plot.sf()}. For more information on
#' plotting in \code{sf}, run \code{?sf::plot.sf()}.
#'
#' @param object An object to plot. When plotting sampling frames or analysis data,
#' a data frame or \code{sf} object. When plotting design sites, an object created by \code{grts()} or
#' \code{irs()} (which has class \code{spdesign}).
#'
#' @param sframe The sampling frame (an \code{sf} object) to plot alongside design
#' sites. This argument is only used when \code{object} corresponds to the design sites.
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
#' two-sided), the total will also be summarized. When plotting sampling frames
#' or analysis data, the default formula is \code{~ 1}. When plotting design sites,
#' \code{siteuse} should be used in the formula, and the default formula is
#' \code{~ siteuse}.
#'
#' @param siteuse A character vector of site types to include when plotting design sites.
#' It can only take on values \code{"sframe"} (sampling frame),
#' \code{"Legacy"} (for legacy sites), \code{"Base"} (for base sites),
#' \code{"Over"} (for \code{n_over} replacement sites), and \code{"Near"}
#' (for \code{n_near} replacement sites). The order of sites represents the
#' layering in the plot (e.g. \code{siteuse = c("Base", "Legacy")} will plot
#' legacy sites on top of base sites. Defaults to all non-\code{NULL} elements
#' in \code{x} and \code{y} with plot order \code{"sframe"}, \code{"Legacy"},
#' \code{"Base"}, \code{"Over"}, \code{"Near"}.
#'
#' @param var_args A named list. The name of each list corresponds to a
#' right-hand side variable in \code{formula}. Values in the list are composed of
#' graphical arguments that are to be passed to \strong{every} level of the
#' variable.
#'
#' @param varlevel_args A named list. The name of each list corresponds to a
#' right-hand side variable in \code{formula}. The first name of each sublist
#' should be \code{"levels"} and contain all levels of the variable. Subsequent
#' names correspond to graphical arguments that are to be passed to
#' the specified levels (in order). Values must be specified
#' for every level of each graphical argument, but applicable sf defaults
#' will be matched by inputting the value \code{NA}.
#'
#' @param geom Should separate geometries for each level of the right-hand
#' side \code{formula} variables be plotted? Defaults to \code{FALSE}.
#'
#' @param onlyshow A string indicating the single level of the single right-hand side
#' variable for which a summary is requested. This argument is only used when
#' a single right-hand side variable is provided.
#'
#' @param fix_bbox Should the geometry bounding box be fixed across plots?
#' Defaults to \code{TRUE}. If \code{TRUE}, the bounding box for each plot will be
#' unique to its coordinates.
#'
#' @param xcoord Name of the x-coordinate (east-west) in \code{object} (only required if
#' \code{object} is not an \code{sf} object)
#'
#' @param ycoord Name of y (north-south)-coordinate in \code{object} (only required if
#' \code{object} is not an \code{sf} object)
#'
#' @param crs Projection code for \code{xcoord} and \code{ycoord} (only
#' required if \code{object} is not an \code{sf} object)
#'
#' @param ... Additional arguments to pass to \code{plot.sf()}.
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @name sp_plot
#'
#' @export
#'
#' @examples
#' data("NE_Lakes")
#' sp_plot(NE_Lakes, formula = ~ELEV_CAT)
#' sample <- grts(NE_Lakes, 30)
#' sp_plot(sample, NE_Lakes)
#' data("NLA_PNW")
#' sp_plot(NLA_PNW, formula = ~BMMI)
sp_plot <- function(object, ...) {
  UseMethod("sp_plot", object)
}

#' @name sp_plot
#' @method sp_plot default
#' @export
sp_plot.default <- function(object, formula = ~1, xcoord, ycoord, crs,
                            var_args = NULL, varlevel_args = NULL,
                            geom = FALSE, onlyshow = NULL, fix_bbox = TRUE, ...) {

  # coerce to sf
  if (!inherits(object, "sf")) {
    object <- st_as_sf(object, coords = c(xcoord, ycoord), crs = crs)
  }

  # setting old graphical parameter value
  oldpar <- par()
  # setting exit handler
  on.exit(par(ask = oldpar$ask), add = TRUE)

  # storing dotlist
  dot_list <- list(...)
  if (fix_bbox) {
    dot_list$xlim <- st_bbox(object)[c(1, 3)]
    dot_list$ylim <- st_bbox(object)[c(2, 4)]
  }


  # making variable list
  formlist <- make_formlist(formula, onlyshow, object)
  varsf <- make_varsf(object, formlist)
  # varsf <- na.omit(varsf)

  # plot geometry or response for ~ 1
  if (length(formlist$varlabels) == 0 && formlist$intercept) {
    if (is.null(formlist$response)) {
      if (!("main" %in% names(dot_list))) {
        dot_list$main <- paste(expression("~"), " ", "1", sep = "")
      }
      sfplot <- do.call("plot", c(list(st_geometry(object)), dot_list))
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

#' @name sp_plot
#' @method sp_plot spdesign
#' @export
sp_plot.spdesign <- function(object, sframe = NULL, formula = ~siteuse, siteuse = NULL,
                             var_args = NULL, varlevel_args = NULL, geom = FALSE, onlyshow = NULL,
                             fix_bbox = TRUE, ...) {
  if ((is.null(siteuse) & (!is.null(object$sites_near))) | "Near" %in% siteuse) {
    object$sites_near$siteuse <- "Near"
  }

  # set siteuse when NULL
  if (is.null(siteuse)) {
    if (is.null(sframe)) {
      siteuse_sframe <- NULL
    } else {
      siteuse_sframe <- "sframe"
    }

    if (is.null(object$sites_legacy)) {
      siteuse_legacy <- NULL
    } else {
      siteuse_legacy <- "Legacy"
    }

    if (is.null(object$sites_base)) {
      siteuse_base <- NULL
    } else {
      siteuse_base <- "Base"
    }

    if (is.null(object$sites_over)) {
      siteuse_over <- NULL
    } else {
      siteuse_over <- "Over"
    }

    if (is.null(object$sites_near)) {
      siteuse_near <- NULL
    } else {
      siteuse_near <- "Near"
    }
    siteuse <- c(siteuse_sframe, siteuse_legacy, siteuse_base, siteuse_over, siteuse_near)
  }

  # bind
  siteuse_spr <- siteuse[!(siteuse %in% "sframe")]
  object <- sp_rbind(object, siteuse = siteuse_spr)
  # make formlists
  formlist_object <- make_formlist(formula, onlyshow, object)
  # make sframe
  varsf_object <- make_varsf(object, formlist_object)

  if (!is.null(sframe) & "sframe" %in% siteuse) {
    sframe$siteuse <- "sframe"
    # make formlists
    formlist_sframe <- make_formlist(formula, onlyshow, sframe)
    # make sframe
    varsf_sframe <- make_varsf(sframe, formlist_sframe)
  } else {
    varsf_sframe <- NULL
  }

  new_varsf <- rbind(varsf_object, varsf_sframe)
  # set as factor
  new_varsf$siteuse <- factor(new_varsf$siteuse, levels = siteuse)

  # arrange by factor level
  ordered_varsf <- with(new_varsf, new_varsf[order(new_varsf$siteuse), , drop = FALSE])

  # make the plot
  sp_plot.default(object = ordered_varsf, formula = formula, var_args = var_args, varlevel_args = varlevel_args, geom = geom, onlyshow = onlyshow, fix_bbox = fix_bbox, ...)
}
