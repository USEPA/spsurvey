#' Plot sample frame and design objects
#' 
#' @description Plot sample frames and design objects. This function is
#' largely built on `plot.sf()`, and all `spsurvey` plotting methods can
#' supply additional arguments to `plot.sf()`. For more information
#' regarding `plot.sf()`, run `?plot.sf()`.
#'
#' @param x object of class "sframe" or "design"
#' @param y ignored if `x` has class "sframe"; an object of class "sframe"
#' if `x` has class "design"
#' @param formula A formula. Left hand side variables can be numeric or
#' categorical (or factor) and right hand side variables can be categorical
#' (or factor). Right hand side variables that are numeric will be coerced to
#' a categorical (or factor) variable. If an intercept is included in the right
#' hand side formula, the total will also be summarized.
#' @param variable_args A named list. The name of each list corresponds to a
#' right hand side variable in `formula`. Values in the list are composed of 
#' graphical arguments that are to be passed to \strong{every} level of the
#' variable.
#' @param level_args A named list. The name of each list corresponds to a
#' right hand side variable in `formula`. The first name of each sublist 
#' should be "levels" and contain all levels of the variable. Subsequent
#' names correspond to graphical arguments that are to be passed to
#' the specified levels (in order). Values must be specified
#' for every level of each graphical argument, but applicable `sf` defaults
#' will be matched by inputting the value `NA`
#' @param geom Should separate geometries for each level of the right hand
#' side `formula` variables be plotted? Defaults to `FALSE`
#' @param onlyshow A string indicating the level of the single right hand side
#' variable for which a summary is requested.
#' @param fix_bbox Should the geometry bounding box be fixed across plots? 
#' Defaults to `TRUE`. If `TRUE`, the bounding box will be the largest
#' possible given `x` and `y`.
#' @showlegacy Should legacy sites be plotted separately from `sites_base`?
#' Defaults to `FALSE`.
#' @param ... Additional arguments to pass to `plot.sf()`. 
#' 
#' @name plot
#' @method plot sframe
#' @return
#' @export
#'
#' @examples
plot.sframe <- function(x, y, formula = ~ 1, variable_args = NULL, level_args = NULL,
                        geom = FALSE, onlyshow = NULL, fix_bbox = TRUE, ...) {
  
  # setting old graphical parameter value
  oldpar <- par()
  
  # storing dotlist
  dot_list <- list(...)
  if (fix_bbox) {
    dot_list$xlim <- st_bbox(x)[c(1, 3)]
    dot_list$ylim <- st_bbox(x)[c(2, 4)]    
  }
  
  
  # making variable list
  formlist <- make_formlist(formula, onlyshow)
  varsf <- make_varsf(x, formlist)
  varsf <- na.omit(varsf)
  
  # plot geometry or response for ~ 1
  if (length(formlist$varlabels) == 0 && formlist$intercept) {
    if (is.null(formlist$response)) {
      # if (!("main" %in% names(dot_list))) {
      #   dot_list$main <- paste(expression("~"), " ", "1", sep = "")
      # }
      return(invisible(do.call("plot", c(list(st_geometry(x)), dot_list))))
    } else {
      if (!("main" %in% names(dot_list))) {
        dot_list$main <- paste(formlist$response, " ", expression("~"), " ", "1", sep = "")
      }
      return(invisible(do.call("plot", c(list(varsf[formlist$response]), dot_list))))
    }
  }
  
  if (is.null(formlist$response)) {
    # get level_args list
    if (!is.null(level_args)) {
      level_args_list <- make_level_args_list(varsf, level_args)
    } else {
      level_args_list <- level_args
    }
    
    if (is.null(onlyshow)) {
      if (geom) {
        # turning on ask if necessary
        if (get_varlevels(formlist, varsf) > 1) {
          par(ask = TRUE)
        }
        lapply(formlist$varlabels, function(a) {
          varsf_split <- split(varsf[a], varsf[[a]])
          names_varsf_split <- names(varsf_split)
          level_args_split <- split(as.data.frame(level_args_list[[a]], stringsAsFactors = FALSE), varsf[[a]])
          lapply(names_varsf_split, function(b) {
            list_args <- c(variable_args[[a]], level_args_split[[b]], dot_list)
            list_args$main <- paste(formlist$response, " ", expression("~"), " ", a, " (", b, ")", sep = "")
            if (any(is.na(unlist(list_args)))) {
              list_args <- match_sf_defaults(varsf_split[[b]], list_args)
            }
            do.call("plot", c(list(st_geometry(varsf_split[[b]])), list_args))
          }
          )
        }
        )
      } else {
        # turning on ask if necessary
        if (length(formlist$varlabels) > 1) {
          par(ask = TRUE)
        }
        lapply(formlist$varlabels, function(a) {
          if (!("main" %in% names(variable_args[[a]]))) {
            variable_args[[a]]$main <- paste(" ", expression("~"), " ", a, sep = "")
          }
          list_args <- c(variable_args[[a]], level_args_list[[a]], dot_list)
          if (any(is.na(unlist(list_args)))) {
            list_args <- match_sf_defaults(varsf[a], list_args)
          }
          do.call("plot", c(list(varsf[a]), list_args))
        }
        )
      }
    } else {
      varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$onlyshow, ]
      if (geom) {
        dot_list$main <- paste(" ", expression("~"), " ", formlist$varlabels, " (", formlist$onlyshow, ")", sep = "")
        do.call("plot", c(list(st_geometry(varsf_sub[formlist$varlabels])), dot_list))
      } else {
        if (!("main" %in% names(dot_list))) {
          dot_list$main <- paste(" ", expression("~"), " ", formlist$varlabels, sep = "")
        }
        do.call("plot", c(list(varsf_sub[formlist$varlabels]), dot_list))
      }
    }
  } else {
    # get level_args list
    if (!is.null(level_args)) {
      level_args_list <- make_level_args_list(varsf, level_args)
    } else {
      level_args_list <- level_args
    }
    
    if (is.null(onlyshow)) {
      if (get_varlevels(formlist, varsf) > 1) {
        par(ask = TRUE)
      }
      
      if (is.numeric(varsf[[formlist$response]])) {
        lapply(formlist$varlabels, function(a) {
          varsf_split <- split(varsf[, c(formlist$response, a)], varsf[[a]])
          names_varsf_split <- names(varsf_split)
          level_args_split <- split(as.data.frame(level_args_list[[a]], stringsAsFactors = FALSE), varsf[[a]])
          lapply(names_varsf_split, function(b) {
            list_args <- c(variable_args[[a]], level_args_split[[b]], dot_list)
            list_args$main <- paste(formlist$response, " ", expression("~"), " ", a, " (", b, ")", sep = "")
            if (any(is.na(unlist(list_args)))) {
              list_args <- match_sf_defaults(varsf_split[[b]], list_args)
            }
            do.call("plot", c(list(varsf_split[[b]][formlist$response]), list_args))
          }
          )
        }
        )
      } else {
        if (!is.null(variable_args)) {
          variable_args_list <- make_variable_args_list(varsf, variable_args)
        } else {
          variable_args_list <- NULL
        }
        lapply(formlist$varlabels, function(a) {
          varsf_split <- split(varsf[, c(formlist$response, a)], varsf[[a]])
          names_varsf_split <- names(varsf_split)
          level_args_split <- split(as.data.frame(level_args_list[[a]], stringsAsFactors = FALSE), varsf[[a]])
          variable_args_split <- split(as.data.frame(variable_args_list[[a]][[formlist$response]], stringsAsFactors = FALSE), varsf[[a]])
          lapply(names_varsf_split, function(b) {
            list_args <- c(variable_args_split[[b]], level_args_split[[b]], dot_list)
            list_args$main <- paste(formlist$response, " ", expression("~"), " ", a, " (", b, ")", sep = "")
            if (any(is.na(unlist(list_args)))) {
              list_args <- match_sf_defaults(varsf_split[[b]], list_args)
            }
            do.call("plot", c(list(varsf_split[[b]][formlist$response]), list_args))
          }
          )
        }
        )
      }
    } else {
      varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$onlyshow, ]
      if (!is.null(variable_args)) {
        variable_args_list <- make_variable_args_list(varsf, variable_args)
        variable_args_split <- split(as.data.frame(variable_args_list[[formlist$varlabels]][[formlist$response]],
                                                   stringsAsFactors = FALSE), varsf[[formlist$varlabels]])
        variable_args_split <- variable_args_split[[formlist$onlyshow]]
      } else {
        variable_args_list <- NULL
        variable_args_split <- NULL
      }
      if (!("main" %in% names(dot_list))) {
        dot_list$main <- paste(formlist$response, " ", expression("~"), " ", formlist$varlabels, " (", formlist$onlyshow, ")", sep = "")
      }
      do.call("plot", c(list(varsf_sub[formlist$response]), variable_args_split, dot_list))
    }
  }
  on.exit(par(ask = oldpar$ask))
}

#' @name plot
#' @method plot design
#' @export
plot.design <- function(x, y = NULL, formula = ~ sites, sites = NULL, 
                          variable_args = NULL, level_args = NULL, geom = FALSE, onlyshow = NULL,
                          fix_bbox = TRUE, showlegacy = FALSE, ...) {

  # y is sframe
  x <- c(list(sframe = y), x)
  if (is.null(sites)) {
    sites <- names(x[!vapply(x, is.null, logical(1))])
  }
  sites <- sites[sites != "dsgn"] # remove dsgn if somehow provided
  x <- x[sites]
  x_names <- names(x)
  x <- lapply(x_names, function(a) merge(x[[a]], data.frame(sites = a)))
  names(x) <- x_names
  if (showlegacy && "sites_base" %in% x_names) {
    levels(x$sites_base$sites) <-  c("sites_base", "legacy")
    x$sites_base$sites[!is.na(x$sites_base$legacy)] <- "legacy"
  }
  # make formlists
  formlist <- make_formlist(formula, onlyshow)
  # make sframe
  varsfs <- lapply(x, function(a) make_varsf(a, formlist))
  x <- do.call("rbind", varsfs)
  plot.sframe(x = x, formula = formula, variable_args = variable_args, level_args = level_args, geom = geom, onlyshow = onlyshow, fix_bbox = fix_bbox, ...)
}
