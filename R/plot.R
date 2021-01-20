plot.sframe <- function(object, formula = ~ 1, variable_args = NULL, level_args = NULL,
                        geom = FALSE, onlyshow = NULL, fix_bbox = TRUE, ...) {
  
  # setting old graphical parameter value
  oldpar <- par()
  
  # storing dotlist
  dot_list <- list(...)
  if (fix_bbox) {
    dot_list$xlim <- st_bbox(object)[c(1, 3)]
    dot_list$ylim <- st_bbox(object)[c(2, 4)]    
  }
  
  
  # making variable list
  formlist <- make_formlist(formula, onlyshow)
  varsf <- make_varsf(object, formlist)
  varsf <- na.omit(varsf)
  
  # plot geometry or response for ~ 1
  if (length(formlist$varlabels) == 0 && formlist$intercept) {
    if (is.null(formlist$response)) {
      # if (!("main" %in% names(dot_list))) {
      #   dot_list$main <- paste(expression("~"), " ", "1", sep = "")
      # }
      return(invisible(do.call("plot", c(list(st_geometry(object)), dot_list))))
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
        lapply(formlist$varlabels, function(x) {
          varsf_split <- split(varsf[x], varsf[[x]])
          names_varsf_split <- names(varsf_split)
          level_args_split <- split(as.data.frame(level_args_list[[x]], stringsAsFactors = FALSE), varsf[[x]])
          lapply(names_varsf_split, function(y) {
            list_args <- c(variable_args[[x]], level_args_split[[y]], dot_list)
            list_args$main <- paste(formlist$response, " ", expression("~"), " ", x, " (", y, ")", sep = "")
            list_args <- match_sf_defaults(varsf_split[[y]], list_args)
            do.call("plot", c(list(st_geometry(varsf_split[[y]])), list_args))
          }
          )
        }
        )
      } else {
        # turning on ask if necessary
        if (length(formlist$varlabels) > 1) {
          par(ask = TRUE)
        }
        lapply(formlist$varlabels, function(x) {
          if (!("main" %in% names(variable_args[[x]]))) {
            variable_args[[x]]$main <- paste(" ", expression("~"), " ", x, sep = "")
          }
          list_args <- c(variable_args[[x]], level_args_list[[x]], dot_list)
          list_args <- match_sf_defaults(varsf[x], list_args)
          do.call("plot", c(list(varsf[x]), list_args))
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
        lapply(formlist$varlabels, function(x) {
          varsf_split <- split(varsf[, c(formlist$response, x)], varsf[[x]])
          names_varsf_split <- names(varsf_split)
          level_args_split <- split(as.data.frame(level_args_list[[x]], stringsAsFactors = FALSE), varsf[[x]])
          lapply(names_varsf_split, function(y) {
            list_args <- c(variable_args[[x]], level_args_split[[y]], dot_list)
            list_args$main <- paste(formlist$response, " ", expression("~"), " ", x, " (", y, ")", sep = "")
            list_args <- match_sf_defaults(varsf_split[[y]], list_args)
            do.call("plot", c(list(varsf_split[[y]][formlist$response]), list_args))
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
        lapply(formlist$varlabels, function(x) {
          varsf_split <- split(varsf[, c(formlist$response, x)], varsf[[x]])
          names_varsf_split <- names(varsf_split)
          level_args_split <- split(as.data.frame(level_args_list[[x]], stringsAsFactors = FALSE), varsf[[x]])
          variable_args_split <- split(as.data.frame(variable_args_list[[x]][[formlist$response]], stringsAsFactors = FALSE), varsf[[x]])
          lapply(names_varsf_split, function(y) {
            list_args <- c(variable_args_split[[y]], level_args_split[[y]], dot_list)
            list_args$main <- paste(formlist$response, " ", expression("~"), " ", x, " (", y, ")", sep = "")
            list_args <- match_sf_defaults(varsf_split[[y]], list_args)
            do.call("plot", c(list(varsf_split[[y]][formlist$response]), list_args))
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

# idea - set default sites equal to non null sites
# change variable order for variable_args and level_args
plot.spsurvey <- function(object, sframe = NULL, formula = ~ sites, sites = NULL, 
                          variable_args = NULL, level_args = NULL, geom = FALSE, onlyshow = NULL,
                          fix_bbox = TRUE, showlegacy = FALSE, ...) {

  object <- c(list(sframe = sframe), object)
  if (is.null(sites)) {
    sites <- names(object[!vapply(object, is.null, logical(1))])
  }
  sites <- sites[sites != "dsgn"] # remove dsgn if somehow provided
  object <- object[sites]
  object_names <- names(object)
  object <- lapply(object_names, function(x) merge(object[[x]], data.frame(sites = x)))
  names(object) <- object_names
  if (showlegacy && "sites.base" %in% object_names) {
    levels(object$sites.base$sites) <-  c("sites.base", "legacy")
    object$sites.base$sites[!is.na(object$sites.base$legacy)] <- "legacy"
  }
  # make formlists
  formlist <- make_formlist(formula, onlyshow)
  # make sframe
  varsfs <- lapply(object, function(x) make_varsf(x, formlist))
  object <- do.call("rbind", varsfs)
  plot.sframe(object, formula, variable_args, level_args, geom, onlyshow, fix_bbox, ...)
}

# plot.spsurvey <- function(object, sframe = NULL, sites = c("sframe", "sites.base"),
#                           vars = ~ sites, showonly = NULL, fix_bbox = TRUE, addargs, ...) {
# 
#   site_names <- sites
#   sites <- sites[sites != "legacy"]
#   object <- c(list(sframe = sframe), object)
#   object <- object[sites]
#   # removing null objects
#   object <- object[!vapply(object, is.null, logical(1))]
#   object_names <- names(object)
#   site_names <- site_names[site_names %in% c(object_names, "legacy")]
#   # append sframe if provided
#   
#   object <- lapply(object_names, function(x) merge(object[[x]], data.frame(sites = x)))
#   names(object) <- object_names
#   if ("sites.base" %in% object_names && any(!is.na(object$sites.base$legacy))) {
#     levels(object$sites.base$sites) <-  c("sites.base", "legacy")
#     object$sites.base$sites[!is.na(object$sites.base$legacy)] <- "legacy"
#   }
#   # make formlists
#   formlist <- make_formlist(vars, showonly)
# 
#   # make sframe
#   varsfs <- lapply(object, function(x) make_varsf(x, formlist))
#   object <- do.call("rbind", varsfs)
#   
#   if (!missing(addargs)) {
#     argslist <- data.frame(c(list(sites = site_names), addargs), stringsAsFactors = FALSE)
#     argsdf <- merge(argslist, object)[, names(addargs), drop = FALSE]
#     addargs <- as.list(argsdf)
#     plot.sframe(object, vars, showonly, fix_bbox, addargs = addargs, ...)
#   } else {
#     plot.sframe(object, vars, showonly, fix_bbox, ...)
#   }
# }