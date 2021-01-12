plot.sframe <- function(object, formula = ~ 1, geom = FALSE, onlyshow = NULL,
                        fix_bbox = TRUE, variable_args = NULL, level_args = NULL, ...) {
  
  # storing dotlist
  dot_list <- list(...)
  if (fix_bbox) {
    dot_list$xlim <- st_bbox(object)[c(1, 3)]
    dot_list$ylim <- st_bbox(object)[c(2, 4)]    
  }

  
  # making variable list
  formlist <- make_formlist(formula, onlyshow)
  
  # plot geometry for ~ 1
  if (length(formlist$varnames) == 0 && formlist$intercept) {
    return(invisible(do.call("plot", c(list(st_geometry(object)), dot_list))))
  }
  
  # make varsf object
  varsf <- make_varsf(object, formlist)
  
  if (is.null(formlist$response)) {
    if (is.null(formlist$onlyshow)) {}
    return(invisible(plot_rhf(varsf, formlist, geom, onlyshow, fix_bbox, variable_args, level_args, dot_list)))
  } else {
    return(invisible(plot_twohf(varsf, formlist, geom, onlyshow, fix_bbox, variable_args, level_args, dot_list)))
  }
}

plot_rhf <- function(varsf, formlist, geom, onlyshow, fix_bbox, variable_args, level_args, dot_list) {
    # get level_args list
    if (!is.null(level_args)) {
      level_args_list <- make_level_args_list(varsf, level_args)
    } else {
      level_args_list <- level_args
    }
  
    if (is.null(onlyshow)) {
      # setting old graphical parameter value
      oldpar <- par()
      

      
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
            do.call("plot", c(list(varsf[x]), list_args))
          }
        )
      }
      on.exit(par(ask = oldpar$ask))
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
}
      
# plot_twohf <- function(varsf, formlist, geom, onlyshow, fix_bbox, variable_args, level_args, dot_list) {
#     
# 
#       lapply(
#         formlist$varnames,
#         function(x) {
#           dotlist$main <- paste(" ", expression("~"), " ", x, sep = "")
#           do.call("plot", c(list(varsf[x]), dotlist))
#         }
#       )
#     } else {
#       varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$onlyshow, ]
#       if (!("main" %in% names(dotlist))) {
#         dotlist$main <- paste(" ", expression("~"), " ", formlist$varlabels, " (", formlist$onlyshow, ")", sep = "")
#       }
#       do.call("plot", c(list(st_geometry(varsf_sub[formlist$varlabels])), dotlist))
#     }
#     on.exit(par(ask = oldpar$ask))
#   }
# }
