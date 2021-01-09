plot.sframe <- function(object, formula = ~ 1, geom = FALSE, showonly = NULL, fix_bbox = TRUE, variableargs = NULL, levelargs = NULL, ...) {
  
  # storing dotlist
  dot_list <- list(...)
  if (fix_bbox) {
    dot_list$xlim <- st_bbox(object)[c(1, 3)]
    dot_list$ylim <- st_bbox(object)[c(2, 4)]    
  }
  
  # making variable list
  formlist <- make_formlist(formula, showonly)
  
  # plot geometry for ~ 1
  if (length(formlist$varnames) == 0 && formlist$intercept) {
    return(invisible(do.call("plot", c(list(st_geometry(object)), dot_list))))
  }
  
  # make varsf object
  varsf <- make_varsf(object, formlist)
  
  # get levelargs list
  if (!is.null(levelargs)) {
    levelargs_list <- make_levelargs_list(varsf, levelargs)
  } else {
    levelargs_list <- NULL
  }

  
  if (is.null(formlist$response)) {
    if (is.null(formlist$showonly)) {}
    return(invisible(plot_rhf(varsf, formlist, geom, showonly, fix_bbox, variableargs, levelargs_list, dot_list)))
  } else {
    return(invisible(plot_twohf(varsf, formlist, geom, showonly, fix_bbox, variableargs, levelargs_list, dot_list)))
  }
}

plot_rhf <- function(varsf, formlist, geom, showonly, fix_bbox, variableargs, levelargs_list, dot_list) {
  
    if (is.null(showonly)) {
      # setting old graphical parameter value
      oldpar <- par()
      
      # turning on ask if necessary
      if (length(formlist$varnames) > 1) {
        par(ask = TRUE)
      }
      
      lapply(formlist$varnames, function(x) {
        if (!("main" %in% names(variableargs[[x]]))) {
          variableargs[[x]]$main <- paste(" ", expression("~"), " ", x, sep = "")
        }
        do.call("plot", c(list(varsf[x]), variableargs[[x]], levelargs_list[[x]], dot_list))
      })
      on.exit(par(ask = oldpar$ask))
    } else {
        varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$showonly, ]
        if (!("main" %in% names(dot_list))) {
          dot_list$main <- paste(" ", expression("~"), " ", formlist$varlabels, sep = "")
        }
        do.call("plot", c(list(varsf_sub[formlist$varlabels]), dot_list))
        # varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$showonly, ]
        # if (!("main" %in% names(dot_list))) {
        #   dot_list$main <- paste(" ", expression("~"), " ", formlist$varlabels, " (", formlist$showonly, ")", sep = "")
        # }
        # do.call("plot", c(list(st_geometry(varsf_sub[formlist$varlabels])), dot_list))
    }
}
    

      lapply(
        formlist$varnames,
        function(x) {
          dotlist$main <- paste(" ", expression("~"), " ", x, sep = "")
          do.call("plot", c(list(varsf[x]), dotlist))
        }
      )
    } else {
      varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$showonly, ]
      if (!("main" %in% names(dotlist))) {
        dotlist$main <- paste(" ", expression("~"), " ", formlist$varlabels, " (", formlist$showonly, ")", sep = "")
      }
      do.call("plot", c(list(st_geometry(varsf_sub[formlist$varlabels])), dotlist))
    }
    on.exit(par(ask = oldpar$ask))
  }
}
