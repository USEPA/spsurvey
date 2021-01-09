plot.spsurvey <- function(object, sframe = NULL, sites = c("sframe", "sites.base"),
                          vars = ~ sites, showonly = NULL, fix_bbox = TRUE, addargs, ...) {

  site_names <- sites
  sites <- sites[sites != "legacy"]
  object <- c(list(sframe = sframe), object)
  object <- object[sites]
  # removing null objects
  object <- object[!vapply(object, is.null, logical(1))]
  object_names <- names(object)
  site_names <- site_names[site_names %in% c(object_names, "legacy")]
  # append sframe if provided
  
  object <- lapply(object_names, function(x) merge(object[[x]], data.frame(sites = x)))
  names(object) <- object_names
  if ("sites.base" %in% object_names && any(!is.na(object$sites.base$legacy))) {
    levels(object$sites.base$sites) <-  c("sites.base", "legacy")
    object$sites.base$sites[!is.na(object$sites.base$legacy)] <- "legacy"
  }
  # make formlists
  formlist <- make_formlist(vars, showonly)

  # make sframe
  varsfs <- lapply(object, function(x) make_varsf(x, formlist))
  object <- do.call("rbind", varsfs)
  
  if (!missing(addargs)) {
    argslist <- data.frame(c(list(sites = site_names), addargs), stringsAsFactors = FALSE)
    argsdf <- merge(argslist, object)[, names(addargs), drop = FALSE]
    addargs <- as.list(argsdf)
    plot.sframe(object, vars, showonly, fix_bbox, addargs = addargs, ...)
  } else {
    plot.sframe(object, vars, showonly, fix_bbox, ...)
  }
}

plot.sframe <- function(object, vars = ~ 1, showonly = NULL, fix_bbox = TRUE, ...) {

  # making variable list
  formlist <- make_formlist(vars, showonly)


  # plot geometry for ~ 1
  if (length(formlist$varnames) == 0 && formlist$intercept) {
    invisible(plot(st_geometry(object), ...))
  } else if (is.null(formlist$response)) {
    # making variable df for plotting
    varsf <- make_varsf(object, formlist)
    invisible(make_catplot(varsf, formlist, fix_bbox, ...))
  } else {
    varsf <- make_varsf(object, formlist)
    invisible(make_contplot(varsf, formlist, fix_bbox, ...))
  }
}

make_catplot <- function(varsf, formlist, fix_bbox, addargs, ...) {
  
  oldpar <- par()
  if (missing(addargs)){
    dotlist <- list(...)
  } else {
    dotlist <- addargs
    dotlistadd <- c(addargs, list(...))
    #dotlistadd <- addargs
  }
  
  if (length(formlist$varnames) > 1) {
    par(ask = TRUE)
  }
  
  if (is.null(formlist$showonly)) {
    if (fix_bbox) {
      dotlist$xlim <- st_bbox(varsf)[c(1, 3)]
      dotlist$ylim <- st_bbox(varsf)[c(2, 4)]
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

make_contplot <- function(varsf, formlist, fix_bbox, addargs, ...) {
  oldpar <- par()
  if (missing(addargs)){
    dotlist <- list(...)
  } else {
    dotlist <- list(...)
    dotlistadd <- addargs
    dotlistadd2 <- c(addargs, dotlist)
  }
  if (is.null(formlist$showonly)) {
    if (get_varlevels(formlist, varsf) + 1 * formlist$intercept > 1) {
      par(ask = TRUE)
    }

    if (fix_bbox) {
      dotlist$xlim <- st_bbox(varsf)[c(1, 3)]
      dotlist$ylim <- st_bbox(varsf)[c(2, 4)]
    }
        
    if (formlist$intercept) {
      if (formlist$response %in% formlist$varlabels) {
        formlisttemp <- formlist
        formlisttemp$varnames <- unique(formlisttemp$varnames)
        if (missing(addargs)) {
          do.call("make_catplot", c(list(varsf), list(formlisttemp), list(fix_bbox), dotlist))
        } else {
          do.call("make_catplot", c(list(varsf), list(formlisttemp), list(fix_bbox), dotlistadd))
        }
      } else {
        dotlist$main <- formlist$response
        do.call("plot", c(list(varsf[formlist$response]), dotlist))
      }
    }
    vars_split <- lapply(formlist$varlabels, function(x) split(varsf[, c(formlist$response, x)], varsf[[x]]))
    names(vars_split) <- formlist$varlabels
    if (formlist$response %in% formlist$varlabels) {
      lapply(formlist$varlabels,
             function(x) {
               lapply(names(vars_split[[x]]),
                      function(y) {
                        dotlist$main <- paste("1", " ", expression("~"), " ", x, " (", y, ")", sep = "")
                        do.call("plot", c(list(st_geometry(vars_split[[x]][[y]])), dotlist))
                      }
               )
             }
      )
    } else {
      lapply(formlist$varlabels,
             function(x) {
               lapply(names(vars_split[[x]]),
                      function(y) {
                        dotlist$main <- paste(formlist$response, " ", expression("~"), " ", x, " (", y, ")", sep = "")
                        do.call("plot", c(list(vars_split[[x]][[y]][formlist$response]), dotlist))
                      }
               )
             }
      )
    }
  } else {
    varsf_sub <- varsf[varsf[[formlist$varlabels]] == formlist$showonly, ]
    if (!("main" %in% names(dotlist))) {
      dotlist$main <- paste(formlist$response," ", expression("~"), " ", formlist$varlabels, " (", formlist$showonly, ")", sep = "")
    }
    do.call("plot", c(list(varsf_sub[formlist$response]), dotlist))
  }
  on.exit(par(ask = oldpar$ask))
}
