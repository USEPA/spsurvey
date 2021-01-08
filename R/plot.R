plot.spsurvey <- function(object, sframe = NULL, sites = c("sframe", "sites.base"),
                          vars = ~ sites, varsub = NULL, fix_aspect = TRUE, addargs, ...) {

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
  # make varlists
  varlist <- make_varlist(vars, varsub)

  # make sframe
  varsfs <- lapply(object, function(x) make_varsf(x, varlist))
  object <- do.call("rbind", varsfs)
  
  if (!missing(addargs)) {
    argslist <- c(list(sites = site_names), addargs)
    argsdf <- merge(argslist, object)[, names(addargs), drop = FALSE]
    addargs <- as.list(argsdf)
    plot.sframe(object, vars, varsub, fix_aspect, addargs = addargs, ...)
  } else {
    plot.sframe(object, vars, varsub, fix_aspect, ...)
  }
}

plot.sframe <- function(object, vars = ~ 1, varsub = NULL, fix_aspect = TRUE, ...) {

  # making variable list
  varlist <- make_varlist(vars, varsub)


  # plot geometry for ~ 1
  if (length(varlist$varnames) == 0 && varlist$intercept) {
    invisible(plot(st_geometry(object), ...))
  } else if (is.null(varlist$response)) {
    # making variable df for plotting
    varsf <- make_varsf(object, varlist)
    invisible(make_catplot(varsf, varlist, fix_aspect, ...))
  } else {
    varsf <- make_varsf(object, varlist)
    invisible(make_contplot(varsf, varlist, fix_aspect, ...))
  }
}

make_varlist <- function(vars, varsub) {
  # find all terms from the formula
  varterms <- terms(vars)
  
  # find all variable names
  allvars <- all.vars(varterms)
  
  # find all right hand side names
  varlabels <- attr(varterms, "term.labels")
  
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
  
  if (missing(varsub)) {
    varsub <- NULL
  }
  
  # storing the output list
  varlist <- list(
    varterms = varterms,
    allvars = allvars,
    varlabels = varlabels,
    intercept = intercept,
    response = response,
    varnames = varnames,
    varnames_split = varnames_split,
    varsub = varsub
  )
}

make_varsf <- function(object, varlist) {
  # can possibly deprecate this in the future by making use of
  # model.frame and extracting the main effects and using them to make the interactions
  # only real advantage will be creating variables for use mid formula with numeric variables
  object_df <- st_drop_geometry(object)
  object_geometry <- st_geometry(object)
  varcols <- lapply(
    varlist$varnames_split,
    function(x) {
      if (length(x) == 1 && is.numeric(object_df[[x]])) {
        return(object_df[, x, drop = FALSE]) # return numeric if provided
      } else {
        return(interaction(object_df[, x, drop = FALSE], sep = ":")) # return factors
      }
    }
  )
  df_varcols <- as.data.frame(varcols, optional = TRUE) # without optional the : in name gets
  # converted to synctactic name with .
  df_to_sf <- st_as_sf(df_varcols, geometry = object_geometry)
}



make_catplot <- function(varsf, varlist, fix_aspect, addargs, ...) {
  
  oldpar <- par()
  if (missing(addargs)){
    dotlist <- list(...)
  } else {
    dotlist <- c(addargs, list(...))
  }
  
  if (length(varlist$varnames) > 1) {
    par(ask = TRUE)
  }
  
  if (is.null(varlist$varsub)) {
    if (fix_aspect) {
      dotlist$xlim <- st_bbox(varsf)[c(1, 3)]
      dotlist$ylim <- st_bbox(varsf)[c(2, 4)]
    }
    lapply(
      varlist$varnames,
      function(x) {
        dotlist$main <- paste(" ", expression("~"), " ", x, sep = "")
        do.call("plot", c(list(varsf[x]), dotlist))
      }
    )
  } else {
    varsf_sub <- varsf[varsf[[varlist$varlabels]] == varlist$varsub, ]
    if (!("main" %in% names(dotlist))) {
      dotlist$main <- paste(" ", expression("~"), " ", varlist$varlabels, " (", varlist$varsub, ")", sep = "")
    }
    do.call("plot", c(list(st_geometry(varsf_sub[varlist$varlabels])), dotlist))
  }
  on.exit(par(ask = oldpar$ask))
}

make_contplot <- function(varsf, varlist, fix_aspect, addargs, ...) {
  oldpar <- par()
  if (missing(addargs)){
    dotlist <- list(...)
  } else {
    dotlist <- c(addargs, list(...))
  }
  if (is.null(varlist$varsub)) {
    if (get_varlevels(varlist, varsf) + 1 * varlist$intercept > 1) {
      par(ask = TRUE)
    }

    if (fix_aspect) {
      dotlist$xlim <- st_bbox(varsf)[c(1, 3)]
      dotlist$ylim <- st_bbox(varsf)[c(2, 4)]
    }
        
    if (varlist$intercept) {
      if (varlist$response %in% varlist$varlabels) {
        varlisttemp <- varlist
        varlisttemp$varnames <- unique(varlisttemp$varnames)
        if (missing(addargs)) {
          make_catplot(varsf, varlisttemp, fix_aspect, ...)
        } else {
          make_catplot(varsf, varlisttemp, fix_aspect, addargs, ...)
        }
      } else {
        dotlist$main <- varlist$response
        do.call("plot", c(list(varsf[varlist$response]), dotlist))
      }
    }
    vars_split <- lapply(varlist$varlabels, function(x) split(varsf[, c(varlist$response, x)], varsf[[x]]))
    names(vars_split) <- varlist$varlabels
    if (varlist$response %in% varlist$varlabels) {
      lapply(varlist$varlabels,
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
      lapply(varlist$varlabels,
             function(x) {
               lapply(names(vars_split[[x]]),
                      function(y) {
                        dotlist$main <- paste(varlist$response, " ", expression("~"), " ", x, " (", y, ")", sep = "")
                        do.call("plot", c(list(vars_split[[x]][[y]][varlist$response]), dotlist))
                      }
               )
             }
      )
    }
  } else {
    varsf_sub <- varsf[varsf[[varlist$varlabels]] == varlist$varsub, ]
    if (!("main" %in% names(dotlist))) {
      dotlist$main <- paste(varlist$response," ", expression("~"), " ", varlist$varlabels, " (", varlist$varsub, ")", sep = "")
    }
    do.call("plot", c(list(varsf_sub[varlist$response]), dotlist))
  }
  on.exit(par(ask = oldpar$ask))
}
