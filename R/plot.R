plot.spsurvey <- function(object, sframe, args_sites.base, args_sites.over, args_sites.near, args_sites.sframe, ...) {
  
}

plot.sframe <- function(object, vars = ~ 1, varsub, fix_aspect = TRUE, ...) {

  # making variable list
  varlist <- make_varlist(vars, varsub)

  
  # plot geometry for ~ 1
  if (length(varlist$varnames) == 0 && varlist$intercept) {
    invisible(plot(st_geometry(object), ...))
  } else if (is.null(varlist$response)) {
    # making variable df for plotting
    vardf <- make_vardf(object, varlist)
    invisible(make_catplot(vardf, varlist, fix_aspect, ...))
  } else {
    vardf <- make_vardf(object, varlist)
    invisible(make_contplot(vardf, varlist, fix_aspect, ...))
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
  
  # saving varsub if needed
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

make_vardf <- function(object, varlist) {
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

get_varlevels <- function(vardf) {
  vardf_nogeom <- st_drop_geometry(vardf)
  levels <- lapply(vardf_nogeom, function(x) {
              if (is.numeric(x)) {
                levels <- 0
              } else {
                levels <- length(na.omit(unique(x)))
              }
            })
  levels <- sum(unlist(levels))
}

make_catplot <- function(vardf, varlist, fix_aspect, ...) {
  
  oldpar <- par()
  dotlist <- list(...)
  if (length(varlist$varnames) > 1) {
    par(ask = TRUE)
  }
  
  if (is.null(varlist$varsub)) {
    if (fix_aspect) {
      dotlist$xlim <- st_bbox(vardf)[c(1, 3)]
      dotlist$ylim <- st_bbox(vardf)[c(2, 4)]
    }
    lapply(
      varlist$varnames,
      function(x) {
        dotlist$main <- paste(" ", expression("~"), " ", x, sep = "")
        do.call("plot", c(list(vardf[x]), dotlist))
      }
    )
  } else {
    vardf_sub <- vardf[vardf[[varlist$varlabels]] == varlist$varsub, ]
    if (!("main" %in% names(dotlist))) {
      dotlist$main <- paste(" ", expression("~"), " ", varlist$varlabels, " (", varlist$varsub, ")", sep = "")
    }
    do.call("plot", c(list(st_geometry(vardf_sub[varlist$varlabels])), dotlist))
  }
  on.exit(par(ask = oldpar$ask))
}

make_contplot <- function(vardf, varlist, fix_aspect, ...) {
  oldpar <- par()
  dotlist <- list(...)
  if (is.null(varlist$varsub)) {
    if (get_varlevels(vardf) + 1 * varlist$intercept > 1) {
      par(ask = TRUE)
    }

    if (fix_aspect) {
      dotlist$xlim <- st_bbox(vardf)[c(1, 3)]
      dotlist$ylim <- st_bbox(vardf)[c(2, 4)]
    }
        
    if (varlist$intercept) {
      dotlist$main <- varlist$response
      do.call("plot", c(list(vardf[varlist$response]), dotlist))
    }
    vars_split <- lapply(varlist$varlabels, function(x) split(vardf[, c(varlist$response, x)], vardf[[x]]))
    names(vars_split) <- varlist$varlabels
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
  } else {
    vardf_sub <- vardf[vardf[[varlist$varlabels]] == varlist$varsub, ]
    if (!("main" %in% names(dotlist))) {
      dotlist$main <- paste(varlist$response," ", expression("~"), " ", varlist$varlabels, " (", varlist$varsub, ")", sep = "")
    }
    do.call("plot", c(list(vardf_sub[varlist$response]), dotlist))
  }
  on.exit(par(ask = oldpar$ask))
}
