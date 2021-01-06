plot.spsurvey <- function(object, args_sites.base, args_sites.over, args_sites.near) {
  
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
    invisible(make_catplot(vardf, varlist, fix_aspect))
  }
}

plot.sframe <- function(object, vars = ~ 1, ...) {
  oldpar <- par()
  varterms <- terms(vars)
  if (identical(vars, ~ 1)) {
    plot(st_geometry(object), ...)
  } else if (attr(varterms, "response") == 0) {
    vars_sf <- subset_vars(object, vars)
    par(ask = TRUE)
    vars_sf_names <- colnames(vars_sf)[!(colnames(vars_sf) %in% c("geometry"))] # use st_drop_geometry()
    invisible(lapply(vars_sf_names, function(x) plot(vars_sf[x], main = paste(" ", expression("~"), " ", x, sep = ""), ...)))
  } else {
    xlim <- st_bbox(object)[c(1, 3)]
    ylim <- st_bbox(object)[c(2, 4)]
    response <- all.vars(terms(vars))[1]
    vars_sf <- subset_vars(object, vars)
    if (attr(varterms, "intercept") == 1) {
      vars_sf$intercept <- "intercept"
      vars_sf <- vars_sf[c("intercept", setdiff(colnames(vars_sf), "intercept"))]
    }
    par(ask = TRUE)
    vars_sf_names <- colnames(vars_sf)[!(colnames(vars_sf) %in% c(response, "geometry"))]
    vars_split <- lapply(vars_sf_names, function(x) split(vars_sf[, c(response, x)], vars_sf[[x]]))
    names(vars_split) <- vars_sf_names
    if (attr(varterms, "intercept") == 1) {
      vars_sf_names <- vars_sf_names[vars_sf_names != "intercept"]
      invisible(plot(vars_split[["intercept"]][["intercept"]][response], ...))
      invisible(lapply(vars_sf_names, function(x) lapply(names(vars_split[[x]]),
                                                         function(y) plot(vars_split[[x]][[y]][response],
                                                                          main = paste(response, " ", expression("~"), " ", x, " (", y, ")", sep = ""),
                                                                          xlim = xlim, ylim = ylim, ...))))
    } else {
      invisible(lapply(vars_sf_names, function(x) lapply(names(vars_split[[x]]),
                                                         function(y) plot(vars_split[[x]][[y]][response],
                                                                          main = paste(response, " ", expression("~"), " ", x, " (", y, ")", sep = ""),
                                                                          xlim = xlim, ylim = ylim, ...))))
    }
  }
  on.exit(par(ask = oldpar$ask))
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
  } else {
    varsub <- varsub
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

  object_df <- st_drop_geometry(object)
  object_geometry <- st_geometry(object)
  varcols <- lapply(
    varlist$varnames_split,
    function(x) {
      if (length(x) == 1 && is.numeric(object_df[[x]])) {
        return(object_df[, x, drop = FALSE]) # return numeric if provided
      } else {
        return(interaction(object_df[, x, drop = FALSE])) # return factors
      }
    }
  )
  df_varcols <- as.data.frame(varcols, optional = TRUE) # without optional the : in name gets
  # converted to synctactic name with .
  df_to_sf <- st_as_sf(df_varcols, geometry = object_geometry)
}

make_catplot <- function(vardf, varlist, fix_aspect, ...) {
  
  if (is.null(varlist$varsub)) {
    lapply(
      varlist$varnames,
      function(x) {
        if (fix_aspect) {
          xlim <- st_bbox(vardf)[c(1, 3)]
          ylim <- st_bbox(vardf)[c(2, 4)]
          plot(vardf[x], main = paste(" ", expression("~"), " ", x, sep = ""), xlim = xlim, ylim = ylim, ...)
        } else {
          plot(vardf[x], main = paste(" ", expression("~"), " ", x, sep = ""), ...)
        }
      } 
    )
  } else {
    vardf_sub <- vardf[vardf[[varlist$varlabels]] == varlist$varsub, ]
    if (fix_aspect) {
      xlim <- st_bbox(vardf)[c(1, 3)]
      ylim <- st_bbox(vardf)[c(2, 4)]
      plot(st_geometry(vardf_sub[varlist$varlabels]),
              main = paste(" ", expression("~"), " ", varlist$varlabels, " (", varlist$varsub, ")", sep = ""),
              xlim = xlim, 
              ylim = ylim,
              ...
           )
    } else {
      plot(st_geometry(vardf_sub[varlist$varlabels]),
              main = paste(" ", expression("~"), " ", varlist$varlabels, " (", varlist$varsub, ")", sep = ""),
              ...
      )
    }
  }
}

# subset_vars <- function(object, vars) {
#   # convert a*b to a + b + a:b
#   varterms <- terms(vars)
#   if (attr(varterms, "response") == 1) {
#     response <- all.vars(varterms)[1]
#   } else {
#     response <- NULL
#   }
#   varnames <- c(response, attr(varterms, "term.labels"))
#   # split variables on : to subset 
#   varnames_split <- strsplit(varnames, ":")
#   object_df <- as.data.frame(unclass(object))
#   object_geometry <- st_geometry(object)
#   list_subset <- lapply(varnames_split, function(x) {
#       if (all(unlist(lapply(object_df[, x], is.numeric)))) {
#         object_df[, x]
#       } else {
#         interaction(object_df[, x], sep = ":") 
#       }
#     }
#   )
#   # giving columns appropriate names
#   names(list_subset) <- varnames
#   # coverting from list to data frame
#   df_subset <- as.data.frame(list_subset, optional = TRUE) # without optional the : in name gets
#   # converted to synctactic name with .
#   df_sf <- st_as_sf(df_subset, geometry = object_geometry)
# }
