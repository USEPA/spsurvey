summary.spsurvey <- function(object, vars, varsub = NULL, ...) {
  
  # keep the sites sf objects from class spsurvey
  sites <- object[names(object) %in% c("sites.base", "sites.over", "sites.near")]
  
  # storing output if non-null
  output <- lapply(sites, function(x) {
      if (is.null(x)) {
        x  
      } else {
        summary.sframe(x, vars, varsub, ...)
      }
    }
  )
  # returning non-null outuput
  output <- output[!vapply(output, is.null, logical(1))]
}

summary.sframe <- function(object, vars, varsub = NULL, ...) {
  # making varlist (utils.R)
  varlist <- make_varlist(vars, varsub)
  # making varsf (utils.R)
  varsf <- make_varsf(object, varlist)
  # accomodating an intercept
  if (varlist$intercept) {
    # adding "total" to varlabels
    varlist$varlabels <- c("total", varlist$varlabels)
    # making a factor called "total"
    varsf$total <- as.factor("total")
    # adding the factor to varsf
    varsf <- varsf[, c(varlist$response, varlist$varlabels), drop = FALSE]
  }
  # calling the appropriate summary based on formula (right hand side vs two sided)
  if (is.null(varlist$response)) {
    output <- cat_summary(varlist, varsf, ...)
  } else {
    output <- cont_summary(varlist, varsf, ...)
  }
}


cat_summary <- function(varlist, varsf, ...) {
  dotlist <- list(...)
  if (!("maxsum" %in% names(dotlist))) {
    dotlist$maxsum <- 1e10
  }
  varsf_nogeom <- st_drop_geometry(varsf)
  if (!is.null(varlist$varsub)) {
    indexcol <- varlist$varlabels[length(varlist$varlabels)]
    varsf_nogeom <- varsf_nogeom[varsf_nogeom[[indexcol]] == varlist$varsub, indexcol, drop = FALSE]
    varsf_nogeom <- na.omit(varsf_nogeom)
    varsf_nogeom[[indexcol]] <- factor(varsf_nogeom[[indexcol]], levels = varlist$varsub)
  }
  output <- do.call("summary.data.frame", c(list(varsf_nogeom), dotlist))
}

cont_summary <- function(varlist, varsf, ...) {
  dotlist <- list(...)
  varsf_nogeom <- st_drop_geometry(varsf)
  if (!is.null(varlist$varsub)) {
    varlist$varlabels <- varlist$varlabels[length(varlist$varlabels)]
    varsf_nogeom <- varsf_nogeom[varsf_nogeom[[varlist$varlabels]] == varlist$varsub, c(varlist$response, varlist$varlabels), drop = FALSE]
    varsf_nogeom <- na.omit(varsf_nogeom)
    varsf_nogeom[[varlist$varlabels]] <- factor(varsf_nogeom[[varlist$varlabels]], levels = varlist$varsub)
  }
  output <- lapply(varlist$varlabels, function(x) {
    varlevels <- do.call("tapply",
                         c(list(X = varsf_nogeom[[varlist$response]],
                                INDEX = varsf_nogeom[[x]],
                                FUN = summary.default),
                           dotlist))
    do.call("rbind", varlevels)
  })
  names(output) <- paste(varlist$response, "by", varlist$varlabels, sep = " ")
  output
}





