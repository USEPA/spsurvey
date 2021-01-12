summary.spsurvey <- function(object, vars, onlyshow = NULL, ...) {
  
  # keep the sites sf objects from class spsurvey
  sites <- object[names(object) %in% c("sites.base", "sites.over", "sites.near")]
  
  # storing output if non-null
  output <- lapply(sites, function(x) {
      if (is.null(x)) {
        x  
      } else {
        summary.sframe(x, vars, onlyshow, ...)
      }
    }
  )
  # returning non-null outuput
  output <- output[!vapply(output, is.null, logical(1))]
}

summary.sframe <- function(object, vars, onlyshow = NULL, ...) {
  # making formlist (utils.R)
  formlist <- make_formlist(vars, onlyshow)
  # making varsf (utils.R)
  varsf <- make_varsf(object, formlist)
  # accomodating an intercept
  if (formlist$intercept) {
    # adding "total" to varlabels
    formlist$varlabels <- c("total", formlist$varlabels)
    # making a factor called "total"
    varsf$total <- as.factor("total")
    # adding the factor to varsf
    varsf <- varsf[, c(formlist$response, formlist$varlabels), drop = FALSE]
  }
  # calling the appropriate summary based on formula (right hand side vs two sided)
  if (is.null(formlist$response)) {
    output <- cat_summary(formlist, varsf, ...)
  } else {
    output <- cont_summary(formlist, varsf, ...)
  }
}


cat_summary <- function(formlist, varsf, ...) {
  dotlist <- list(...)
  if (!("maxsum" %in% names(dotlist))) {
    dotlist$maxsum <- 1e10
  }
  varsf_nogeom <- st_drop_geometry(varsf)
  if (!is.null(formlist$onlyshow)) {
    indexcol <- formlist$varlabels[length(formlist$varlabels)]
    varsf_nogeom <- varsf_nogeom[varsf_nogeom[[indexcol]] == formlist$onlyshow, indexcol, drop = FALSE]
    varsf_nogeom <- na.omit(varsf_nogeom)
    varsf_nogeom[[indexcol]] <- factor(varsf_nogeom[[indexcol]], levels = formlist$onlyshow)
  }
  output <- do.call("summary.data.frame", c(list(varsf_nogeom), dotlist))
}

cont_summary <- function(formlist, varsf, ...) {
  dotlist <- list(...)
  varsf_nogeom <- st_drop_geometry(varsf)
  if (!is.null(formlist$onlyshow)) {
    formlist$varlabels <- formlist$varlabels[length(formlist$varlabels)]
    varsf_nogeom <- varsf_nogeom[varsf_nogeom[[formlist$varlabels]] == formlist$onlyshow, c(formlist$response, formlist$varlabels), drop = FALSE]
    varsf_nogeom <- na.omit(varsf_nogeom)
    varsf_nogeom[[formlist$varlabels]] <- factor(varsf_nogeom[[formlist$varlabels]], levels = formlist$onlyshow)
  }
  output <- lapply(formlist$varlabels, function(x) {
    varlevels <- do.call("tapply",
                         c(list(X = varsf_nogeom[[formlist$response]],
                                INDEX = varsf_nogeom[[x]],
                                FUN = summary.default),
                           dotlist))
    do.call("rbind", varlevels)
  })
  names(output) <- paste(formlist$response, "by", formlist$varlabels, sep = " ")
  output
}





