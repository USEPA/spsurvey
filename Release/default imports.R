imports_for_undefined_globals <- function(txt, lst, selective = TRUE) {
	if(!missing(txt))
		lst <- scan(what = character(), text = txt, quiet = TRUE)
	nms <- lapply(lst, find)
	ind <- sapply(nms, length) > 0L
	imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
	if(selective) {
		sprintf("importFrom(%s)",
				vapply(Map(c, names(imp), imp),
					   function(e)
						   paste0("\"", e, "\"", collapse = ", "),
					   ""))
	} else {
		sprintf("import(\"%s\")", names(imp))
	}
}					   

txt <- "addmargins axis box dist dnorm ftable graphics.off legend lines
	model.frame mtext par pchisq pdf pf pnorm points qnorm rainbow rnorm
	runif text title var"

writeLines(imports_for_undefined_globals(txt))

importFrom(graphics, axis, box, legend, lines, mtext, par, points, text, title)
importFrom(grDevices, graphics.off, pdf, rainbow)
importFrom(stats, addmargins, dist, dnorm, ftable, model.frame, pchisq, pf,
	pnorm, qnorm, rnorm, runif, var)
