#' Internal function to run MARK analysis on simulated data
#' 
#' \code{.inp} files should be located in the working directory where MARK
#' can find them.
#' 
#' @param fn Character. A (absolute) path to an .inp file. If relative, it
#'  looks for it in the working directory.
#' @param wd Character. Working directory where model files (\code{wd.model} 
#'  will be deposited and the source of \code{.inp} files (\code{wd.inp}).
#'  
# TODO: dodati še exporte za RMark funkcije

markAnalysis <- function(fn, wd.model, wd.inp, ...) {
	
  # browser()
  
	# READ IN RAW FILE
	oldwd <- getwd()
	setwd(wd.inp)
	data.in <- readMark(as.character(fn))
	
	# DO THE ANALYSIS
	setwd(oldwd)
	setwd(wd.model)
	tr.process <- process.data(data.in, model = "Huggins")
	tr.ddl <- make.design.data(tr.process)
	
	p.cequal.dot <- list(formula = ~ 1, share = TRUE)
	p.cequal.dot.sp <- list(formula = ~ sp, share = TRUE)
	
	# collect models and run
	cml <- create.model.list("Huggins")
	tr.result <- mark.wrapper(cml, data = tr.process, ddl = tr.ddl, adjust = FALSE,
			invisible = TRUE)

	# EXTRACT DATA FROM THE ANALYSIS
	cp.der <- data.frame(tr.result$p.cequal.dot$results$derived,
			model.name = tr.result$p.cequal.dot$model.name,
			AICc = tr.result$p.cequal.dot$results$AICc)
	cpsp.der <- data.frame(tr.result$p.cequal.dot.sp$results$derived,
			model.name = tr.result$p.cequal.dot.sp$model.name,
			AICc = tr.result$p.cequal.dot.sp$results$AICc)
	der <- data.frame(rbind(cp.der, cpsp.der))
	
	cp.real <- data.frame(tr.result$p.cequal.dot$results$real,
			model.name = tr.result$p.cequal.dot$model.name,
			AICc = tr.result$p.cequal.dot$results$AICc)
	cpsp.real <- data.frame(tr.result$p.cequal.dot.sp$results$real,
			model.name = tr.result$p.cequal.dot.sp$model.name,
			AICc = tr.result$p.cequal.dot.sp$results$AICc)
	real <- data.frame(rbind(cp.real, cpsp.real))

	# Calculate differences in models' AIC. If value is negative, it means that the 
	# presumably "better" model (p(~sp)c()) is inferior and a warning is issued.
	dAIC <- abs(tr.result$p.cequal.dot$results$AICc - tr.result$p.cequal.dot.sp$results$AICc)
	if (dAIC <= 0) warning("AIC of p(.)=c(.)*sp is equal or worse than p(.) = c(.) ")
	
	# OUTPUT THE DATA
	out <- list()
	out$real.fun.pars <- real
	out$est.der.pars <- der
	out$deltaAIC <- dAIC
	
	setwd(oldwd)
	
	return(out)
}