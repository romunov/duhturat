#' Internal function to run MARK analysis on simulated data
#' 
#' \code{.inp} files should be located in the working directory where MARK
#' can find them.
#' 
#' @param fn Character. A (absolute) path to an .inp file. If relative, it
#'  looks for it in the working directory.

markAnalysis <- function(fn) {
  # Create temporary folder and remove it after the analysis is done.	
  oldwd <- getwd()
  x <- tempdir()
  on.exit(setwd(oldwd))
  on.exit(unlink(x), add = TRUE)
  
  # READ IN RAW FILE
  data.in <- readMark(as.character(fn))
  
	# DO THE ANALYSIS
	setwd(x)
	tr.process <- process.data(data.in, model = "Huggins")
	tr.ddl <- make.design.data(tr.process)
	
	# https://sites.google.com/site/workshoponcmr/home/sche/10-closed-capture/huggins
	p.cequal.dot <- list(formula = ~ 1, share = TRUE)
	p.cequal.dot.sp <- list(formula = ~ sp, share = TRUE)
	
	# collect models and run
	cml <- create.model.list("Huggins")
	tr.result <- mark.wrapper(cml, data = tr.process, ddl = tr.ddl, adjust = FALSE,
			invisible = TRUE)
	
	setwd(oldwd)

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

	# Calculate differences in models' AIC
	dAIC <- tr.result$p.cequal.dot$results$AICc - tr.result$p.cequal.dot.sp$results$AICc
	
	# OUTPUT THE DATA
	out <- list()
	out$real.fun.pars <- real
	out$est.der.pars <- der
	out$deltaAIC <- dAIC
	out$simulation.pars <- extractPars(fn)
	
	return(out)
}
