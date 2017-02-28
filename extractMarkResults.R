#' Run MARK with a defined analysis on inp files and extract results and parameters
#' used for the generation of data.
#' @param x Character. An \code{imp} file.
#' @param wd.dump Character. Directory where files from MARK analysis are stored.
#' @param wd.source Character. Directory where \code{.inp} files are located.
#' @out Character. (Full) path to the filename used to store results.

extractMarkResults <- function(x, wd.dump, wd.source, out) {
	
	# TODO: poigraj se s tem, da ne bo treba dajat cele poti do direktorija kjer se shranjujejo
	# fajli od modelov, ampak da boš dal recimo samo ime direktorija, pa bo sam naredil pot oz.
	# bo iskal ./nov_direktorij
  
  browser()
  
	old.wd <- getwd()
	# RUN MARK ANALYSIS
	mark.analysis <- markAnalysis(fn = x, wd.model = wd.dump, wd.inp = wd.source)
	
	# EXTRACT THE DATA FROM THE ANALYSIS
	# extract parameters, change column names to reflect their origin
	lmu.real <- with(mark.analysis, real.fun.pars[, c("lcl", "estimate", "ucl")])
	colnames(lmu.real) <- paste("real", colnames(lmu.real), sep = ".")
	rownames(lmu.real) <- NULL
	
	lmu.der <- with(mark.analysis, est.der.pars[, c("lcl", "estimate", "ucl")])
	colnames(lmu.der) <- paste("derived", colnames(lmu.der), sep = ".")
	rownames(lmu.der) <- NULL
	
	# find where the analysis came from (were weights used, which curve - lower, mean, upper)
	efi <- unlist(strsplit(x, split = "\\.")) # Extract File Information
	efi.curve <- efi[4] # extract from which curve the data came from
	efi.weight <- paste("weight", efi[6], sep = ".") # extract if weights were used or not
	
	
	# read in initial simulation parameters
	# TODO: spremeni branje parametrov, ker se je spremenil zapisovanje inp fajlov
	inp <- extractPars(inp = x) # v readRunModels.R
	
	# make one big data.frame out of initial and calculated parameters
	to.write.out <- data.frame(inp, lmu.real, lmu.der, AIC = mark.analysis$est.der.pars$AICc,
			dAIC = mark.analysis$deltaAIC, model = mark.analysis$real.fun.pars[, "model.name"],
			weight = efi.weight, curve = efi.curve, file.name = x)
	
	# WRITE TO RESULT FILE
	# If file already exists, it presumes that it has at least one line of data
	# written down so no column names are written, and file is just appended. If
	# file doesn't exist, it creates one with col names and the whole shabang.
	setwd(wd.source)
  
	if (file.exists(out)) {
		write.table(x = to.write.out, file = out, append = TRUE, col.names = FALSE,
				row.names = FALSE)
	} else {
		write.table(x = to.write.out, file = out, append = FALSE, col.names = TRUE,
				row.names = FALSE)
	}
	setwd(old.wd)

	return(to.write.out)
}