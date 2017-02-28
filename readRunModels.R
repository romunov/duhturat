#' A set of function to read and run MARK files/models.

readMark <- function(inp) {
	raw.lines <- readLines(inp)
	num.skip <- which(raw.lines == "")
	cap.hist <- read.table(file = inp, col.names = c("ch", "int", "grp", "sp"),
			skip = num.skip, colClasses = c("character", "factor", "factor", "numeric"),
			comment.char = ";")
	return(cap.hist)
}

runModels <- function(obj, wd, ...) {
	# obj is read in data from inp file
	# wd is working directory where magic happens (files are deposited)
	# ... other parameters to be passed to mark.wrapper
	wd.old <- getwd()
	setwd(wd)
	
	tr.process <- process.data(obj, model = "Huggins")
	tr.ddl <- make.design.data(tr.process)
	
	
	p.cequal.dot <- list(formula = ~ 1, share = TRUE)
	p.cequal.dot.sp <- list(formula = ~ sp, share = TRUE)
	
	# collect models and run
	cml <- create.model.list("Huggins")
	tr.result <- mark.wrapper(cml, data = tr.process, ddl = tr.ddl, adjust = FALSE,
			invisible = TRUE, ...)
	
	setwd(wd.old)
	invisible(tr.result)
}

extractPars <- function(inp) {
#	browser() #BROWSER
	
	# extract if pars written in humanoid readable form
	inp.raw <- readLines(inp)
	
	# extract data.frame that is between these two variables
	read.start <- which(inp.raw == "read start ")
	read.stop <- which(inp.raw == "read stop ")
	data.frame.from.inp <- read.table(file = inp, header = TRUE, skip = read.start,
		nrows = diff(c(read.start, read.stop)) - 2) #
	
	# this chunk-o-code is for human-readable format, but it messes up the number
	# of walkers sampled. didn't have time to fix it.
#	inp.data <- sapply(X = inp.raw[c(3:6, 8:15, 17:18)], FUN = function(m) {
#				m <- unlist(strsplit(m, split = ": "))
#				out <- data.frame(name = m[1], value = as.numeric(m[2]))
#				return(out)
#			}, simplify = FALSE, USE.NAMES = FALSE)
#	inp.pars <- do.call("rbind", inp.data)
#	# this is a DIY t() with 
#	inp.df <- data.frame(as.list(inp.pars$value))
#	colnames(inp.df) <- make.names(inp.pars$name) # make syntactically valid names
	
	return(data.frame.from.inp)
}