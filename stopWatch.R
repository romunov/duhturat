#' Show elapsed time of an expression.
#' 
#' This function will measure elapsed time of an expression. It requires
#' tcl/tk. After initialization, a window will pop up and measure time
#' in seconds.
#' 
#' @param expr An expression (also a function).
#' @author Barry Rowlingson Feb 2012

stopWatch <- function(expr){
# tcltk R expression timer function, Barry Rowlingson Feb 2012 
	require(tcltk)
	
	timeForm <- function(t) {
		t <- as.numeric(t)
		s <- t %% 60
		m <- ((t-s)/60) %% 60
		h <- t %/% (60*60)
		stopifnot(s+m*60+h*60*60 == t)
		tstring <- sprintf("%02d", c(h, m, s))
		return(paste(tstring, collapse = ":"))
		list(h = h,m = m,s = s)
	}
	
	start <- Sys.time()
	tt <- tktoplevel()
	
	tkwm.title(tt, gsub(" ", "", paste(deparse(substitute(expr)), sep = "", collapse = ";")))
	
	labelText <- tclVar("Running...")
	label1 <- tklabel(tt, text = tclvalue(labelText))
	tkconfigure(label1, textvariable = labelText)
	tkgrid(label1)
	e <- environment()
	z <- function () {
		tclvalue(labelText) <- paste("Running: ", timeForm(Sys.time() - start));
		assign("sw", tcl("after", 1000, z), env = e)
	}
	quit.but <- tkbutton(tt, text = "Close", command = function() tkdestroy(tt))
	tkgrid(quit.but)
	sw = tcl("after", 1000, z)
	
	finished <- function() {
		tcl("after", "cancel", sw)
		tclvalue(labelText) = paste("Done after: ", timeForm(Sys.time() - start));
	}
	
	tryCatch(eval(expr), finally = finished())
}