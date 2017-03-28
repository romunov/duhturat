#' A wrapper for a bunch of functions that calcualte contribution of
#' individual walker to the sampling area. For arguments, see individual
#' functions.
#' @author Roman Luštrik


walkerContribution <- function(num.walkers, sw, area, home.range, sap.poly, 
	n.steps, prob, sessions, weight.switch, .object, .num.boots, custom.walkers, SD, ...) {
  # browser()
	# We first need to populate our world from which we will sample.
	walk.walkers <- populateWorld(num.walkers = num.walkers, sw = sw,
		area = area, home.range = home.range, n.steps = n.steps, 
    custom.walkers = custom.walkers, ...)
	
  # Sample walkers that come in contact with the sampling area with
	# a certain probability in a number of sessions.
	walk.sample <- sampleWorld(walk = walk.walkers, sap.poly = sap.poly,
		sessions = sessions, prob = prob, SD = SD, ...)
#  lapply(walk.walkers[rownames(walk.contrib$cona$weight.yes)], plot, add = T)
  
	walk.contrib <- calculateContribution(walk.pair = walk.sample$walk.pair,
		sap.poly = sap.poly, walks = walk.sample$sample, weight.switch = weight.switch,
		..object = .object, ..num.boots = .num.boots, ...)

  browser()
	# Construct output object
	out <- list(
		walkers = walk.walkers,
		sample = walk.sample,
		contribs = walk.contrib
	)
	
	return(out)
}