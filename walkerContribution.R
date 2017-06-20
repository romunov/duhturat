#' A wrapper for a bunch of functions that calcualte contribution of
#' individual walker to the sampling area. For arguments, see individual
#' functions.
#' @author Roman Luštrik

walkerContribution <- function(num.walkers, sw, area, home.range, sap.poly, seed,
                               prob, sessions, weight.switch, .object, .num.boots, custom.walkers, SD, 
                               sim.dist, work.dir, ...) {
  
  # We first need to populate our world from which we will sample.
  walk.walkers <- populateWorld(num.walkers = num.walkers, sap = sap.poly,
                                area = area, home.range = home.range, custom.walkers = custom.walkers)
  
  # Sample walkers that come in contact with the sampling area with
  # a certain probability in a number of sessions.
  walk.sample <- sampleWorld(walk = walk.walkers, sap.poly = sap.poly,
                             sessions = sessions, prob = prob, SD = SD)
  
  walk.contrib <- calculateContribution(walk.pair = walk.sample$walk.pair,
                                        sap.poly = sap.poly, walks = walk.sample$sample, weight.switch = weight.switch,
                                        ..object = .object, ..num.boots = .num.boots, sim.dist = sim.dist, SD = SD, 
                                        work.dir = work.dir, seed = seed, ...)
  
  # Construct output object
  out <- list(
    walkers = walk.walkers,
    sample = walk.sample,
    contribs = walk.contrib
  )
  
  return(out)
}