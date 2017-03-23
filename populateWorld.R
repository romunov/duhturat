#' This function generates walkers inside and around the future to be sampling area.
#' 
#' See \code{simulation} for the description of other parameters.
#' 
#' @param ... Parameters passed on to \code{snowfall:::sfInit()}.
#' @returnType List.
#' @return A list of walkers.
#' @author Roman Luštrik

populateWorld <- function(num.walkers, sw, area, home.range, n.steps, sap, 
  custom.walkers, ...) {
  
  if (!is.null(custom.walkers)) {
    xy <- custom.walkers
  } else {
    xy <- data.frame(x = runif(num.walkers, min = -sw/2, max = sw/2), 
                     y = runif(num.walkers, min = -sw/2, max = sw/2),
                     count = 1:num.walkers)
    
    # Simulation can't handle a lot of walkers, so we'll subset only the points
    # that are close to the sampling area.
    bf <- area + 1.5 * home.range #TODO: izracunaj kje je sampling area, dodaj malo paddinga in uporabi to za subsetanje
    xy <- xy[xy$x < bf & xy$x > -bf & xy$y > -bf & xy$y < bf, ]
  }
  
  message("Walkers near the sampling area: ", nrow(xy))

  # Create home range area for each walker.
  out <- apply(xy[, c("x", "y")], MARGIN = 1, FUN = function(xy, n.steps, home.range) {
      dot <- SpatialPoints(coordinates(matrix(c(xy[1], xy[2]), ncol = 2)))
      random.walk <- gBuffer(spgeom = dot, width = home.range, quadsegs = home.range * 5)
      random.walk
    }, n.steps = n.steps, home.range = home.range)
  
  names(out) <- paste(1:length(custom.walkers[, "capt"]), custom.walkers[, "capt"], sep = "_")
  out
  
#  plot(0,0, type = "n", xlim = c(-300, 300), ylim = c(-300, 300), asp = 1)
#  plot(get("sap.poly", parent.frame(2)), add = T)
#  lapply(out, plot, add = T)
#  plot(out[[1]], add = T)
#  plot(wrld, add = T, border = "red") # je v sandbox.R
  
# > summary(out) # seznam walkerjev
#    	Length Class        Mode
#	2   1      SpatialLines S4  
#	3   1      SpatialLines S4  
#	8   1      SpatialLines S4  
#	31  1      SpatialLines S4
  
}

