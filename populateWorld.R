#' This function generates walkers inside and around the future to be sampling area.
#' 
#' See \code{simulation} for the description of other parameters.
#' 
#' @param num.walkers Number of walkers to generate.
#' @param area The amount of offset (width) of the sampling area polygon (sap).
#' @param home.range Radius of each walker's home range by which the centroid will be expanded. This is used later on
#' to calculate if a walker is included in theoretical density or not.
#' @param sap SpatialPolygons object which represents the area where sampling is takign place.
#' @param custom.walkers If a data.frame (x, y coordinates) of custom walkers is being passed in, this is the place
#' to do it.
#' @returnType List.
#' @return A list of walkers.
#' @author Roman Luštrik

populateWorld <- function(num.walkers, area, home.range, sap, custom.walkers) {
  
  if (!is.null(custom.walkers)) {
    xy <- custom.walkers
  } else {
    # Simulate walkers around SAP.
    expand.sap <- gBuffer(sap, width = area)
    xy <- as.data.frame(spsample(expand.sap, n = num.walkers, type = "random"))
    plot(0,0, type = "n", xlim = c(-800, 800), ylim = c(-800, 800), asp = 1)
    points(xy)
    plot(sap, add = TRUE, border = "blue", lwd = 2)
    plot(expand.sap, add = TRUE, border = "red")
  }
  
  message("Walkers near the sampling area: ", nrow(xy))
  
  # Create home range area for each walker.
  out <- apply(xy[, c("x", "y")], MARGIN = 1, FUN = function(xy, home.range) {
    dot <- SpatialPoints(coordinates(matrix(c(xy[1], xy[2]), ncol = 2)))
    random.walk <- gBuffer(spgeom = dot, width = home.range, quadsegs = home.range * 5)
    random.walk
  }, home.range = home.range)
  
  names(out) <- paste(1:length(custom.walkers[, "capt"]), custom.walkers[, "capt"], sep = "_")
  out
  
  # plot(0,0, type = "n", xlim = c(-500, 500), ylim = c(-500, 500), asp = 1)
  # plot(sap, add = T)
  # points(xy)
  # plot(expand.sap, add = TRUE, border = "red")
  # # lapply(out, plot, add = T)
  # # plot(out[[1]], add = T)
  # # plot(wrld, add = T, border = "red") # je v sandbox.R
  
  # > summary(out) # seznam walkerjev
  #    	Length Class        Mode
  #	2   1      SpatialLines S4  
  #	3   1      SpatialLines S4  
  #	8   1      SpatialLines S4  
  #	31  1      SpatialLines S4
  
}

