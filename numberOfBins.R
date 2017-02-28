#' Calculate the maximum number of possible bins.
#' 
#' @param object RasterLayer object used in the simulation.
#' @param walk.pair A vector of pair-wize distances used to fetch effect.distance.
#' @author Roman Luštrik

numberOfBins <- function(...object, walk.pair) {

	effect.distance <- max(unlist(walk.pair))
	op <- matrix(c(0, 0), ncol = 2) # celica okoli katere se bo našlo buffer
	
	...object[] <- 1:ncell(...object)
	#find cell numbers
	cell.num <- extract(x = ...object, y = op, buffer = effect.distance)[[1]]
	#find coords of cell numbers for pointDistance() function
	cell.xy <- xyFromCell(object = ...object, cell = cell.num) 
	#get cell distance values
	cell.dist <- raster::pointDistance(p1 = op, p2 = cell.xy,
			longlat = FALSE) 
	# Find number of cells between origin and the rim of the contribution area.
	origin.point.plus.ed <- matrix(c(op[1], op[2]+ effect.distance), ncol = 2)
	my.line <- SpatialLines(list(Lines(list(Line(rbind(op, origin.point.plus.ed))), ID = 1)))
	num.bins <- nrow(extract(x = ...object, y = my.line, cellnumbers = TRUE)[[1]])
	# do some garbage collection
	
	return(num.bins)
}