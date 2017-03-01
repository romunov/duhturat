#' Correction weights. Calculates the proportion of distances that get lost
#' because of the sampling area size. Creates N random points within the study
#' area, makes a single step from them (from 0 to max.step) with a uniform dis-
#' tribution at a random angle. Output is the vector of step sizes and whether
#' the second point is inside or not.
#' 
#' @param sample.area	Polygon. A polygon of sampling area.
#' @param max.step	Numeric. Maximal step length.
#' @param gens Numeric. Number of points generated inside the sampling.area.
#' @return A \code{data.frame} of \code{steps} and \code{p.inside}.
#' 
#' @author Tomaž Skrbinšek, adapted by Roman Luštrik, 10.12.2010

distWeights <- function(gens = 10000, max.step, sampling.area) {	

	# Create and define the start points, angles and distances that will be
	# used to construct end points.
	#extract matrix to be able to feed csr

  # sampling.area.matrix <- sampling.area@polygons[[1]]@Polygons[[1]]@coords 
  sampling.area.matrix <- geom(sampling.area)[, c("x", "y")] # extract coordinates
	start.points <- csr(sampling.area.matrix, gens)
	colnames(start.points) <- c("x", "y")
	angles <- runif(gens, min = 0, max = 2*pi)
	steps <- runif(gens, min = 0, max = max.step)
	
	end.points <- matrix(ncol = 2, nrow = gens)
	end.points[, 1] <- start.points[, 1] + steps * sin(angles) #x coords
	end.points[, 2] <- start.points[, 2] + steps * sin(angles) #y coords
	
	# Extract matrix of values to be able to feed the point.in.polygon function
	p.inside <- point.in.polygon(point.x = end.points[, 1],
			point.y = end.points[, 2], pol.x = sampling.area.matrix[, 1],
			pol.y = sampling.area.matrix[, 2])

	result <- data.frame(steps = steps, p.inside = p.inside)
	return(result)
	
}
