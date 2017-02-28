#' This function calculates distance from a point within a specified radius.
#' It returns a raster object. Technically, this function finds which cells
#' are within the radius of effect.distance from origin point, get their co-
#' ordinates and uses pointDistance() to calculate their value based on distance
#' from origin point.
#' 
#' # This function is different from calcDistance only by the way the masking is
#' done. calcDistance uses raster::mask while here, we just subset using vector
#' indexing.
#' 
#' @param origin.point Matrix. A 1x2 matrix (can be extracted from a bigger n*2 matrix 
#'			with [i, ] operator) denoting coordinates of the origin points from
#'			where distances will be calculated.
#' @param object RasterLayer object where drama takes place. Must be raster with 
#'			values \code{1:ncell(raster)}. This is because we use extract() to 
#'			extract a buffer around the origin point, which reads only values,
#'			and not their position.
#' @param effect.distance Numeric. A number specifying the radius from the origin point.
#'			All points within this radius will be used to calculate distances. 
#'			WARNING: If effect distance is bigger than area/2, buffer falls 
#'			outside raster range and xyValues() fails. The function should return
#'			a vector (because of unlist) of values, but when at least one value
#'			falls outside the raster range, only one NA is returned. This will
#'			result in an epic function fail.
#' @param ring.weights A vector of values of the curve used to construct the
#' 			contribution area.
#' @param empty.object An empty RasterLayer object into which the result is inserted.
#' 			Must be of the same dimensions as \code{object}.
#' @param raster.mask Numeric or RasterLayer object. Used to mask out only the desired
#'       portion of the base layer. 
#' @return Numeric. Contribution (a proportion) of area inside the sampling area.
#' @author Roman Luštrik, 22.11.2010

# This function is different from calcDistance only by the way the masking is
# done. calcDistance uses raster::mask while here, we just subset using vector
# indexing.

calcDistanceNumeric <- function(origin.point, object, effect.distance, ring.weights,
		empty.object, raster.mask) {
	# Some functions may pass origin.point as data.frame/matrix, while others
	# pass a vector. We need to perform a check and coerce to the correct
	# class if needed, otherwise raster::extract will crap out.
	if (class(origin.point) == "numeric") {
		origin.point <- matrix(origin.point, ncol = 2)
	}
	
	# Take an origin point and find all cells within its buffer range.
	cell.num <- unique(unlist(extract(x = object, y = origin.point,
							buffer = effect.distance)))
	
	#we need coordinates of cells for pointDistance() function
	cell.xy <- xyFromCell(object = object, cell = cell.num) 
	
	#we get cell distance values - put them on a raster
	cell.dist <- raster::pointDistance(p1 = origin.point, p2 = cell.xy,
			longlat = FALSE) 
	
	# Cut the distances into intervals.
	raster.cut <- cut(cell.dist, breaks = length(ring.weights), labels = FALSE) 
	# each bin (interval) is denoted by a 
	# unique number, use labels = NULL to see the intervals. I have
	# added a zero to breaks to make the damn thing even work.
	# Another option worth exploring is adding -Inf and Inf at both ends of
	# the breaks. Testing required.
	
	# Swap cut values for weights and normalize.
	raster.cut <- ring.weights[][as.factor(raster.cut)]
	raster.cut <- raster.cut / sum(raster.cut)
	
	# And write to an empty raster object.
	empty.object[cell.num] <- raster.cut

#eo <- empty.object
#eo <- eo *1000
#eo[is.na(eo)] <- 0
#plot(eo)
#plot3D(eo)
#plot(1:29, get("ring.weights", parent.frame(8))$weight.yes[, "mean"])
#plot(eo)
#plot(get("sap.poly", parent.frame(8)), add = T)

	# extract raster.mask cells and sum them
#	return(sum(empty.object[][raster.mask], na.rm = TRUE))
	sum(empty.object[][raster.mask], na.rm = TRUE)
}