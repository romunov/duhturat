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
#' @param ring.weights
#' @param empty.object
#' @param raster.mask Numeric or RasterLayer object. Used to mask out only the desired
#'       portion of the base layer. 
#' @return Numeric. Contribution (a proportion) of area inside the sampling area.
#' @author Roman Luštrik, 22.11.2010
# mask	Logical. If TRUE, raster is masked (using raster.mask)
# contr	Logical. If TRUE, cells are summed. If you do not mask values, the re-
#			turned values will be 1.
#
# Author: Roman Luštrik, 22.11.2010
###############################################################################

calcDistance <- function(origin.point, object, effect.distance, ring.weights,
  empty.object, raster.mask, mask = FALSE, contr = FALSE) {
  
  # Some functions may pass origin.point as data.frame/matrix, while others
  # pass a vector. We need to perform a check and coerce to the correct
  # class if needed, otherwise raster::extract will crap out.
  if (class(origin.point) == "numeric") {
    origin.point <- matrix(origin.point, ncol = 2)
  }
  
  # Take an origin point and find all cells within its buffer range.
#	cell.num <- unique(unlist(extract(x = object, y = origin.point,
#							buffer = effect.distance)))
  # note: effect distance is actually 2r, so we need to divide it by 2
  cell.num <- unique(unlist(extract(x = object, y = origin.point,
        buffer = effect.distance/2)))
  
  # to each cell inside the estimated home.range, assign a value
  # that is 1/n-th of the total number of cells. All cells within
  # the estimated home.range should sum to 1 (see proof)
  empty.object[cell.num] <- 1/length(cell.num)

  # proof:
#  > sum(empty.object[], na.rm = TRUE)
#  [1] 1
#  plot(empty.object)
#  plot(get("sap.poly", parent.frame(8)), add = TRUE)
#  points(get("xy", parent.frame(8)))
  # see also line with text()
  
  # Apply mask to cut out all the values
  if (mask) {
    empty.object <- raster::mask(x = empty.object, mask = raster.mask)
  }
  
#  text(x = -119, y = 136, labels = sum(empty.object[], na.rm = TRUE))
  
  # Calculate contribution
  if (mask & contr) {
    sum(empty.object[], na.rm = TRUE)
  } else {
    empty.object
  }
}