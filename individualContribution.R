#' This function finds individual contribution for each walker.
#' 
#' @param walk A \code{list} of \code{data.frame}s with sampled coordinates of
#' 				of individual walkers.
#' @param ...object RasterLayer. Base object used in the simulation.
#' @param .sap.poly SpatialPolygons. A \code{SpatialPolygons} object of
#' 				the sampling area.
#' @param effect.distance Numeric. A vector of length 1 noting the maximum
#' 				distance used to calculate contribution area.
#' @param ring.weights A vector of values of the curve used to construct the
#' 			contribution area.
#' @param ... Parameters passed to \code{snowfall:::sfInit}.
#' @return A list of contribution values corresponding to individual walker.
#' @author Roman Luštrik

individualContribution <- function(walk, ...object, .sap.poly, effect.distance, 
                                   ring.weights, ...) {
  
  if (missing(effect.distance)) stop("No effect.distance argument in individualContribution")
  
  empty.object <- ...object #store empty object to be passed to later functions
  
  # For each walker, find a medoid point of all sampled points.
  medoids <- lapply(X = walk, FUN = function(x) {
    remove.na <- x[x$capt == 1, ]
    #      remove.na <- x[complete.cases(x), ]
    # if there's only one sample,
    # output that one row
    if (nrow(remove.na) < 2) {
      return(remove.na[, c("x", "y")])
    } else {
      calc.medoid <- pam(x = remove.na[c("x", "y")], k = 1)$medoids
      return(calc.medoid)
    }
  })
  
  message(paste("Started calculating individual contribution at:", Sys.time(), sep = " "))
  
  # Create mask that will be used to subset a binned raster. sampling.area.poly
  # is added to the empty.object raster and all values inside the polygon will
  # serve as non-NA vales when masking.
  raster.mask <- rasterize(x = .sap.poly, y = empty.object)
  
  ...object[] <- 1:ncell(...object)
  
  # For weights.yes and weights.no, calculate how much each walker contributes
  # to the sampling area if different accumulation curves (lower and upper CI,
  # mean) is used.
  list.of.meco <- lapply(ring.weights, FUN = function(w, .rst = ...object, 
                                                      .effect.distance = effect.distance, .empty.object = empty.object,
                                                      .raster.mask = raster.mask, ...) {
    
    #			iterate.over <- c("lower", "mean", "upper")
    iterate.over <- c("mean")
    
    #      sfInit(parallel = TRUE, ...)
    #      sfExport(list = c("medoids", "...object", "calcDistance", "effect.distance",
    #          "empty.object", "raster.mask"), local = TRUE)
    #      sfLibrary(raster)
    
    #      mecos.lmu <- sfSapply(x = iterate.over, fun = function(x, .medoids = medoids,
    #          .object = .rst, ..effect.distance = .effect.distance,
    #          ..empty.object = .empty.object, ..raster.mask = .raster.mask,
    #          bin.data = w) {
    ##################
    mecos.lmu <- sapply(X = iterate.over, FUN = function(x, .medoids = medoids,
                                                         .object = .rst, ..effect.distance = .effect.distance,
                                                         ..empty.object = .empty.object, ..raster.mask = .raster.mask,
                                                         bin.data = w) {
      ##################
      custom.curve <- bin.data[, x]
      meco <- lapply(X = .medoids, FUN = calcDistance, object = .object,
                     effect.distance = ..effect.distance, ring.weights = custom.curve,
                     empty.object = ..empty.object, raster.mask = ..raster.mask, mask = TRUE,
                     contr = TRUE)
      meco
    })
    
    #      sfStop()
    
    mecos.lmu
  })
  
  list.of.meco # meco = MEdoid COntribution
}