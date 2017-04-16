#' This function finds individual contribution for each walker.
#' 
#' @param walk A \code{list} of \code{data.frame}s with sampled coordinates of
#' 				of individual walkers.
#' @param ...object RasterLayer. Base object used in the simulation.
#' @param .sap.poly SpatialPolygons. A \code{SpatialPolygons} object of
#' 				the sampling area.
#' @param effect.distance Numeric. A vector of length 1 noting the maximum
#' 				distance used to calculate contribution area.
#' @param bins A data.frame from function `calculateBins`. Data from this source will be used to construct
#' a parametric curve based on half normal distribution with some additional parameters which aid in
#' achieving a better fit with regards to longer plateau on the left side of the function. See inline
#' code comments for more info.
#' @return A list of contribution values corresponding to individual walker.
#' @author Roman Luštrik

individualContribution <- function(walk, ...object, .sap.poly, effect.distance, bins,
                                   sim.dist, SD, area) {
  
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
  
  message(paste("Preparing rasters to calculate individual contribution, starting at:", Sys.time(), sep = " "))
  
  # Create mask that will be used to subset a binned raster. sampling.area.poly
  # is added to the empty.object raster and all values inside the polygon will
  # serve as non-NA vales when masking.
  raster.mask <- rasterize(x = .sap.poly, y = empty.object)
  
  ...object[] <- 1:ncell(...object)
  
  if (sim.dist == "normal") {
    message(sprintf("Starting to process contribution based on normal distribution for %d walkers.", length(medoids)))
    
    
    rres <- res(...object)[1]
    yticks <- xticks <- seq(from = xmin(extent(...object)), to = xmax(extent(...object)) - rres, by = rres)
    side <- max(res(...object)) # cell size
    
    # For each medoid, calculate its contribution inside the sampling area.
    ic <- sapply(medoids, FUN = function(xy, xticks, yticks, side, sap, SD) {
      xy <- as.numeric(xy)
      
      mat <- calcNormal2D(x = xticks, y = yticks, side = side, mu1 = xy[1], mu2 = xy[2],
                          s1 = SD, s2 = SD)
      mat <- raster(x = mat, template = raster.mask)
      
      cont.sum.ind <- sum(mat[])
      
      # browser()
      # plot(mat)
      # plot(sap, add = TRUE)
      
      cont.in.sap <- raster::mask(x = mat, mask = raster.mask)
      out <- sum(cont.in.sap[], na.rm = TRUE)/cont.sum.ind
      
      out
      
    }, xticks = xticks, yticks = yticks, side = side, sap = .sap.poly, SD = SD)
    
    # make output the same as for non-normal contribution
    out <- vector("list", 1)
    out[[1]] <- as.list(ic)
    names(out) <- "weight.yes"
    return(out)
  }
  
  if (sim.dist == "empirical") {
    # Fit a model to the distribution of paired distances.
    xd <- bins$bins$weight.yes
    mdl <- nls(mean ~ hazardFunction(x = bins, sigma, b, mx), data = xd, 
               start = list(sigma = mean(xd$bins),               # variance
                            b = max(xd$bins)/(max(xd$bins)/2),   # scale parameter
                            mx = xd$mean[1]))                    # height, since function is not scaled   
    prs <- mdl$m$getPars() # get estimated parameters, could also use prs <- summary(mdl)
    
    pdf("fit_check.pdf")
    plot(mean ~ bins, data = xd, main = sprintf("sigma: %f, xm = %f, b = %f", prs["sigma"], prs["mx"], prs["b"]))
    curve(hazardFunction(x, sigma = prs["sigma"], b = prs["b"], mx = prs["mx"]), 
          from = 0, to = 200, n = 200, add = TRUE, col = "red")
    dev.off()
    
    # Calculate distance for every cell.
    ic <- sapply(medoids, FUN = function(xy, object, sigma, b, mx) {
      dist.mat <- distanceFromPoints(object = object, xy = xy)
      
      # And based on the model above and distance from walker's medoid, find value of each cell.
      dist.mat[] <- hazardFunction(x = dist.mat[], sigma = sigma, b = b, mx = mx)
      
      # plot(dist.mat, asp = 1, xlim = c(-500, 500), ylim = c(-500, 500))
      # plot(.sap.poly, add = TRUE)
      
      # Scale values between 0 and 1 (trust me on this one).
      sx <- dist.mat[]
      dist.mat[] <- (sx - min(sx))/(max(sx) - min(sx))
      
      # and calculate proportion of walker's "home range" inside the sampling areas.
      prop.all <- sum(dist.mat[], na.rm = TRUE)
      prop.in.sap <- raster::mask(x = dist.mat, mask = raster.mask)
      
      sum(prop.in.sap[], na.rm = TRUE)/prop.all
    }, object = ...object, sigma = prs["sigma"], b = prs["b"], mx = prs["mx"])
    
    out <- vector("list", 1)
    out[[1]] <- as.list(ic)
    names(out) <- "weight.yes"
    return(out)
  }
}