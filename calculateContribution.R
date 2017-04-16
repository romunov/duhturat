#' This function calculates contribution of individual coordinate to the
#' sampling area.
#' 
#' @param walk.pair Numeric. A vector of pairwise distances.
#' @param sap.poly SpatialPolygons. A \code{SpatialPolygons} object of
#' 				the sampling area.
#' @param ..num.boots Integer. Number of iterations in a bootstrap.
#' @param weight.switch Logical. If TRUE, output is a list of length two
#' 				one element holding a \code{data.frame} with weighted and the
#' 				second element containing values for the weighted \code{data.frame}.
#' @param ..object RasterLayer. Base object used in the simulation.
#' @param walks A \code{list} of \code{data.frame}s with sampled coordinates of
#' 				of individual walkers.
#' @param SD Standard deviation used to generate walkers.

calculateContribution <- function(walk.pair, sap.poly, ..num.boots,
                                  weight.switch, ..object, walks, sim.dist, SD, ...) {
  
  # Calculate the number of cells in a radius of the home range and
  # recommend number of bins.
  num.bins <- numberOfBins(...object = ..object, walk.pair = walk.pair)
  
  # Calculate proper bin cuts to construct a curve that will be used
  # to weigh distances from the center.
  my.bins <- calculateBins(walk.pair = walk.pair, num.bins = num.bins, 
                           sap.poly = sap.poly, ...num.boots = ..num.boots,	weight.switch = weight.switch)
  
  # if curves provided, use that
  # input is numeric vector of values
  # function constructs an appropriate data structure
  
  # output of my.bins is a list of two (bins, effect.distance)
  #	List of 2
  #	$ bins           :List of 2
  #	..$ weight.no :'data.frame':	28 obs. of  5 variables:
  #		.. ..$ bins  : num [1:28] 0.952 1.9 2.86 3.81 4.76 5.71 6.66 7.62 8.57 9.52 ...
  #	.. ..$ weight: logi [1:28] NA NA NA NA NA NA ...
  #	.. ..$ lower : num [1:28] 221 218 209 198 191 ...
  #	.. ..$ mean  : num [1:28] 225 223 216 207 200 ...
  #	.. ..$ upper : num [1:28] 229 228 223 216 210 ...
  #	..$ weight.yes:'data.frame':	28 obs. of  5 variables:
  #		.. ..$ bins  : num [1:28] 0.952 1.9 2.86 3.81 4.76 5.71 6.66 7.62 8.57 9.52 ...
  #	.. ..$ weight: num [1:28] 0.00293 0.00514 0.01648 0.02761 0.02597 ...
  #	.. ..$ lower : num [1:28] 222 219 213 204 196 ...
  #	.. ..$ mean  : num [1:28] 226 224 220 213 205 ...
  #	.. ..$ upper : num [1:28] 230 229 227 222 215 ...
  #	$ effect.distance: num 26.7
  
  # Calculate individual contribution of each walker to the sampling area
  # based on
  
  walker.contrib <- individualContribution(walk = walks, ...object = ..object, bins = my.bins,
                                           .sap.poly = sap.poly, effect.distance = my.bins$effect.distance,
                                           sim.dist = sim.dist, SD = SD)
  
  #####################
  #### DIAGNOSTICS ####
  #####################
  # Izračunaj še dve stvari: dejanski delež poti znotraj vzorčnega območja in
  # prispevek v vzorčno območje glede na model.
  
  # prispevek v območje vzorčenja glede na dejanski centroid walkerja
  #NOTE: this is just provisional to test if our "sp" behaves as expected
  # some ugly coding, we're calling objects from global environment
  # we are calculating contribution from original home range centers
  
  #  # diagnostics plotting
  # sp.smp <- walker.contrib[[1]]
  # sp.mdl <- moco[[1]]
  # smp.nms <- as.numeric(unlist(lapply(strsplit(rownames(sp.smp), "_"), "[", 1)))
  # mrg <- cbind(model = sp.mdl[smp.nms, ], sample = sp.smp)
  # plot(mrg)
  # my.lm <- lm(unlist(sp.mdl[smp.nms,]) ~ unlist(sp.smp[, 1]))
  # dev.new()
  # par(mfrow = c(2,2))
  # summary(my.lm)
  # plot(my.lm)
  
  # /diagnostics plotting
  #####################
  #### DIAGNOSTICS ####
  #####################
  
  # output of walker.contrib is a list of two.
  #	List of 2
  #	$ weight.no :List of 834
  #	..$ : num 1
  #	..$ : num 0.999
  #	...
  #	$ weight.yes:List of 834
  #	..$ : num 1
  #	..$ : num 0.999
  #  ...
  
  # Return the output of the simulation.
  out <- list(
    cona = walker.contrib,
    bins = my.bins$bins,
    effect.distance = my.bins$effect.distance
    # model.contrib = moco
  )
  
  ## these names can be used to merge the two data sets together
  #  length(out$cona$weight.yes)
  #  length(out$model.contrib$weight.yes)
  #  out$cona$weight.yes[, 1]
  #  out$model.contrib$weight.yes[, 1]
  #  names(out$model.contrib$weight.yes[, 1])
  #  names(out$cona$weight.yes[, 1])
  
  return(out)
}