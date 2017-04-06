#' This function will calculate bins, bootstrap, effect distance and ring weights.
#' 
#' @param walk.pair	Numeric. A vector of pair-wise distances.
#' @param num.bins	Numeric. Number of bins.
#' @param num.boots	Numeric. Number of bootstrap iterations performed.
#'
#' @author Roman Luštrik, 18.10.2010

calculateBins <- function(walk.pair, num.bins, sap.poly, ...num.boots,
                          weight.switch) {
  
  # Generate bins based on pairwise distances.
  bins <- cut(x = walk.pair, breaks = seq(from = 0, to = max(walk.pair),
                                          length.out = num.bins + 1))
  bins <- levels(bins)
  bins <- sub("^.*\\,", "", bins)
  bins <- as.numeric(substr(bins, 1, nchar(bins) - 1))
  
  # Calculate weights for bins according to the percent of walks that fall
  # outside the range. Result is a data.frame with original bins, its weights
  # and weighted bins.
  bins <- weighDistances(bins = bins, sap.poly = sap.poly,
                         walk.pair = walk.pair, num.boots = ...num.boots, weight.switch = weight.switch)
  
  # Cut-off (threshold) value for effect distance around sampled points.
  # You need to be careful that the effect distance does not "fall off of 
  # the world". This is solved by creating large enough margins between the
  # sampling area and the "world".
  effect.distance <- max(walk.pair)
  
  # This will make all bins uniform.
  uniform <- lapply(bins, function(x) {
    x[-1] <- 1
    x
  })
  
  output <- list(
    # bins = uniform,
    bins = bins,
    effect.distance = effect.distance
  )
  
  output
}

# $bins
# $bins$weight.yes
#   bins      weight      lower      mean     upper
# 1 0.726 0.000622665 61.5206076 67.908258 74.295908
# 2 1.450 0.001805054 35.9910417 44.966020 53.940998
# 3 2.180 0.004683841 12.3039497 20.018326 27.732701
# 4 2.910 0.005689001  2.9220037  8.187314 13.452625
# 5 3.630 0.009546539  0.3209117  4.145198  7.969484
# 6 4.360 0.015421115  0.0000000  1.025575  3.087672
# 
# 
# $effect.distance
# [1] 4.358369