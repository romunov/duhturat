#' Function calculates weights for distances from the walker's home range
#' centroid.
#' @author Roman Luštrik

weighDistances <-  function(bins, sap.poly, walk.pair, num.boots,
  weight.switch) {
  
  walk.pair <- round(walk.pair, 1) # round to 1 because bins are rounded to 1
  
  bins <- c(bins)
  weight <-  rep(NA, length(bins))
  out <- list() # store results
  
  # Bootstrap pairwise distances and construct a curve from mean and mean +- SE
  boot.wp <- sapply(1:num.boots, function(x, wp) {
      sort(sample(x = wp, size = length(wp), replace = TRUE))
      #reality check works
      #sort(sample(1:10, 10, replace = TRUE))
    }, wp = walk.pair, simplify = FALSE)
  
  boot.bins <- lapply(boot.wp, FUN = function(x, bins) {
      ww <- rep(NA, length(bins))
      # Check how many distances fall inside weighted bins.
      for (i in 1:length(bins)) {
        ww[i] <- length(x[x >= bins[i]])
      }
      return(ww)
    }, bins = bins)
  
  boot.bins <- do.call("rbind", boot.bins)
  
  # Calculate mean, sd, upper and lower bound. qt calculates
  # number of sd for 95% CI.
  boot.mean <- apply(X = boot.bins, MARGIN = 2, mean)
  boot.sd <- apply(X = boot.bins, MARGIN = 2, sd)
  boot.lower <- boot.mean - qt(0.975, df = num.boots) * boot.sd
  boot.upper <- boot.mean + qt(0.975, df = num.boots) * boot.sd
  
#	# Comment this out if you don't want a plot for every simulation that you run.
  # pdf(width = 7, height = 5)
  # plot(1:length(boot.mean), boot.mean, type = "n")
  # lines(apply(boot.bins, 1, lines, col = "light gray"))
  # lines(boot.mean, lwd = 2)
  # lines(boot.lower, lty = "dashed")
  # lines(boot.upper, lty = "dashed")
  # mtext("95% CI", side = 4, adj = 1)
  # dev.off()
  
  # Give weight.walk weights. If weight.switch == "weights" was used, a set
  # of weights is applied, but if "raw" is used, no weights are applied. 
  out$weight.no <- data.frame(bins = bins, weight = weight, lower = boot.lower,
    mean = boot.mean, upper = boot.upper)
  
  # Lower CI can (sometimes?) be below zero. Convert any value
  # that is below 0 to 0.
  out$weight.no$lower[out$weight.no$lower < 0] <- 0
  
  # WEIGHT #
  
  # Calculate the frequency of in/out segments in a particular bin.
  # We need to add zero to the bins to make things work (because of [i-1]).
  
  #	browser() #BROWSER
  
  # Switch that enables/disables curve adjusting via weights.
  # Result of this function is object weight
  if (weight.switch) {
    
    bins <- c(0, bins)
    weight <-  rep(NA, length(bins))
    
    # Generate weights for pairwise distances (adjusted curve).
    steps <- distWeights(gens = 10000, max.step = max(walk.pair),
      sampling.area = sap.poly)
    
    # We extract all distances and distances that have fallen outside the sampling area.
    distances <- steps[1]
    distances.out <- steps[steps[2] == 0, ]
    distances.out <- distances.out[1]
    
    # Apply weights to bins according to the number of distances that have fallen in/out.
    for (i in 2:length(bins)) { #start from 2, because 1 has the values of zero and will be removed at the end
      a <- length(distances.out[distances.out <= bins[i] &
            distances.out > bins[i-1]]) # number of distances that have fallen outside the sampling area in a particular bin ([i-1]!)
      b <- length(distances[distances <= bins[i] & distances > bins[i-1]]) # number of distances in that particular bin
      weight[i] <- a / b
    }
    
    rm(steps); rm(a); rm(b); rm(distances.out);	rm(distances) #some gc
    bins <- bins[-1]
    weight <- weight[-1]
    
    ## out is a weight.no object. We copy it, add a proper weight column and
    ## apply weights to lower/mean/upper columns.
    out$weight.yes <- out$weight.no
    out$weight.yes$weight <- weight
    yes <- out$weight.yes[, c("lower", "mean", "upper")]
    yes <- yes + yes * weight
    out$weight.yes[, c("lower", "mean", "upper")] <- yes
    
    ################################
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
    ################################
    # uncomment the next line if you want
    # to include weight.no into the final output
    # I've decided on 8/3/2012 to remove the weight.no
    # to make the simulation a bit lighter
    out$weight.no <- NULL
  } 
  
  return(out)
}