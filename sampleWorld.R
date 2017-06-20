sampleWorld <- function(walk, sap.poly, sessions, prob, SD) {
  
  walk.sample <- sampleWalkers(walk = walk, sessions = sessions, prob = prob,
                               sap = sap.poly, SD = SD)
  
  # Calculate pairwise distances within each walker's sampled points. 
  walk.pair <- lapply(X = walk.sample[["sample"]], FUN = function(x) {
    remove.na <- x[x$capt == 1, ]
    pairwise.distances <- dist(x = remove.na, method = "euclidean")
    out <- as.numeric(pairwise.distances)
    out
  })
  
  #	walk.sample$in.out <- length(walk.subset)
  walk.sample$in.out <- length(walk)
  walk.sample$walk.pair <- unlist(walk.pair)
  #  walk.sample$actual.ratio <- actual.ratio
  
  walk.sample
}


