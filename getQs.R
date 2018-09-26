#' Get (simulated) quantiles from two dimensional normal distribution.
#' 
#' @param SD Numeric. Based on what standard deviations the 2D quantile is to be calculated.
#' @param p Numeric. Probability at which the 2D normal distribution quantile is to be calculated.
#' 
#' http://stats.stackexchange.com/a/36023/144
#' https://stats.stackexchange.com/a/127486/144
getQnormal <- function(probs, SD) {
  # browser()
  N <- 10000
  xy <- data.frame(x = rnorm(N, mean = 0, sd = SD),
                   y = rnorm(N , mean = 0, sd = SD))
  
  es <- eigen(cov(xy))
  e1 <- es$vec %*% diag(sqrt(es$val))
  r1 <- sqrt(qchisq(probs, 2)) # 1 SD
  theta <- seq(0, 2 * pi, len = 2000)
  v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
  pts <- t(colMeans(xy) - (e1 %*% t(v1)))
  
  colnames(pts) <- c("x", "y")
  sp.krog2 <- SpatialPolygons(list(Polygons(list(Polygon(pts)), ID = 1)))
  plot(xy, asp = 1, axes = TRUE)
  lines(pts, col = "blue", lwd = 2)
  abline(v = range(pts[, "x"]), col = "red")
  abline(h = range(pts[, "y"]), col = "red")
  
  out <- diff(range(pts[, 1]))/2 # assumes symmetrical 2D normal distribution
  names(out) <- paste(probs * 100, "%", sep = "")
  out
}


#' Get quantiles from arbitrary distribution
#' 
#' Quantiles given probability are calculated numerically using rejection sampling. See my post
#' on [Crossvalidated](https://stats.stackexchange.com/a/278972/144).
#' 
#' @param fnc Function (of class `function`) for which the quantiles are to be found. First argument 
#' should take a vector of values from the x dimension.
#' @param ... Any possible parameters passed to the function.
#' @param xrange A list of range boundaries on x axis (min, max) for which the function is to be sampled.
#' @param N Number of samples to be taken from the distribution. Number of final samples will
#' be less (or equal, but unlikely) than `N`.
#' @probs Vector of probabilities of desired quantiles.
#' 
#' @author Roman Luštrik (\email{roman.lustrik@@biolitika.si})
#' 
#' @return A vector of quantiles corresponding to the requested `probs`.
#' 
#' @examples
getQcustom <- function(fnc, ..., xrange = NULL, N = 10000, probs = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)) {
  if (is.null(xrange)) stop("xrange argument missing")
  
  xy <- data.frame(proposed = runif(N, min = xrange[[1]], max = xrange[[2]]))
  xy$fit <- fnc(xy$proposed, ...)
  xy$fit <- xy$fit/max(xy$fit) # scale values to be between 0 and 1 for the next step
  xy$random <- runif(N, min = 0, max = 1)
  
  xy$accepted <- with(xy, random <= fit)
  xy.out <- xy[xy$accepted, ] # retain only those values that are "below" the custom distribution
  
  quantile(xy.out$proposed, probs = probs)
}
