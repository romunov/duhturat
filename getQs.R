#' Get quantiles from Normal distribution
#' 
#' @param mu Parameter of mean of the distribution.
#' @param sd Parameter of standard deviation of the distribution.
#' @param probs Vector of probabilities of desired quantiles.
#' 
#' @author Roman Luštrik (\email{roman.lustrik@@biolitika.si}).
#' 
#' @return A vector of quantiles corresponding to the requested `probs`.
#' 
#' @examples
#' qnorm(0.5) # at probability of 0.5, the quantile is 0

getQnormal <- function(mu, sd, probs = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)) {
  out <- qnorm(p = probs, mean = mu, sd = sd)
  names(out) <- sprintf("%0.f%%", probs * 100)
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
#' 

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