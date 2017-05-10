#' Internal function used to calculate proportion of time/volume of a 2D distribution inside the
#' sampling polygon.
#' 
# http://stackoverflow.com/questions/31216151/discrete-approximation-to-a-bivariate-normal-distribution
# and optimized by BrodieG http://chat.stackoverflow.com/transcript/message/36529093#36529093
calcNormal2D <- function(x, y, side, mu1, mu2, s1, s2) {
  x.lo <- pnorm(x - side/2, mu1, s1)
  x.hi <- pnorm(x + side/2, mu1, s1)
  x.prob <- x.hi - x.lo
  
  y.lo <- pnorm(y - side/2, mu2, s2)
  y.hi <- pnorm(y + side/2, mu2, s2)
  y.prob <- y.hi - y.lo
  
  outer(x.prob, y.prob, `*`)
}

#' This is a nonlinear function for fitting a half normal curve to data. It holds
#' two extra parameters. It is a CDF of Weibull distribution.
#' sigma is the standard "dispersion" parameter.
#' b is used to extend the initial plateau before the function starts dropping. 
#' mx parameter is used to scale the function. This is basically the intercept.

CDFWeibull <- function(x, sigma, b, mx) {
  1 - exp(-(x/sigma)^(-b)) * (-mx)
}
