#' Extract results from markAnalysis()
#' 
#' @param x List. As returned by the markAnalysis() function.
#' @param lf List of capture history/metadata files as produced by writeINP() function.
#' @param ... Parameters passed to fun.

calculateIndices <- function(x, lf) {
  # calculate TIRM model by Miller 2005 and add it to the mix
  mrk <- lf[grepl(x$simulation.pars$seed, lf)]
  ch <- readMark(mrk)
  info <- read.table(mrk, skip = 3, nrows = 1, header = TRUE)
  ch$count <- sapply(strsplit(ch$ch, ""), FUN = function(y) sum(as.numeric(y)))
  
  mdl <- fitTirm(data = buildClassTable(ch$count), max.pop = info$generated_walkers)
  info$N.tirm <- mdl$ml.pop.size
  x$simulation.pars <- info
  
  # add population size estimates and the difference
  size.1 <- x$est.der.pars$N.Population.Size.estimate[1]
  size.sp <- x$est.der.pars$N.Population.Size.estimate[2]
  size.est.diff <- diff(x$est.der.pars$N.Population.Size.estimate)
  size.tirm <- x$simulation.pars$N.tirm
  
  # add capture probability esimates and the difference
  p.1 <- x$real.fun.pars$estimate[1]
  p.sp <- x$real.fun.pars$estimate[2]
  p.diff <- diff(x$real.fun.pars$estimate)
  p.target.1 <- x$simulation.pars$capture_prob - x$simulation.pars$p.1
  p.target.sp <- x$simulation.pars$capture_prob - x$simulation.pars$p.sp
  p <- x$simulation.pars$capture_prob
  
  # dAIC <- x$deltaAIC # negative value means .sp bigger than .1
  better.model <- ifelse(x$real.fun.pars$AICc[1] > x$real.fun.pars$AICc[2], ".1", ".sp")
  dAIC <- x$deltaAIC
  area <- x$simulation.pars$sampling_area_r
  
  # naive sap area
  area.naive <- pi * area^2
  # SAP enlarged for home range
  area.hr <- pi * (x$simulation.pars$home_range + area)^2
  # sap enlarged for max walked distance
  area.effect <- pi * (x$simulation.pars$effect_distance + area)^2
  
  # based on quantile function, calculate distance for given quantiles
  if (x$simulation.pars$sim.dist == "empirical") {
    sigma <- unlist(x$simulation.pars$hazard_fun_sigma)
    b <- unlist(x$simulation.pars$hazard_fun_b)
    mx <- unlist(x$simulation.pars$hazard_fun_mx)
    set.seed(x$simulation.pars$seed)
    qs <- getQcustom(fnc = weibullLikeDistribution, sigma = sigma, b = b, mx = mx, xrange = c(0, sigma * 3))
  }
  
  if (x$simulation.pars$sim.dist == "normal") {
    SD <- x$simulation.pars$SD # preveir, da dela
    qs <- getQnormal(mu = 0, sd = SD)
  }
  
  area.50 <- pi * (area + qs["50%"])^2
  area.60 <- pi * (area + qs["60%"])^2
  area.70 <- pi * (area + qs["70%"])^2
  area.80 <- pi * (area + qs["80%"])^2
  area.90 <- pi * (area + qs["90%"])^2
  area.95 <- pi * (area + qs["95%"])^2
  area.99 <- pi * (area + qs["99%"])^2
  
  # true density
  dens.true <- pi * x$simulation.pars$area_size^2
  dens.true <- x$simulation.pars$generated_walkers / dens.true
  
  # calculate density given (enlarged) sampling area
  dens.naive.1 <- size.1 / area.naive
  dens.hr.1 <- size.1 / area.hr
  dens.effect.1 <- size.1 / area.effect
  dens.50.1 <- size.1 / area.50
  dens.60.1 <- size.1 / area.60
  dens.70.1 <- size.1 / area.70
  dens.80.1 <- size.1 / area.80
  dens.90.1 <- size.1 / area.90
  dens.95.1 <- size.1 / area.95
  dens.99.1 <- size.1 / area.99
  
  dens.naive.sp <- size.sp / area.naive
  dens.hr.sp <- size.sp / area.hr
  dens.effect.sp <- size.sp / area.effect
  dens.50.sp <- size.sp / area.50
  dens.60.sp <- size.sp / area.60
  dens.70.sp <- size.sp / area.70
  dens.80.sp <- size.sp / area.80
  dens.90.sp <- size.sp / area.90
  dens.95.sp <- size.sp / area.95
  dens.99.sp <- size.sp / area.99
  
  dens.naive.tirm <- size.tirm / area.naive
  dens.hr.tirm <- size.tirm / area.hr
  dens.effect.tirm <- size.tirm / area.effect
  dens.50.tirm <- size.tirm / area.50
  dens.60.tirm <- size.tirm / area.60
  dens.70.tirm <- size.tirm / area.70
  dens.80.tirm <- size.tirm / area.80
  dens.90.tirm <- size.tirm / area.90
  dens.95.tirm <- size.tirm / area.95
  dens.99.tirm <- size.tirm / area.99
  
  # calculate index ((D - D^)/D) * 100
  # calIn <- function(Dh, D = dens.true) ((D - Dh)/D) * 100
  calIn <- function(Dh, D = dens.true) ((D-Dh))
  
  dens.naive.1 <- calIn(Dh = dens.naive.1)
  dens.hr.1 <- calIn(dens.hr.1)
  dens.effect.1 <- calIn(dens.effect.1)
  dens.50.1 <- calIn(dens.50.1)
  dens.60.1 <- calIn(dens.60.1)
  dens.70.1 <- calIn(dens.70.1)
  dens.80.1 <- calIn(dens.80.1)
  dens.90.1 <- calIn(dens.90.1)
  dens.95.1 <- calIn(dens.95.1)
  dens.99.1 <- calIn(dens.99.1)
  
  dens.naive.sp <- calIn(dens.naive.sp)
  dens.hr.sp <- calIn(dens.hr.sp)
  dens.effect.sp <- calIn(dens.effect.sp)
  dens.50.sp <- calIn(dens.50.sp)
  dens.60.sp <- calIn(dens.60.sp)
  dens.70.sp <- calIn(dens.70.sp)
  dens.80.sp <- calIn(dens.80.sp)
  dens.90.sp <- calIn(dens.90.sp)
  dens.95.sp <- calIn(dens.95.sp)
  dens.99.sp <- calIn(dens.99.sp)
  
  dens.naive.tirm <- calIn(dens.naive.tirm)
  dens.hr.tirm <- calIn(dens.hr.tirm)
  dens.effect.tirm <- calIn(dens.effect.tirm)
  dens.50.tirm <- calIn(dens.50.tirm)
  dens.60.tirm <- calIn(dens.60.tirm)
  dens.70.tirm <- calIn(dens.70.tirm)
  dens.80.tirm <- calIn(dens.80.tirm)
  dens.90.tirm <- calIn(dens.90.tirm)
  dens.95.tirm <- calIn(dens.95.tirm)
  dens.99.tirm <- calIn(dens.99.tirm)
  
  out <- data.frame(size.1, size.sp, # estimated sizes
                    size.est.diff, # difference in estimated population sizes
                    p.diff, # difference in estimated p
                    p, # true p
                    better.model, # which model performs better
                    dAIC, # difference in AICc between models
                    area.naive, # area of sap
                    area.hr, # area of sap enlarged for home range
                    area.effect, # area of sap enlarged for max pairwise distance recorded
                    area.50, area.60, area.70, area.80, area.90, area.99, # sap enlarged for quantile
                    fun = x$simulation.pars$sim.dist,  # empirical or normal
                    hr = x$simulation.pars$home_range, # simulated home range size
                    num.generated.walkers = x$simulation.pars$generated_walkers, # num. of sim. walkers
                    num.captured.walkers = x$simulation.pars$num_of_sampled_walkers, # num. of sampled walkers
                    sessions = x$simulation.pars$sessions, # number of sessions
                    max.walk.dist = x$simulation.pars$effect_distance, # max walked distance
                    # deviation from true density given expanded sampling areas
                    dens.naive.1, dens.hr.1, dens.effect.1, dens.50.1, dens.60.1, dens.70.1, dens.80.1,
                    dens.90.1, dens.95.1, dens.99.1,
                    dens.naive.sp, dens.hr.sp, dens.effect.sp, dens.50.sp, dens.60.sp, dens.70.sp, dens.80.sp,
                    dens.90.sp, dens.95.sp, dens.99.sp,
                    dens.naive.tirm, dens.hr.tirm, dens.effect.tirm, dens.50.tirm, dens.60.tirm, dens.70.tirm,
                    dens.80.tirm, dens.90.tirm, dens.95.tirm, dens.99.tirm
  )
  
  rownames(out) <- NULL
  out
}