# Chunk 1 load packages and scripts
source("../markAnalysis.R")
source("../readRunModels.R")

library(RMark)

# Chunk 2 import data
data.n <- list.files("../data/normal/", pattern = ".inp", full.names = TRUE)
data.e <- list.files("../data/empirical/", pattern = ".inp", full.names = TRUE)

length(data.n)
length(data.e)

# analyse simulations using MARK
anal.n <- sapply(data.n, FUN = markAnalysis,
                 wd.model = "./mark_intermediate/", simplify = FALSE)

# save(anal.n, file = "anal.n.RData")

anal.e <- sapply(data.e, FUN = markAnalysis,
                 wd.model = "./mark_intermediate/", simplify = FALSE)
save(anal.e, file = "anal.e.RData")

rm(list = ls())
load("anal.e.RData")
load("anal.n.RData")

# [list element, simulation number]
unlist(sapply(anal.n[4, ], FUN = "[", "seed"))
# TODO: pripni seed v data.frame od simulacij

# for each simulation, check list of 4 and do manipulations to
# append input parameters with desired statistics

anal.e <- apply(anal.e, MARGIN = 2, FUN = function(x) {
  browser()
  # add population size estimates and the difference
  x$simulation.pars$size.1 <- x$est.der.pars$N.Population.Size.estimate[1]
  x$simulation.pars$size.sp <- x$est.der.pars$N.Population.Size.estimate[2]
  x$simulation.pars$size.est.diff <- diff(x$est.der.pars$N.Population.Size.estimate)
  
  # add capture probability esimates and the difference
  x$simulation.pars$p.1 <- x$real.fun.pars$estimate[1]
  x$simulation.pars$p.sp <- x$real.fun.pars$estimate[2]
  x$simulation.pars$p.diff <- diff(x$real.fun.pars$estimate)
  x$simulation.pars$p.target.1 <- x$simulation.pars$capture_prob - x$simulation.pars$p.1
  x$simulation.pars$p.target.sp <- x$simulation.pars$capture_prob - x$simulation.pars$p.sp
  
  x$simulation.pars$dAIC <- x$deltaAIC

  # naive sap area
  x$simulation.pars$sap.area.naive <- pi * x$simulation.pars$sampling_area_r^2
  # SAP enlarged for home range
  x$simulation.pars$sap.area.hr <- pi * (x$simulation.pars$home_range + x$simulation.pars$sampling_area_r)^2
  x
})

apply(anal.n, MARGIN = 2, FUN = function(x) {
  browser()
  # population size estimate of ~1 model
  x$simulation.pars$size.1 <- x$est.der.pars$N.Population.Size.estimate[1]
  # population size estimate of ~sp model
  x$simulation.pars$size.sp <- x$est.der.pars$N.Population.Size.estimate[2]
  # difference in population size estimates between the two models
  x$simulation.pars$size.est.diff <- diff(x$est.der.pars$N.Population.Size.estimate)
  
  # add capture probability esimates and the difference
  x$simulation.pars$p.1 <- x$real.fun.pars$estimate[1]
  x$simulation.pars$p.sp <- x$real.fun.pars$estimate[2]
  x$simulation.pars$p.diff <- diff(x$real.fun.pars$estimate)
  x$simulation.pars$p.target.1 <- x$simulation.pars$capture_prob - x$simulation.pars$p.1
  x$simulation.pars$p.target.sp <- x$simulation.pars$capture_prob - x$simulation.pars$p.sp
  
  x$simulation.pars$dAIC <- x$deltaAIC
  
  # naive sap area
  x$simulation.pars$sap.area.naive <- pi * x$simulation.pars$sampling_area_r^2
  # SAP enlarged for home range
  x$simulation.pars$sap.area.hr <- pi * (x$simulation.pars$home_range + x$simulation.pars$sampling_area_r)^2
  x
})
