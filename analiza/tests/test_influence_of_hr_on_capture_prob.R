library(raster)
library(rgeos)
library(cluster)
library(splancs)
library(ggplot2)
library(tidyr)
library(dplyr)

xy <- data.frame(SD = c(5, 10, 15, 20, 30, 50, 100), prob = 0.2, num.walkers = 1300, sessions = 10)

xy$sap <- 200
xy$home.range <- xy$SD
xy$area <- 1000
xy$work.dir <- "data"
# xy$seed <- 1:nrow(xy)
xy$seed <- 357
xy$sim.dist <- "normal"
xy$summary.file <- sprintf("simulation_list_%s.txt", xy$sim.dist)
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$num.boots <- 5000

library(foreach)

foreach(i = 1:nrow(xy)) %do% {
  library(raster)
  library(rgeos)
  library(cluster)
  library(splancs)
  
  source("simulation.R")
  source("walkerContribution.R")
  source("populateWorld.R")
  source("sampleWorld.R")
  source("sampleWalkers.R")
  source("numberOfBins.R")
  source("calculateContribution.R")
  source("calculateBins.R")
  source("weighDistances.R")
  source("distWeights.R")
  source("individualContribution.R")
  source("calcNormal2D.R")
  source("superPopulation.R")
  source("writeINP.R")
  source("calcNormal2D.R")
  
  out <- tryCatch({
    simulation(
      SD = xy$SD[i],
      prob = xy$prob[i],
      sessions = xy$sessions[i],
      num.walkers = xy$num.walkers[i],
      sap = xy$sap[i],
      area = xy$area[i],
      work.dir = xy$work.dir[i],
      seed = xy$seed[i],
      summary.file = xy$summary.file[i],
      home.range = xy$home.range[i],
      rsln = xy$rsln[i],
      weight.switch = xy$weight.switch[i],
      sim.dist = xy$sim.dist[i],
      num.boots = xy$num.boots[i]
    )
  }, 
  error = function(e) e,
  warning = function(w) w)
  
  if (any(class(out) %in% c("simpleError", "simpleWarning"))) {
    print(out)
  }
}

# prepare for MARK analysis
source("./analiza/markAnalysis.R")
source("./analiza/readRunModels.R")
library(RMark)

out <- list.files("./data", pattern = ".\\.inp$", full.names = TRUE)
out <- sapply(out, FUN = markAnalysis, simplify = FALSE)
out <- out[order(unname(sapply(out, FUN = function(x) x$simulation.pars$home_range)))]

sapply(out, FUN = "[", "real.fun.pars")
unname(sapply(out, FUN = function(x) x$real.fun.pars$estimate))
unname(sapply(out, FUN = function(x) x$simulation.pars$home_range))

# če se veča SD, se slabša p (kot bi pričakoval)

# Chunk 1 load packages and scripts
library(capwire)

source("./analiza/markAnalysis.R")
source("./analiza/readRunModels.R")
source("getQs.R")
source("calcNormal2D.R")
source("./analiza/calculateIndices.R")

lf <- list.files("./data/", pattern = ".inp", full.names = TRUE)
rsl <- mapply(FUN = calculateIndices, x = out, lf = lf, SIMPLIFY = FALSE)
rsl <- do.call(rbind, rsl)
rownames(rsl) <- NULL
rsl

xep <- rsl[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", "hr",
                 "sessions", "area.naive")]
xep <- gather(xep, key = p.var, value = p.val, starts_with("p.target."))
xep <- xep[xep$p.val > 0, ] # include only p values with sensible estimate above 0
xep$hr.sap.ratio <- with(xep, hr/sqrt(xep$area.naive/pi))
xep$p.ratio <- xep$p.val/xep$true.p

ggplot(xep, aes(x = hr.sap.ratio, y = p.ratio)) +
  theme_bw() +
  geom_jitter(size = 0.5, alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_grid(num.generated.walkers ~ sessions)
