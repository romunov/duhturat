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

set.seed(357) # use seed for reproducibility of generating starting values
nsim <- 500
xy <- data.frame(SD = round(runif(nsim, min = 5, max = 200)),
                 prob = runif(nsim, min = 0.1, max = 0.3),
                 num.walkers = sample(c(1000, 2000, 3000, 4000, 5000), size = nsim, replace = TRUE),
                 sessions = sample(c(3, 6, 10), size = nsim, replace = TRUE)
)

# SD=60, home range extends from 0 to about 200, see
# curve(dnorm(x, sd = 60), from = 0, to = 600)
# SD=20, home range extends from 0 to about 50
# curve(dnorm(x, sd = 20), from = 0, to = 600)
xy <- data.frame(SD = c(5, 10, 15), prob = 0.18, num.walkers = 800, sessions = 5)
  
xy$sap <- 200
xy$home.range <- xy$SD
xy$area <- 1000
xy$work.dir <- "data"
# xy$seed <- 1:nrow(xy)
xy$seed <- 1
xy$sim.dist <- "normal"
xy$summary.file <- sprintf("simulation_list_%s.txt", xy$sim.dist)
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$num.boots <- 5000

i <- sample(1:nrow(xy), size = 1, replace = FALSE)
# i <- which.min(xy$SD)

rdt <- data.frame(
  SD = as.numeric(xy[i, "SD"]),
  prob = as.numeric(xy[i, "prob"]),
  sessions = as.numeric(xy[i, "sessions"]),
  num.walkers = as.numeric(xy[i, "num.walkers"]),
  sap = as.numeric(xy[i, "sap"]),
  area = as.numeric(xy[i, "area"]),
  work.dir = xy[i, "work.dir"],
  seed = as.numeric(xy[i, "seed"]),
  summary.file = xy[i, "summary.file"],
  home.range = as.numeric(xy[i, "home.range"]),
  rsln = as.numeric(xy[i, "rsln"]),
  weight.switch = as.logical(xy[i, "weight.switch"]),
  sim.dist = xy[i, "sim.dist"],
  num.boots = as.numeric(xy[i, "num.boots"]),
  comment = "not passed",
  stringsAsFactors = FALSE
)

simulation(
  SD = rdt$SD,
  prob = rdt$prob,
  sessions = rdt$sessions,
  num.walkers = rdt$num.walkers,
  sap = rdt$sap,
  area = rdt$area,
  work.dir = rdt$work.dir,
  seed = rdt$seed,
  summary.file = rdt$summary.file,
  home.range = rdt$home.range,
  rsln = rdt$rsln,
  weight.switch = rdt$weight.switch,
  sim.dist = rdt$sim.dist,
  num.boots = rdt$num.boots
)

# paralelna verzija
library(foreach)
library(doParallel)

cl <- makeCluster(3)
registerDoParallel(cl)


foreach(i = 1:3) %dopar% {
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
  source("stopWatch.R")
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
}

# setwd("..")
source("./analiza/markAnalysis.R")
source("./analiza/readRunModels.R")
library(RMark)
out.3 <- markAnalysis(fn = "./data/mark-2017-10-27-10-34_normal_1.inp")
out.1 <- markAnalysis(fn = "./data/mark-2017-10-27-10-41_normal_1.inp")
out.05 <- markAnalysis(fn = "./data/mark-2017-10-27-11-05_normal_1.inp")


out.3$real.fun.pars
out.1$real.fun.pars
out.05$real.fun.pars
# velikost celic bistveno ne vpliva na oceno parametrov



# By how much should cut-off of home range be expanded to encompass practically all sampled points?
# ~ 4
source("getQs.R")
sapply(0.999, FUN = getQnormal, SD = 20)/20
sapply(0.999, FUN = getQnormal, SD = 60)/60
qnorm(seq(0.5, 1, by = 0.1), sd = 20)
qnorm(0.7, sd = 20)
pnorm(5.07, sd = 20)
curve(dnorm(x, sd = 20), from = 0, to = 100)
1 - pnorm(40, sd = 20, lower.tail = FALSE)


# test density
library(sp)
library(rgeos)

c(1000, 2000, 3000, 4000, 5000)/(pi * 1000^2)

N <- 2000
# N <- 5000
area.sap <- 200
area.world <- 1000
SD <- 200

sap <- SpatialPoints(matrix(c(0,0), nrow = 1))
sap <- gBuffer(sap, width = area.sap)

world <- SpatialPoints(matrix(c(0,0), nrow = 1))
world <- gBuffer(world, width = area.world)

xy <- spsample(x = world, n = N, type = "random")
pts <- SpatialPoints(xy)

plot(sap, xlim = c(-600, 600), ylim = c(-600, 600), border = "blue", axes = TRUE, lwd = 2)
points(pts, col = "light grey")

# true density
plot(world, add = TRUE, lwd = 2)
length(pts)/gArea(world) # true simulated density
(td <- length(pts)/(pi * area.world^2))

for (i in seq(0, 8, by = 1)) {
  supop <- gBuffer(sap, width = SD * i)
  # plot(supop, add = TRUE, lwd = 2)
  pts.supop <- over(pts, supop)
  nw <- sum(pts.supop == 1, na.rm = TRUE)
  estD <- sum(pts.supop, na.rm = TRUE)/gArea(sap)
  # points(pts[!is.na(pts.supop), ], col = "dark grey")
  message(sprintf("i: %s; nwalkers: %s; estD: %s, bias: %s; trueD: %s", 
                  i, 
                  nw,
                  round(estD, 5),
                  round(estD/td, 5),
                  round(nw/gArea(supop), 5)
                  )
          )
}

curve(dnorm(x, sd = 200), from = 0, to = 1000)

load("./analiza/simulations.RData")
esde <- sapply(xy.mark, FUN = function(x) x$simulation.pars$SD)
range(esde)


with(xy, xy[num.generated.walkers == "800" & sessions == "5" & num.captured.walkers == 39 &
              hr == "68", ])
