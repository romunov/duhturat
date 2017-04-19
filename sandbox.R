library(raster)
library(rgeos)
# library(snowfall)
library(cluster)
library(splancs) #csr
library(RMark)

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
source("calcDistance.R")
source("calcDistanceNumeric.R")
source("superPopulation.R")
source("stopWatch.R")
source("writeINP.R")
# funkcije za analizo
source("extractMarkResults.R")
source("markAnalysis.R")
source("readRunModels.R")
source("calcNormal2D.R")

simulation(
        sap = 200, #polmer!!!!111oneoneeleven
        area = 200, # koliko bo SAP povečan
        num.walkers = 500,
        sessions = 5,
        work.dir = "./data",
        seed = 610,
        summary.file = "simulation_list.txt",
        home.range = sqrt(qchisq(0.68, 20)), # ena SD za dano SD
        prob = 0.4,
        rsln = 0.5,
        SD = 20,
        num.boots = 1000,
        weight.switch = TRUE, #currently only weighted curve is included in the result, so switch should be TRUE
        sim.dist = "normal"
)

set.seed(357)
xy <- data.frame(SD = rep(seq(from = 10, to = 50, by = 2), each = 10*4*6),
            prob = rep(seq(from = 0.15, to = 0.4, by = 0.05), each = 4),
            sessions = 4:7
            )

xy$sap <- 200
xy$area <- 600
xy$work.dir <- "./data"
xy$seed <- 1:nrow(xy)
xy$summary.file <- "simulation_list.txt"
xy$home.range <- sqrt(qchisq(0.68, xy$SD))
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$sim.dist <- "normal"

ax <- sapply(list.files("./data", pattern = ".inp"), FUN = markAnalysis, 
             wd.model = "./temp", 
             wd.inp = "./data",
             simplify = FALSE)

#######################
set.seed(357)
xy <- data.frame(SD = rep(seq(from = 50, to = 100, by = 5), each = 10*4*6*5),
                 prob = rep(seq(from = 0.15, to = 0.4, by = 0.05), each = 4),
                 num.walkers = c(100, 200, 400, 800, 1000),
                 sessions = 4:7
)

xy$sap <- 200
xy$area <- 600
xy$work.dir <- "./data"
xy$seed <- 1:nrow(xy)
xy$summary.file <- "simulation_list.txt"
xy$home.range <- sqrt(qchisq(0.68, xy$SD))
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$sim.dist <- "normal"
xy$num.boots <- 5000
xy["sim.dist"] <- "empirical"

i <- sample(1:nrow(xy), size = 1, replace = FALSE)

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

out <- tryCatch({
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
}, 
error = function(e) e,
warning = function(w) w)
