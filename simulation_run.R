library(raster)
library(rgeos)
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