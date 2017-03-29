library(raster)
library(rgeos)
library(snowfall)
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

hr <- 80
N <- 200
SD <- 1

crc <- gBuffer(SpatialPoints(matrix(c(0, 0), ncol = 2), proj4string = CRS(as.character(NA))), width = hr/2, quadsegs = 100)
wrld <- gBuffer(SpatialPoints(matrix(c(0, 0), ncol = 2), proj4string = CRS(as.character(NA))), width = 2*(hr/2), quadsegs = 100)
xy <- spsample(wrld, N, type = "random")
xy <- coordinates(xy)
xy <- cbind(xy, capt = 1)

#check
plot(crc, xlim = c(-200, 200), ylim = c(-200, 200))
points(xy[, 1], xy[, 2], col = as.factor(xy[, 3]))
plot(wrld, add = T, border = "red")
rd <- order(xy[, 1], xy[, 2], decreasing = TRUE)
#plot(gBuffer(SpatialPoints(xy[identify(xy[, 1:2]), 1:2, drop = FALSE], proj4string = CRS(as.character(NA))), width = hr, quadsegs = 100), lty = "dashed", add = T)
plot(gBuffer(SpatialPoints(xy[rd[1], 1:2, drop = FALSE], proj4string = CRS(as.character(NA))), width = hr/2, quadsegs = 100), lty = "dashed", add = T)

simulation(
        sap = 200, #polmer!!!!111oneoneeleven
        num.walkers = 1500,
        n.steps = 1000,
        sessions = 5,
        work.dir = "./data",
        seed = 600,
        summary.file = "simulation_list.txt",
        home.range = hr/2, # treba uporabit polmer
        prob = 0.2,
        sw = 1000,
        rsln = 1,
        cpus = 3,
        SD = SD,
        type = "SOCK",
        num.boots = 1000,
        weight.switch = TRUE #currently only weighted curve is included in the result, so switch should be TRUE
        # custom.walkers = NULL
)

ax <- sapply(list.files("./data", pattern = ".inp"), FUN = markAnalysis, 
             wd.model = "./temp", 
             wd.inp = "./data",
             simplify = FALSE)

# TODO: home.range in SD se tepeta