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
xy <- data.frame(sap = 200,
                 area = 600,
                 num.walkers = 500,
                 sessions = rep(3:8, each = 50),
                 work.dir = "./data",
                 seed = sample(1:10e6, size = 300),
                 summary.file = "simulation_list.txt",
                 home.range = sqrt(qchisq(0.68, SD)),
                 prob = runif(n = 40, min = 0.15, max = 0.4),
                 rsln = 0.5,
                 SD = sample(10:50, size = 300),
                 num.boots = 1000,
                 weight.switch = TRUE,
                 sim.dist = "empirical" # ali "normal"
                 )

ax <- sapply(list.files("./data", pattern = ".inp"), FUN = markAnalysis, 
             wd.model = "./temp", 
             wd.inp = "./data",
             simplify = FALSE)
