library(RMark)
library(parallel)

source("markAnalysis.R")
source("readRunModels.R")

if (Sys.info()["sysname"] == "Windows") {
  ncores <- 8
} else {
  ncores <- 46
}

cl <- makeCluster(ncores)
on.exit(stopCluster(cl))

# clusterEvalQ(cl = cl, expr = source("readRunModels.R"))
clusterExport(cl = cl, varlist = c("extractPars", "readMark"))
clusterEvalQ(cl = cl, expr = library(RMark))

xy <- list.files("../data/", pattern = ".inp", full.names = TRUE)

system.time(xy.mark <- parSapply(cl = cl, X = xy, FUN = markAnalysis, simplify = FALSE))
system.time(xy.mark <- sapply(X = xy, FUN = markAnalysis, simplify = FALSE))

stopCluster(cl)

save(xy.mark, file = "simulations.RData")
save(xy.mark, file = "simulations_test.RData")
