# Chunk 1 load packages and scripts
library(ggplot2)
library(tidyr)
library(dplyr)

source("../markAnalysis.R")
source("../readRunModels.R")
source("../getQs.R")
source("../calcNormal2D.R")
source("../calculateIndices.R")

# ======================== VANTAJM ===========================================================
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

# ======================== END VANTAJM =======================================================

rm(list = ls())
load("anal.e.RData")

ae <- anal.e

# for each simulation, check list of 4 and do manipulations to
# append input parameters with desired statistics

ane <- sapply(ae, FUN = calculateIndices, simplify = FALSE)

ane <- do.call(rbind, ane)
rownames(ane) <- NULL

xy <- gather(ane, key = variable, value = index, starts_with("dens."))
xy$variable <- factor(xy$variable)

# insert some identifications
xy$correction <- ".1"
xy$correction[grepl(".sp$", xy$variable)] <- ".sp"
xy$correction.type <- sapply(strsplit(as.character(xy$variable), "\\."), "[", 2)

xy <- xy[xy$index > -1e+10, ]
xy1 <- xy[sample(1:nrow(xy), size = 1000), ]

ggplot(xy1, aes(x = hr, y = index, group = variable)) +
  theme_bw() +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(color = correction), method = "gam", k = 5) +
  facet_wrap(~ correction.type)
  # facet_grid(correction ~ correction.type)



# ********************************************** normal *******************************************
load("anal.n.RData")
an <- anal.n


