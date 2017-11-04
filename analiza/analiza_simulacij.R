# Chunk 1 load packages and scripts
library(ggplot2)
library(tidyr)
library(dplyr) # starts_with, used in gather
source("functions.R")

# Uncomment to rerun the analysis from the beginning. You might be waiting a while...
# source("analysis_run_mark.R")
# source("analysis_calculate_indices.R")

load("simulations_calculated_indices.RData")

plt <- c(a0 = TRUE, b0 = TRUE, d0 = TRUE, f0 = TRUE, h0 = TRUE, i0 = TRUE, a1 = TRUE,
         b1 = TRUE, a2 = TRUE, a5 = TRUE, a6 = TRUE, a7 = TRUE)
#### EMPIRICAL ####
createFigures(xy = xy, type = "E", plt)

#### NORMAL ####
createFigures(xy = xy, type = "N", plt)

# out <- table(xy$num.generated.walkers, xy$sessions, xy$fun)
# 5  10  15
# 500  -46 -22  -7
# 800  -38 -13  -5
# 1000 -40  -8   0
# 1300 -23   0  -1
# 1500 -28  -4   0
