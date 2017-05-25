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
rm(anal.n)

an.seed <- data.frame(seed = apply(an, MARGIN = 2, FUN = function(x) x$simulation.pars$seed))

set.seed(9) # for normal walkers
xy <- data.frame(SD = rep(seq(from = 20, to = 50, by = 5), each = 10*4*6*5),
                 prob = rep(seq(from = 0.15, to = 0.4, by = 0.05), each = 4),
                 num.walkers = c(100, 200, 400, 800, 1000),
                 sessions = 4:7
)

xy$seed <- (1:nrow(xy)) + 8400 # normal

an.seed <- merge(xy, an.seed)

ann <- apply(an, MARGIN = 2, FUN = calculateIndices, xy = an.seed)
ann <- do.call(rbind, ann)
rownames(ann) <- NULL

xc <- gather(ann, key = variable, value = index, starts_with("dens."))
xc$variable <- factor(xc$variable)

xc$correction <- ".1"
xc$correction[grepl(".sp$", xc$variable)] <- ".sp"
xc$correction.type <- sapply(strsplit(as.character(xc$variable), "\\."), "[", 2)
xc$sap.hr.ratio <- with(xc, area.naive/hr)

head(xc)
hist(xc$index)
xc <- xc[xc$index > -1, ]

ggplot(xc, aes(x = hr, y = index, group = variable)) +
  theme_bw() +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(color = correction), method = "gam", k = 5) +
  # facet_grid(correction ~ correction.type)
  facet_wrap(~ correction.type)

ggplot(xc, aes(x = num.generated.walkers, y = index, color = correction)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = correction), method = "gam", k = 5) +
  # geom_jitter(alpha = 0.5) +
  facet_wrap(~ correction.type)

# bias due to decreasing hr (ratio increases)
ggplot(xc, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = correction), method = "gam", k = 5) +
  facet_wrap(~ correction.type)

# density plot of p bias
ggplot(xc, aes(x = as.factor(p), y = p + p.diff)) +
  theme_bw() +
  geom_violin()
