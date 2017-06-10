# Chunk 1 load packages and scripts
library(ggplot2)
library(tidyr)
library(dplyr)
library(capwire)

library(parallel) # only if relcalculating aee/ann

source("../markAnalysis.R")
source("../readRunModels.R")
source("../getQs.R")
source("../calcNormal2D.R")
source("calculateIndices.R")

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
# save(anal.e, file = "anal.e.RData")

# ======================== END VANTAJM =======================================================

# ********************************************** empirical *******************************************
# load("anal.e.RData")
# ae <- anal.e
# rm(anal.e)

# cl <- makeCluster(4)
# clusterEvalQ(cl, source("../calcNormal2D.R"))
# clusterEvalQ(cl, source("../getQs.R"))
# clusterEvalQ(cl, source("../readRunModels.R"))
# clusterEvalQ(cl, library(capwire))
# 
# lf <- list.files("../data/empirical/", pattern = ".inp", full.names = TRUE)
# aee <- parSapply(cl = cl, X = ae, FUN = calculateIndices, lf = lf, simplify = FALSE)
# # aee <- sapply(X = ae, FUN = calculateIndices, lf = lf, simplify = FALSE)
# aee <- do.call(rbind, aee)
# rownames(aee) <- NULL

# save(aee, file = "aee.RData")
load("aee.RData")

xe <- gather(aee, key = variable, value = index, starts_with("dens."))
rownames(xe) <- NULL
xe$variable <- factor(xe$variable)

xe$model <- ".1"
xe$model[grepl(".sp$", xe$variable)] <- ".sp"
xe$model[grepl(".tirm", xe$variable)] <- ".tirm"
xe$correction.type <- sapply(strsplit(as.character(xe$variable), "\\."), "[", 2)
xe$sap.hr.ratio <- with(xe, area.naive/hr)

# first examine p
xep <- gather(xe[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", "variable", "index", "sap.hr.ratio")],
                 key = p.var, value = p.val, p.target.1, p.target.sp)
xep$correction.type <- gsub("dens\\.(.*)\\.(.*)$", "\\1", x = xep$variable)
xep <- xep[!duplicated(xep[, c("p.val", "p.var", "num.generated.walkers", "true.p", "correction.type")]), ]
# xep.orig <- xep
xep <- xep.orig[sample(1:nrow(xep.orig), size = round(0.2 * nrow(xep.orig))), ]

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE)
ggsave("./figures/E-0 pristranskost .1 in .sp ocene ulovljivosti.png")

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-0 pristranskost p.1 in p.sp glede na st. walkerjev in correction type.png")

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ .)
ggsave("./figures/E-0 pristranskost p glede na model in st. generiranih walkerjev.png")
summary(glm(p.val ~ true.p * p.var * num.generated.walkers, data = xep))

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/E0- pristranskost p glede na model in tip popravka.png")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-0 pristranskot p glede na sap.hr ratio po st. gen. walkerjih in popravku.png")

# xe.orig <- xe
# xe <- xe.orig
xe <- xe.orig[sample(1:nrow(xe.orig), size = round(nrow(xe.orig)/10)), ]
xe <- xe[xe$index > -2.0e+06, ]

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90)) +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk.png")

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90)) +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(num.generated.walkers ~ correction.type, scales = "free_y")
ggsave("./figures/E-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk free_y.png")

# density plot of p bias, do not include tirm model since we don't have p for it
ggplot(droplevels(xe[!(xe$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(num.generated.walkers ~ sessions)
ggsave("./figures/E-2.razlika v p glede na simuliran p po stevilu sessionov in st. gen. walk.png", 
       width = 25, height = 18, units = "cm")

ggplot(droplevels(xe[!(xe$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(num.generated.walkers ~ sessions, scales = "free_y")
ggsave("./figures/E-2.razlika v p glede na simuliran p po stevilu sessionov in st. gen. walk free_y.png", 
       width = 25, height = 18, units = "cm")

ggplot(droplevels(xe[!(xe$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(correction.type ~ sessions)
ggsave("./figures/E-3.razlika v p glede na simuliran p po stevilu sessionov in popravek.png",
       width = 25, height = 18, units = "cm")

ggplot(droplevels(xe[!(xe$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(correction.type ~ sessions, scales = "free_y")
ggsave("./figures/E-3.razlika v p glede na simuliran p po stevilu sessionov in popravek free_y.png",
       width = 25, height = 18, units = "cm")

ggplot(droplevels(xe[!(xe$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-4.razlika v p glede na simuliran p po stevilu sim. walkerjev in modelu.png",
       width = 25, height = 18, units = "cm")
# več je vzorcev, bolj točno so ocenjeni parametri?

ggplot(xe, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk.png")

ggplot(xe, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type, scales = "free_y")
ggsave("./figures/E-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk free y.png")

ggplot(xe, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = better.model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-6.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.png")

# ********************************************** normal *******************************************
# load("anal.n.RData")
# an <- anal.n
# rm(anal.n)

# cl <- makeCluster(4)
# clusterEvalQ(cl, source("../calcNormal2D.R"))
# clusterEvalQ(cl, source("../getQs.R"))
# clusterEvalQ(cl, source("../readRunModels.R"))
# clusterEvalQ(cl, library(capwire))
# 
# lf <- list.files("../data/normal/", pattern = ".inp", full.names = TRUE)
# ann <- parApply(cl = cl, X = an, MARGIN = 2, FUN = calculateIndices, lf = lf)
# ann <- apply(an, MARGIN = 2, FUN = calculateIndices, lf = lf)
# ann <- do.call(rbind, ann)
# rownames(ann) <- NULL
# save(ann, file = "ann.RData")
load("ann.RData")

xc <- gather(ann, key = variable, value = index, starts_with("dens."))
rownames(xc) <- NULL
xc$variable <- factor(xc$variable)

xc$model <- ".1"
xc$model[grepl(".sp$", xc$variable)] <- ".sp"
xc$model[grepl(".tirm", xc$variable)] <- ".tirm"
xc$correction.type <- sapply(strsplit(as.character(xc$variable), "\\."), "[", 2)
xc$sap.hr.ratio <- with(xc, area.naive/hr)

####
xnp <- gather(xc[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", "variable", "index", "sap.hr.ratio")],
              key = p.var, value = p.val, p.target.1, p.target.sp)
xnp$correction.type <- gsub("dens\\.(.*)\\.(.*)$", "\\1", x = xnp$variable)
xnp <- xnp[!duplicated(xnp[, c("p.val", "p.var", "num.generated.walkers", "true.p", "correction.type")]), ]
# xnp.orig <- xnp
xnp <- xnp.orig[sample(1:nrow(xnp.orig), size = round(0.2 * nrow(xnp))), ]

ggplot(xnp, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE)
ggsave("./figures/N-0 pristranskost .1 in .sp ocene ulovljivosti.png")

ggplot(xnp, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-0 pristranskost p.1 in p.sp glede na st. walkerjev in correction type.png")

ggplot(xnp, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ .)
ggsave("./figures/N-0 pristranskost p glede na model in st. generiranih walkerjev.png")
summary(glm(p.val ~ true.p * p.var * num.generated.walkers, data = xnp))

ggplot(xnp, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/N0- pristranskost p glede na model in tip popravka.png")

ggplot(xnp, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-0 pristranskot p glede na sap.hr ratio po st. gen. walkerjih in popravku.png")

########
# xc.orig <- xc
# xc <- xc.orig
xc <- xc.orig[sample(1:nrow(xc.orig), size = round(nrow(xc.orig)/10)), ]
xc <- xc[xc$index > -0.015, ]

ggplot(xc, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk.png")

# density plot of p bias, do not include tirm model since we don't have p for it
ggplot(droplevels(xc[!(xc$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(num.generated.walkers ~ sessions)
ggsave("./figures/N-2.razlika v p glede na simuliran p po stevilu sessionov in st. gen. walk.png", 
       width = 25, height = 18, units = "cm")

ggplot(droplevels(xc[!(xc$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(correction.type ~ sessions)
ggsave("./figures/N-3.razlika v p glede na simuliran p po stevilu sessionov in popravek.png",
       width = 25, height = 18, units = "cm")

ggplot(droplevels(xc[!(xc$model %in% ".tirm"), ]), aes(x = as.factor(p), y = p.diff, fill = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_violin() +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-4.razlika v p glede na simuliran p po stevilu sim. walkerjev in modelu.png",
       width = 25, height = 18, units = "cm")
# več je vzorcev, bolj točno so ocenjeni parametri?

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk.png")

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type, scales = "free_y")
ggsave("./figures/N-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk free y.png")

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = better.model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-6.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.png")
