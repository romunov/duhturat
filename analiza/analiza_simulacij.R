# Chunk 1 load packages and scripts
library(ggplot2)
library(tidyr)
library(dplyr)
library(capwire)

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
load("anal.e.RData")
ae <- anal.e
rm(anal.e)

lf <- list.files("../data/empirical/", pattern = ".inp", full.names = TRUE)
aee <- sapply(ae, FUN = calculateIndices, lf = lf, simplify = FALSE)
aee <- do.call(rbind, aee)
rownames(aee) <- NULL

xe <- gather(aee, key = variable, value = index, starts_with("dens."))
rownames(xe) <- NULL
xe$variable <- factor(xe$variable)

xe$correction <- ".1"
xe$correction[grepl(".sp$", xe$variable)] <- ".sp"
xe$correction.type <- sapply(strsplit(as.character(xe$variable), "\\."), "[", 2)
xe$sap.hr.ratio <- with(xe, area.naive/hr)

xe <- xe[xe$index > -2.0e+07, ]
xe.orig <- xe
xe <- xe.orig[sample(1:nrow(xe.orig), size = round(nrow(xe.orig)/10)), ]

# ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
#   theme_bw() +
#   geom_jitter(alpha = 0.5, shape = 1) +
#   geom_smooth(aes(color = correction), method = "loess") +
#   scale_color_brewer(palette = "Set1") +
#   facet_wrap(~ correction.type)
# ggsave("./figures/E-gostota glede na razmerje hr_sap.jpg")

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = correction), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-gostota gled na razmerje hr_sap po correction type in st. gen.walk.jpg")

# density plot of p bias
ggplot(xe, aes(x = as.factor(p), y = p.diff)) +
  theme_bw() +
  geom_violin() +
  facet_grid(num.generated.walkers ~ sessions)
ggsave("./figures/E-razlika v p glede na simuliran p po stevilu sessionov in st. gen. walk.jpg")

ggplot(xe, aes(x = as.factor(p), y = p.diff)) +
  theme_bw() +
  geom_violin() +
  facet_grid(correction.type ~ sessions)
ggsave("./figures/E-razlika v p glede na simuliran p po stevilu sessionov in popravek.jpg")

ggplot(xe, aes(x = as.factor(p), y = p.diff)) +
  theme_bw() +
  geom_violin() +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-razlika v p glede na simuliran p po stevilu sim. walkerjev in modelu.jpg")
# več je vzorcev, bolj točno so ocenjeni parametri?

#### AIC
# ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
#   theme_bw() +
#   geom_jitter(alpha = 0.5) +
#   geom_smooth(aes(color = correction), method = "gam", k = 5) +
#   scale_color_brewer(palette = "Set1") +
#   facet_wrap(~ correction.type)
# ggsave("./figures/E-dAIC glede na razmerje hr_sap.jpg")

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = correction), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-dAIC gled na razmerje hr_sap po correction type in st. gen.walk.jpg")

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = better.model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.jpg")



















# ********************************************** normal *******************************************
load("anal.n.RData")
an <- anal.n
rm(anal.n)

lf <- list.files("../data/normal/", pattern = ".inp", full.names = TRUE)
ann <- apply(an, MARGIN = 2, FUN = calculateIndices, lf = lf)
ann <- do.call(rbind, ann)
rownames(ann) <- NULL

xc <- gather(ann, key = variable, value = index, starts_with("dens."))
rownames(xc) <- NULL
xc$variable <- factor(xc$variable)

xc$correction <- ".1"
xc$correction[grepl(".sp$", xc$variable)] <- ".sp"
xc$correction.type <- sapply(strsplit(as.character(xc$variable), "\\."), "[", 2)
xc$sap.hr.ratio <- with(xc, area.naive/hr)

head(xc)
hist(xc$index)
xc <- xc[xc$index > -0.01, ]
xc.orig <- xc
xc <- xc.orig[sample(1:nrow(xc.orig), size = round(nrow(xc.orig)/10)), ]

# ggplot(xc, aes(x = sap.hr.ratio, y = index)) +
#   theme_bw() +
#   geom_jitter(alpha = 0.5, shape = 1) +
#   geom_smooth(aes(color = correction), method = "loess", se = FALSE) +
#   scale_color_brewer(palette = "Set1") +
#   facet_wrap(~ correction.type)
# ggsave("./figures/N-gostota glede na razmerje hr_sap.jpg")

ggplot(xc, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = correction), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-gostota gled na razmerje hr_sap po correction type in st. gen.walk.jpg")

# density plot of p bias
ggplot(xc, aes(x = as.factor(p), y = p.diff)) +
  theme_bw() +
  geom_violin() +
  facet_grid(num.generated.walkers ~ sessions)
ggsave("./figures/N-razlika v p glede na simuliran p po stevilu sessionov in st. gen. walk.jpg")

ggplot(xc, aes(x = as.factor(p), y = p.diff)) +
  theme_bw() +
  geom_violin() +
  facet_grid(correction.type ~ sessions)
ggsave("./figures/N-razlika v p glede na simuliran p po stevilu sessionov in popravek.jpg")

ggplot(xc, aes(x = as.factor(p), y = p.diff)) +
  theme_bw() +
  geom_violin() +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-razlika v p glede na simuliran p po stevilu sim. walkerjev in modelu.jpg")
# več je vzorcev, bolj točno so ocenjeni parametri?

#### AIC
# ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
#   theme_bw() +
#   geom_jitter(alpha = 0.5) +
#   geom_smooth(aes(color = correction), method = "gam", k = 5) +
#   scale_color_brewer(palette = "Set1") +
#   facet_wrap(~ correction.type)
# ggsave("./figures/dAIC glede na razmerje hr_sap.jpg")

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = correction), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-dAIC gled na razmerje hr_sap po correction type in st. gen.walk.jpg")

ggplot(xc, aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = better.model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/N-dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.jpg")



# ******* CAPWIRE ********
library(capwire)

