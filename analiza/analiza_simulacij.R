# Chunk 1 load packages and scripts
library(ggplot2)
library(tidyr)
library(dplyr)

# Uncomment to rerun the analysis from the beginning. You might be waiting a while...
# source("analysis_run_mark.R")
# source("analysis_calculate_indices.R") # TODO: recalculate this because area_size was not correct
load("simulations_calculated_indices.RData")
xy <- aee
xy <- droplevels(aee[aee$fun == "empirical", ])

xe <- gather(xy, key = variable, value = index, starts_with("dens."))
rownames(xe) <- NULL
xe$variable <- factor(xe$variable)

xe$model <- ".1"
xe$model[grepl(".sp$", xe$variable)] <- ".sp"
xe$model[grepl(".tirm", xe$variable)] <- ".tirm"
xe$correction.type <- sapply(strsplit(as.character(xe$variable), "\\."), "[", 2)
xe$sap.hr.ratio <- with(xe, area.naive/hr)

# first examine p
xep <- gather(xe[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", 
                     "variable", "index", "sap.hr.ratio", "sessions")],
                 key = p.var, value = p.val, p.target.1, p.target.sp)
xep$correction.type <- gsub("dens\\.(.*)\\.(.*)$", "\\1", x = xep$variable)
xep <- xep[!duplicated(xep[, c("p.val", "p.var", "num.generated.walkers", "true.p", "correction.type")]), ]
# xep.orig <- xep
# xep <- xep.orig[sample(1:nrow(xep.orig), size = round(0.2 * nrow(xep.orig))), ]

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.05) +
  scale_y_continuous(limits = c(0, 0.4)) +
  geom_smooth(method = "loess", se = FALSE)
ggsave("./figures/E-0a pristranskost .1 in .sp ocene ulovljivosti.png")

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-0b pristranskost p.1 in p.sp glede na st. walkerjev in correction type.png")

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ .)
ggsave("./figures/E-0c pristranskost p glede na model in st. generiranih walkerjev.png")
summary(glm(p.val ~ true.p * p.var * num.generated.walkers, data = xep))

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/E-0d pristranskost p glede na model in tip popravka.png")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-0e pristranskot p glede na sap.hr ratio po st. gen. walkerjih in popravku.png")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ .)
ggsave("./figures/E-0f pristranskost p glede na sap.hr ratio po st. gen walkerjev brez popravka.png")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/E-0g pristranskost p glede na sap.hr ratio po popravku brez gostote walkerjev.png")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(sessions ~ correction.type)
ggsave("./figures/E-0h pristranskost p glede sap.hr razmerje glede na model.png")
summary(glm(p.val ~ sap.hr.ratio * sessions * p.var, data = xep))

ggplot(xep, aes(x = sap.hr.ratio, y = p.val)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(color = sessions), method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/E-0i pristranskost p glede na sap.hr.ratio in glede na popravek brez sessions.png",
       units = "mm", width = 100, height = 250)

# analyse density
xe <- xe[xe$index > -200, ]

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
ggsave("./figures/E-1b.gostota gled na razmerje hr_sap po correction type in st. gen.walk.png")

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90)) +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures")

ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(aes(color = model), method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk.png")

ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = better.model), method = "lm") +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-6.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.png")


# # # normal # # #

xz <- droplevels(aee[aee$fun == "normal", ])