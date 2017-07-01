# Chunk 1 load packages and scripts
library(ggplot2)
library(tidyr)
library(dplyr) # starts_with, used in gather

# Uncomment to rerun the analysis from the beginning. You might be waiting a while...
source("analysis_run_mark.R")
source("analysis_calculate_indices.R")

load("simulations_calculated_indices.RData")
xy <- droplevels(xy[xy$fun == "empirical", ])

xe <- gather(xy, key = variable, value = index, starts_with("dens."))
rownames(xe) <- NULL
xe$variable <- factor(xe$variable)

xe$model <- ".1"
xe$model[grepl(".sp$", xe$variable)] <- ".sp"
xe$model[grepl(".tirm", xe$variable)] <- ".tirm"
xe$correction.type <- sapply(strsplit(as.character(xe$variable), "\\."), "[", 2)
xe$sap.hr.ratio <- with(xe, area.naive/(pi * xe$hr^2))
xe$correction.type <- factor(xe$correction.type, levels = c("naive", "hr", "50", "60", "70", "80", "90", "95", "99", "effect"))

# first examine p
xep <- gather(xe[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", 
                     "variable", "index", "sap.hr.ratio", "sessions")],
              key = p.var, value = p.val, p.target.1, p.target.sp)
xep$correction.type <- gsub("dens\\.(.*)\\.(.*)$", "\\1", x = xep$variable)
xep$correction.type <- factor(xep$correction.type, levels = c("naive", "hr", "50", "60", "70", "80", "90", "95", "99", "effect"))
xep <- xep[!duplicated(xep[, c("p.val", "p.var", "num.generated.walkers", "true.p", "correction.type")]), ]
xep$p.val <- xep$p.val/xep$true.p

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", se = FALSE)
ggsave("./figures/E-0a pristranskost .1 in .sp ocene ulovljivosti.png",
       width = 10, height = 5, units = "in")

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-0b pristranskost p.1 in p.sp glede na st. walkerjev in correction type.png",
       width = 10, height = 5, units = "in")

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ .)
ggsave("./figures/E-0c pristranskost p glede na model in st. generiranih walkerjev.png",
       width = 10, height = 5, units = "in")
summary(glm(p.val ~ true.p * p.var * num.generated.walkers, data = xep))

ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/E-0d pristranskost p glede na model in tip popravka.png",
       width = 10, height = 5, units = "in")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-0e pristranskot p glede na sap.hr ratio po st. gen. walkerjih in popravku.png",
       width = 10, height = 5, units = "in")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(num.generated.walkers ~ .)
ggsave("./figures/E-0f pristranskost p glede na sap.hr ratio po st. gen walkerjev brez popravka.png")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ correction.type)
ggsave("./figures/E-0g pristranskost p glede na sap.hr ratio po popravku brez gostote walkerjev.png",
       width = 10, height = 5, units = "in")

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = p.var)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(sessions ~ num.generated.walkers)
ggsave("./figures/E-0h pristranskost p glede sap.hr razmerje glede na st. walkerjev in st. sessionov.png",
       width = 10, height = 5, units = "in")
summary(glm(p.val ~ sap.hr.ratio * sessions * p.var, data = xep))

ggplot(xep, aes(x = sap.hr.ratio, y = p.val, color = as.factor(sessions))) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("p_ests/p_true") +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(. ~ as.factor(num.generated.walkers))
ggsave("./figures/E-0i pristranskost p glede na sap.hr.ratio in glede sessions.png",
       width = 10, height = 5, units = "in")

# analyse density
# remove 1% of values considered as outlayers due to numerical problems
xe <- xe[xe$index < quantile(xe$index, probs = 0.99), ]
fh <- 5
fw <- fh * 1.62

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_jitter(alpha = 0.2, shape = 1) +
  geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
  scale_color_brewer(palette = "Set1") +
  # scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = 1, size = 1, alpha = 0.5) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk.png",
       width = fw, height = fh, units = "in")

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  # geom_jitter(alpha = 0.2, shape = 1) +
  geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 10)) +
  geom_hline(yintercept = 1, size = 1, alpha = 0.5) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk brez pik.png",
       width = fw, height = fh, units = "in")

ggplot(xe, aes(x = sap.hr.ratio, y = index)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_jitter(alpha = 0.5, shape = 1) +
  # scale_y_continuous(limits = c(0, 2)) +
  # scale_x_continuous(limits = c(0, 5000)) +
  geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = 1, size = 1, alpha = 0.5) +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-2.gostota glede na razmerje, corr.type in st. gen walkerjev pozoomano.png",
       width = 10, height = 8, units = "in")

# AICc
ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_jitter(alpha = 0.5, shape = 1) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk.png",
       width = 10, height = 8, units = "in")

ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = sap.hr.ratio, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_brewer(palette = "Set1") +
  geom_jitter(alpha = 0.5, shape = 1) +
  geom_smooth(aes(color = better.model), method = "lm") +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-6.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.png",
       width = 10, height = 8, units = "in")

ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = model, y = dAIC)) +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_violin() +
  facet_grid(num.generated.walkers ~ correction.type)
ggsave("./figures/E-7 dAIC po modelih sp in 1.png", width = 10, height = 8, units = "in")

# # # normal # # #

xz <- droplevels(aee[aee$fun == "normal", ])
