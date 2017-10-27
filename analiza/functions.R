createFigures <- function(xy, type = c("N", "E")) {
  if (type == "E") {
    xy <- droplevels(xy[xy$fun == "empirical", ])
  }
  
  if (type == "N") {
    xy <- droplevels(xy[xy$fun == "normal", ])
  }
  
  xe <- gather(xy, key = variable, value = index, starts_with("dens."))
  rownames(xe) <- NULL
  xe$variable <- factor(xe$variable)
  
  xe$model <- ".1"
  xe$model[grepl(".sp$", xe$variable)] <- ".sp"
  xe$model[grepl(".tirm", xe$variable)] <- ".tirm"
  xe$correction.type <- sapply(strsplit(as.character(xe$variable), "\\."), "[", 2)
  # xe$hr.sap.ratio <- with(xe, (pi * xe$hr^2)/area.naive)
  xe$hr.sap.ratio <- with(xe, hr/sqrt(xe$area.naive/pi))
  xe$correction.type <- factor(xe$correction.type, 
                               levels = c("naive", "hr", "50", "60", "70", 
                                          "80", "90", "95", "99", "effect"))
  
  # first examine p
  xep <- gather(xe[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", 
                       "variable", "index", "hr.sap.ratio", "sessions")],
                key = p.var, value = p.val, p.target.1, p.target.sp)
  xep$correction.type <- gsub("dens\\.(.*)\\.(.*)$", "\\1", x = xep$variable)
  xep$correction.type <- factor(xep$correction.type, 
                                levels = c("naive", "hr", "50", "60", "70", 
                                           "80", "90", "95", "99", "effect"))
  xep <- xep[!duplicated(xep[, c("p.val", "p.var", "num.generated.walkers", 
                                 "true.p", "correction.type")]), ]
  xep$p.val <- 1 - (xep$p.val/xep$true.p)
  
  ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab("p_ests/p_true") +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.05, size = 0.5) +
    geom_smooth(method = "loess", se = FALSE)
  ggsave(sprintf("./figures/%s-0a pristranskost .1 in .sp ocene ulovljivosti.png", type),
         width = 10, height = 5, units = "in")
  
  ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab("p_ests/p_true") +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.1, size = 0.5) +
    geom_smooth(method = "loess", se = FALSE, size = 0.5) +
    facet_grid(. ~ num.generated.walkers)
  ggsave(sprintf("./figures/%s-0b pristranskost p.1 in p.sp glede na st. walkerjev.png", type),
         width = 7, height = 3, units = "in")
  
  # ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
  #   theme_bw() +
  #   theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #   ylab("p_ests/p_true") +
  #   scale_color_brewer(palette = "Set1") +
  #   geom_jitter(alpha = 0.1) +
  #   geom_smooth(method = "loess", se = FALSE) +
  #   facet_grid(num.generated.walkers ~ .)
  # ggsave(sprintf("./figures/%s-0c pristranskost p glede na model in st. generiranih walkerjev.png", type),
  #        width = 10, height = 5, units = "in")
  
  print("Pristranskost p glede na model in št. generiranih walkerjev")
  print(summary(glm(p.val ~ true.p * p.var * num.generated.walkers, data = xep)))
  
  ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab("p_ests/p_true") +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.1, size = 0.5) +
    geom_smooth(method = "loess", se = FALSE, size = 0.5) +
    facet_grid(. ~ correction.type)
  ggsave(sprintf("./figures/%s-0d pristranskost p glede na model in tip popravka.png", type),
         width = 7, height = 3, units = "in")
  
  # ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
  #   theme_bw() +
  #   theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #   ylab("p_ests/p_true") +
  #   scale_color_brewer(palette = "Set1") +
  #   geom_jitter(alpha = 0.1, size = 0.5) +
  #   geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  #   facet_grid(num.generated.walkers ~ correction.type)
  # ggsave(sprintf("./figures/%s-0e pristranskot p glede na sap.hr ratio po st. gen. walkerjih in popravku.png", type),
  #        width = 10, height = 5, units = "in")
  
  ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab("p_ests/p_true") +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.1, size = 0.5) +
    geom_smooth(method = "loess", se = FALSE, size = 0.5) +
    facet_grid(. ~ num.generated.walkers)
  ggsave(sprintf("./figures/%s-0f pristranskost p glede na sap.hr ratio po st. gen walkerjev brez popravka.png", type))
  
  # ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
  #   theme_bw() +
  #   theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #   ylab("p_ests/p_true") +
  #   scale_color_brewer(palette = "Set1") +
  #   geom_jitter(alpha = 0.1) +
  #   geom_smooth(method = "loess", se = FALSE) +
  #   facet_grid(. ~ correction.type)
  # ggsave(sprintf("./figures/%s-0g pristranskost p glede na sap.hr ratio po popravku brez gostote walkerjev.png", type),
  #        width = 10, height = 5, units = "in")
  
  ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab("p_ests/p_true") +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE) +
    facet_grid(sessions ~ num.generated.walkers)
  ggsave(sprintf("./figures/%s-0h pristranskost p glede sap.hr razmerje glede na st. walkerjev in st. sessionov.png", type),
         width = 10, height = 5, units = "in")
  summary(glm(p.val ~ hr.sap.ratio * sessions * p.var, data = xep))
  
  ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = as.factor(sessions))) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab("p_ests/p_true") +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE) +
    facet_grid(. ~ as.factor(num.generated.walkers))
  ggsave(sprintf("./figures/%s-0i pristranskost p glede na hr.sap.ratio in glede sessions.png", type),
         width = 10, height = 5, units = "in")
  
  # analyse density
  # remove 1% of values considered as outlayers due to numerical problems
  xe <- xe[xe$index < quantile(xe$index, probs = 0.95), ]
  fh <- 5
  fw <- fh * 1.62
  
  ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_jitter(alpha = 0.2, shape = 1) +
    geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
    scale_color_brewer(palette = "Set1") +
    geom_hline(yintercept = 1, size = 1, alpha = 0.5) +
    facet_grid(num.generated.walkers ~ correction.type)
  ggsave(sprintf("./figures/%s-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk.png", type),
         width = fw, height = fh, units = "in")
  
  ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(limits = c(0, 10)) +
    geom_hline(yintercept = 1, size = 1, alpha = 0.5) +
    facet_grid(num.generated.walkers ~ correction.type)
  ggsave(sprintf("./figures/%s-1.gostota gled na razmerje hr_sap po correction type in st. gen.walk brez pik.png", type),
         width = fw, height = fh, units = "in")
  
  ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_jitter(alpha = 0.5, shape = 1) +
    geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
    scale_color_brewer(palette = "Set1") +
    geom_hline(yintercept = 1, size = 1, alpha = 0.5) +
    facet_grid(num.generated.walkers ~ correction.type)
  ggsave(sprintf("./figures/%s-2.gostota glede na razmerje, corr.type in st. gen walkerjev pozoomano.png", type),
         width = 10, height = 8, units = "in")
  
  # AICc
  ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = hr.sap.ratio, y = dAIC)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_jitter(alpha = 0.5, shape = 1) +
    scale_color_brewer(palette = "Set1") +
    facet_grid(num.generated.walkers ~ correction.type)
  ggsave(sprintf("./figures/%s-5.dAIC gled na razmerje hr_sap po correction type in st. gen.walk.png", type),
         width = 10, height = 8, units = "in")
  
  ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = hr.sap.ratio, y = dAIC)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_color_brewer(palette = "Set1") +
    geom_jitter(alpha = 0.5, shape = 1) +
    geom_smooth(aes(color = better.model), method = "lm") +
    facet_grid(num.generated.walkers ~ correction.type)
  ggsave(sprintf("./figures/%s-6.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.png", type),
         width = 10, height = 8, units = "in")
  
  ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = model, y = dAIC)) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_violin() +
    facet_grid(num.generated.walkers ~ correction.type)
  ggsave(sprintf("./figures/%s-7 dAIC po modelih sp in 1.png", type), width = 10, height = 8, units = "in")
  
  # ~.sp vedno oceni več osebkov, zato je vedno nad ~1
  # ker MR po defaultu precenjuje dejansko gostoto, je za pričakovat, da bodo vsi nad 1
  # ker so vsi nad 1, je za pričakovat, da bo tisti, ki oceni številčnost nižjo, manj pristranski
  # # # normal # # #
}