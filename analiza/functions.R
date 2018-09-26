createFigures <- function(xy, type = c("N", "E"), ...) {
  #' ... can be a vector of named logical values which will make plot and summary print - or not.
  #' See code for possible values (e.g. "a0", "b0", "a1"). Example of usage would be:
  #' createFigures(xy, type = "N", c(a0 = TRUE, b0 = FALSE, a1 = TRUE))
  
  ps <- as.list(...) # ps = plot switch
  
  if (type == "E") {
    xy <- droplevels(xy[xy$fun == "empirical", ])
  }
  
  if (type == "N") {
    xy <- droplevels(xy[xy$fun == "normal", ])
  }
  
  # prepare for calculations with density
  xe <- gather(xy, key = variable, value = index, starts_with("dens."))
  rownames(xe) <- NULL
  xe$variable <- factor(xe$variable)
  
  xe$model <- ".1"
  xe$model[grepl(".sp$", xe$variable)] <- ".sp"
  xe$model[grepl(".tirm", xe$variable)] <- ".tirm"
  xe$correction.type <- sapply(strsplit(as.character(xe$variable), "\\."), "[", 2)
  xe$hr.sap.ratio <- with(xe, hr/sqrt(xe$area.naive/pi)) # ratio of r is equivalent to ratio of areas
  xe$correction.type <- factor(xe$correction.type, 
                               levels = c("naive", "hr", "50", "60", "70", 
                                          "80", "90", "95", "99", "effect"))
  
  # prepare data to examine p for Huggins' model
  xep <- gather(xe[, c("true.p", "p.target.1", "p.target.sp", "num.generated.walkers", 
                       "variable", "index", "hr.sap.ratio", "sessions")],
                key = p.var, value = p.val, p.target.1, p.target.sp)
  xep$correction.type <- gsub("dens\\.(.*)\\.(.*)$", "\\1", x = xep$variable)
  xep$correction.type <- factor(xep$correction.type, 
                                levels = c("naive", "hr", "50", "60", "70", 
                                           "80", "90", "95", "99", "effect"))
  xep <- xep[!duplicated(xep[, c("p.val", "p.var", "num.generated.walkers", 
                                 "true.p", "correction.type")]), ]
  xep$p.val <- xep$p.val/xep$true.p
  
  # Prepare labellers
  labeller.addN <- function(string) {
    return(sprintf("N = %s", string))
  }
  
  labeller.addK <- function(string) {
    return(sprintf("K = %s", string))
  }
  
  if (any(names(ps) %in% "a0")) {
    ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Simulirana vrednost p") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.05, size = 2, shape = 1) +
      geom_smooth(method = "loess", se = FALSE)
    ggsave(sprintf("./figures/%s-0a pristranskost .1 in .sp ocene ulovljivosti.pdf", type),
           width = 5, height = 5, units = "in", device = cairo_pdf)
    
    message(sprintf("Printing global average for model %s", type))
    print(summary(glm(p.val ~ p.var, data = xep)))
  }
  
  if (any(names(ps) %in% "b0")) {
    ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text = element_text(size = 6)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Simulirana vrednost p") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.1, size = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ num.generated.walkers, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-0b pristranskost p.1 in p.sp glede na st. walkerjev.pdf", type),
           width = 7, height = 3, units = "in", device = cairo_pdf)
    
    print("Pristranskost p glede na model in št. generiranih walkerjev")
    print(summary(glm(p.val ~ true.p * p.var * num.generated.walkers, data = xep)))
  }
  
  if (any(names(ps) %in% "d0")) {
    ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Simulirana vrednost p") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.2, size = 1) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ correction.type)
    ggsave(sprintf("./figures/%s-0d pristranskost p glede na model in tip popravka.pdf", type),
           width = 7, height = 3, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "f0")) {
    ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.1, size = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ num.generated.walkers, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-0f pristranskost p glede na sap.hr ratio po st. gen walkerjev brez popravka.pdf", type),
           device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "h0")) {
    ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.1, size = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(sessions ~ num.generated.walkers, labeller = labeller(sessions = labeller.addK,
                                                                       num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-0h pristranskost p glede sap.hr razmerje glede na st. walkerjev in st. sessionov.pdf", type),
           width = 6, height = 5, units = "in", device = cairo_pdf)
    summary(glm(p.val ~ hr.sap.ratio * sessions * p.var, data = xep))
  }
  
  if (any(names(ps) %in% "i0")) {
    ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = as.factor(sessions))) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      scale_color_brewer(palette = "Set1", name = "Št. odlovnih intervalov (K)") +
      geom_jitter(alpha = 0.1, size = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ num.generated.walkers, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-0i pristranskost p glede na hr.sap.ratio in glede sessions.pdf", type),
           width = 10, height = 5, units = "in", device = cairo_pdf)
    
  }
  # analyse density
  # remove 1% of values considered as outlayers due to numerical problems
  xe <- xe[xe$index < quantile(xe$index, probs = 0.95), ]
  fh <- 6
  fw <- fh * 1.62
  
  if (any(names(ps) %in% "a1")) {
    ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      scale_y_continuous(limits = c(0, 5)) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-1a.gostota gled na razmerje hr_sap po correction type in st. gen.walk.pdf", type),
           width = fw, height = fh, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "b1")) {
    ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      scale_y_continuous(limits = c(0, 10)) +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-1b.gostota gled na razmerje hr_sap po correction type in st. gen.walk brez pik.pdf", type),
           width = fw, height = fh, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "c1")) {
    ggplot(xe[xe$sessions == 5, ], aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      scale_y_continuous(limits = c(0, 5)) +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      labs(caption = "K = 5") +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-1c.gostota gled na razmerje hr_sap po correction type in st. gen.walk za k5.pdf", type),
           width = fw, height = fh, units = "in", device = cairo_pdf)
  }
  if (any(names(ps) %in% "d1")) {
    ggplot(xe[xe$sessions == 10, ], aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      scale_y_continuous(limits = c(0, 5)) +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      labs(caption = "K = 10") +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-1d.gostota gled na razmerje hr_sap po correction type in st. gen.walk za k10.pdf", type),
           width = fw, height = fh, units = "in", device = cairo_pdf)
  }
  if (any(names(ps) %in% "e1")) {
    ggplot(xe[xe$sessions == 15, ], aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      scale_y_continuous(limits = c(0, 5)) +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      labs(caption = "K = 15") +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-1e.gostota gled na razmerje hr_sap po correction type in st. gen.walk za k15.pdf", type),
           width = fw, height = fh, units = "in", device = cairo_pdf) 
  }
  
  if (any(names(ps) %in% "a2")) {
    ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_jitter(alpha = 0.5, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-2a.gostota glede na razmerje, corr.type in st. gen walkerjev pozoomano.pdf", type),
           width = 10, height = 8, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "a5")) {
    # AICc
    ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = hr.sap.ratio, y = dAIC)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(Delta ~ "AIC")) +
      theme(legend.position = "top", axis.title.y = element_text(angle = 90, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_jitter(alpha = 0.5, shape = 1, size = 0.5) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-5a.dAIC gled na razmerje hr_sap po correction type in st. gen.walk.pdf", type),
           width = 10, height = 8, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "a6")) {
    ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = hr.sap.ratio, y = dAIC, color = better.model)) +
      theme_bw() +
      ylab(expression(Delta ~ "AIC")) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      theme(legend.position = "top", axis.title.y = element_text(angle = 90, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.5, shape = 1, size = 0.5) +
      guides(color = guide_legend(override.aes = list(size = 2), alpha = 1)) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-6a.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.pdf", type),
           width = 10, height = 8, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "b6")) {
    ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = hr.sap.ratio, y = dAIC, color = better.model)) +
      theme_bw() +
      ylab(expression(Delta ~ "AIC")) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      theme(legend.position = "top", axis.title.y = element_text(angle = 90, vjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      guides(color = guide_legend(override.aes = list(size = 2), alpha = 1)) +
      geom_jitter(alpha = 0.5, shape = 1, size = 0.5)
    ggsave(sprintf("./figures/%s-6b.dAIC glede na razmerje hr_sap po correction type in st.gen.walk in boljsi model.pdf", type),
           width = 10, height = 8, units = "in", device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "a7")) {
    ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = model, y = dAIC)) +
      theme_bw() +
      ylab(expression(Delta ~ "AIC")) +
      xlab("Model") +
      theme(legend.position = "top", axis.title.y = element_text(angle = 90, vjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_violin() +
      scale_x_discrete(labels = c(expression(M[0]), expression(M[sp]))) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = labeller.addN))
    ggsave(sprintf("./figures/%s-7a dAIC po modelih sp in 1.pdf", type), width = 10, height = 8, units = "in", 
           device = cairo_pdf)
  }
  
  if (any(names(ps) %in% "a8")) {
    # This is needed to make pretty facet titles
    daic.hist <- xy
    daic.hist$better.model <- factor(daic.hist$better.model, labels = c(expression(M[0]), expression(M[sp])))
    
    ggplot(daic.hist, aes(x = dAIC)) +
      theme_bw() +
      ylab("Frekvenca") +
      xlab(expression(Delta ~ "AIC")) +
      geom_histogram() +
      facet_wrap(~ better.model, scales = "free_x", labeller = label_parsed)
    ggsave(sprintf("./figures/%s-8a dAIC glede na najboljsi model.pdf", type), width = 8, height = 4, units = "in", 
           device = cairo_pdf)
  }
}
