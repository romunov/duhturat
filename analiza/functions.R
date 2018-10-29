createFigures <- function(xy, type = c("N", "E"), dev, ...) {
  #' ... can be a vector of named logical values which will make plot and summary print - or not.
  #' See code for possible values (e.g. "a0", "b0", "a1"). Example of usage would be:
  #' createFigures(xy, type = "N", c(a0 = TRUE, b0 = FALSE, a1 = TRUE))
  #' `dev` is there to control what to output, e.g. pdf or png
  
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
  
  # generate labels, number of expected walkers within the sampling area
  # 1000 = area, world used to generate walkers
  # round to 10
  exp.N.sap <- round(sort(unique(xe$num.generated.walkers))/(pi * 1000^2) * 125663.7, -1)
  exp.N.sap <- sprintf("bar(N)~'='~%s", exp.N.sap)
  xe$num.generated.walkers <- factor(xe$num.generated.walkers,
                                      levels = sort(unique(xe$num.generated.walkers)),
                                      labels = exp.N.sap)
  
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
  
  # Prepare labeller
  labeller.addK <- function(string) {
    return(sprintf("K = %s", string))
  }
  
  # display legend only for E plots
  if (type == "E") {
    smart.legend <- theme(legend.position = "top")
  } else {
    # smart.legend <- theme(legend.position = "none")
    smart.legend <- theme(legend.position = "top")
  }
  
  if (any(names(ps) %in% "a0")) {
    ggplot(xep, aes(x = true.p, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Simulirana vrednost p") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.01, size = 2, shape = 1) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
    ggsave(sprintf("./figures/%s-0a_pristranskost_1_in_sp_ocene_ulovljivosti.%s", type, dev),
           width = 5, height = 3, units = "in")
    
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
      geom_jitter(alpha = 0.01, size = 2, shape = 1) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ num.generated.walkers, labeller = label_parsed)
    ggsave(sprintf("./figures/%s-0b_pristranskost_p1_in_psp_glede_na_st_walkerjev.%s", type, dev),
           width = 7, height = 3, units = "in")
    
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
      geom_jitter(alpha = 0.1, size = 2, shape = 1) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ correction.type)
    ggsave(sprintf("./figures/%s-0d_pristranskost_p_glede_na_model_in_tip_popravka.%s", type, dev),
           width = 7, height = 3, units = "in")
  }
  
  if (any(names(ps) %in% "f0")) {
    ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
      theme_bw() +
      theme(legend.position = "top", 
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 11, vjust = -1.5),
            axis.text.x = element_text(angle = 0, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      ylab(expression(frac(hat(p), p))) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.01, size = 1, shape = 1) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(. ~ num.generated.walkers, labeller = label_parsed)
    ggsave(sprintf("./figures/%s-0f_pristranskost_p_glede_na_sap_hr_ratio_po_st_gen_walkerjev_brez_popravka.%s", type, dev),
           width = 7, height = 3)
  }
  
  if (any(names(ps) %in% "h0")) {
    ggplot(xep, aes(x = hr.sap.ratio, y = p.val, color = p.var)) +
      theme_bw() +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 11, vjust = -1.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      smart.legend +
      ylab(expression(frac(hat(p), p))) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      geom_jitter(alpha = 0.01, size = 1, shape = 1) +
      geom_smooth(method = "loess", se = FALSE, size = 0.5) +
      facet_grid(sessions ~ num.generated.walkers, labeller = labeller(sessions = labeller.addK,
                                                                       num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-0h_pristranskost_p_glede_sap_hr_razmerje_glede_na_st_walkerjev_in_st_sessionov.%s", type, dev),
           width = 6, height = 5, units = "in")
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
      facet_grid(. ~ num.generated.walkers, labeller = label_parsed)
    ggsave(sprintf("./figures/%s-0i_pristranskost_p_glede_na_hr_sap_ratio_in_glede_sessions.%s", type, dev),
           width = 10, height = 5, units = "in")
    
  }
  # analyse density
  # remove 1% of values considered as outlayers due to numerical problems
  xe <- xe[xe$index < quantile(xe$index, probs = 0.95), ]
  fh <- 5
  fw <- fh * 1.62
  
  if (any(names(ps) %in% "a1")) {
    ggplot(xe, aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 12, vjust = -1.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      smart.legend +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      scale_y_continuous(limits = c(0, 5)) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_jitter(alpha = 0.05, shape = 1, size = 1) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-1a_gostota_gled_na_razmerje_hr_sap_po_correction_type_in_st_gen_walk.%s", type, dev),
           width = fw, height = fh, units = "in")
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
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-1b_gostota_gled_na_razmerje_hr_sap_po_correction_type_in_st_gen_walk_brez_pik.%s", type, dev),
           width = fw, height = fh, units = "in")
  }
  
  if (any(names(ps) %in% "c1")) {
    ggplot(xe[xe$sessions == 5, ], aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      theme(legend.position = "top", 
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 12, vjust = -1.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      scale_y_continuous(limits = c(0, 5)) +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      labs(caption = "K = 5") +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-1c_gostota_gled_na_razmerje_hr_sap_po_correction_type_in_st_gen_walk_za_k5.%s", type, dev),
           width = fw, height = fh, units = "in")
  }
  if (any(names(ps) %in% "d1")) {
    ggplot(xe[xe$sessions == 10, ], aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      scale_y_continuous(limits = c(0, 5)) +
      theme(legend.position = "top", 
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 12, vjust = -1.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      labs(caption = "K = 10") +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-1d_gostota_glede_na_razmerje_hr_sap_po_correction_type_in_st_gen_walk_za_k10.%s", type, dev),
           width = fw, height = fh, units = "in")
  }
  if (any(names(ps) %in% "e1")) {
    ggplot(xe[xe$sessions == 15, ], aes(x = hr.sap.ratio, y = index)) +
      theme_bw() +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      ylab(expression(frac(hat(D), D))) +
      scale_y_continuous(limits = c(0, 5)) +
      theme(legend.position = "top", 
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 12, vjust = -1.5),
            axis.text.x = element_text(angle = 90, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      geom_jitter(alpha = 0.2, shape = 1, size = 0.5) +
      geom_smooth(aes(color = model), method = "loess", se = TRUE, size = 0.5) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp]), expression(M[tirm])), name = "Model") +
      geom_hline(yintercept = 1, size = 0.5, alpha = 0.5) +
      labs(caption = "K = 15") +
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-1e_gostota_glede_na_razmerje_hr_sap_po_correction_type_in_st_gen_walk_za_k15.%s", type, dev),
           width = fw, height = fh, units = "in") 
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
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-2a_gostota_glede_na_razmerje_corr_type_in_st_gen_walkerjev_pozoomano.%s", type, dev),
           width = 10, height = 8, units = "in")
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
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-5a_dAIC_gled_na_razmerje_hr_sap_po_correction_type_in_st_gen.walk.%s", type, dev),
           width = 10, height = 8, units = "in")
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
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-6a_dAIC_glede_na_razmerje_hr_sap_po_correction_type_in_st_gen.walk_in_boljsi_model.%s", type, dev),
           width = 10, height = 8, units = "in")
  }
  
  if (any(names(ps) %in% "b6")) {
    ggplot(droplevels(xe[xe$model %in% c(".1", ".sp"), ]), aes(x = hr.sap.ratio, y = dAIC, color = better.model)) +
      theme_bw() +
      ylab(expression(Delta ~ "AIC")) +
      xlab("Razmerje med velikostjo domačega okoliša in velikostjo območja vzorčenja") +
      theme(legend.position = "top", 
            axis.title.y = element_text(angle = 90, vjust = 0.5),
            axis.title.x = element_text(size = 10, vjust = -0.8),
            axis.text.x = element_text(angle = 0, vjust = 0.4),
            axis.text = element_text(size = 6)) +
      scale_color_brewer(palette = "Set1", labels = c(expression(M[0]), expression(M[sp])), name = "Model") +
      guides(color = guide_legend(override.aes = list(size = 2), alpha = 1)) +
      geom_jitter(alpha = 0.01, shape = 1, size = 1) +
      guides(color = guide_legend(override.aes = list(alpha = 1)))
    ggsave(sprintf("./figures/%s-6b_dAIC_glede_na_razmerje_hr_sap_po_correction_type_in_st_gen_walk_in_boljsi_model.%s", type, dev),
           width = 6, height = 4, units = "in")
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
      facet_grid(num.generated.walkers ~ correction.type, labeller = labeller(num.generated.walkers = label_parsed))
    ggsave(sprintf("./figures/%s-7a_dAIC_po_modelih_sp_in_1.%s", type, dev), width = 10, height = 8, units = "in")
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
    ggsave(sprintf("./figures/%s-8a_dAIC_glede_na_najboljsi_model.%s", type, dev), width = 8, height = 4, units = "in")
  }
}
