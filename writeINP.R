#' Write data to MARK compatible file
#' 
#' This function will take a capture history matrix (i.e. output of 
#' sampleWithinSamplingArea()) and format the data to fit per instructions in
#' MARK book. Output is an .inp file (see Chapter 2 in the MARK book).
#' 
#' @param object	List. An compound object from stepRasterWalker. Relevant
#'			information is extracted automatically.
#' @param supop		Numeric. A character of length one as a result of superPopulation
#'			function.
#' @param comment Character. A comment to be written to the file.
#' @param probs A list of probabilities ("sp") for individual walker.
#' @param pars A list of parameters used to simulate the data.
#' @author Roman Luštrik (\email{roman.lustrik@gmail.com})
# roxygenize()

writeINP <- function(object, supop = NULL, pars, probs) {
  if (is.null(pars$comment)) pars$comment <- NA
  if (is.null(supop)) supop <- ""
  
  file.name <- sub(pattern = ".txt", replacement = "", x = pars$file.name) # remove .txt
  file.name <- paste(file.name, ".inp", sep = "")
  # file.name <- paste(paste(file.name, colnames(supop), attr(supop, "weight"), sep = "-"), "inp", sep = ".")
  
  #FOR TESTING: prepare grouping variable
  # group <- object$sample$include
  
  ###### COMPARE REAL, MODELED AND SAMPLED #######
  sp.real <- object$sample$actual.ratio
  sp.mdl <- object$contrib$model.contrib$weight.yes
  sp.smp <- object$contrib$cona$weight.yes
  
  # merge sampled contributions
  # smp.nms <- as.numeric(unlist(lapply(strsplit(rownames(sp.smp), "_"), "[", 1)))
  #  mrg <- data.frame(real = sp.real, model = sp.mdl, sample = NA)
  # mrg <- cbind(model = sp.mdl[smp.nms, ], sample = sp.smp)
  #  mrg[smp.nms, "sample"] <- as.numeric(sp.smp) 
  # plot(mrg)
  # mrg.sorted <- mrg[order(sp.real), ]
  
  # pdf(paste("diagnostics_plot", paste(format(Sys.time(), format = "%Y-%m-%d_at_%H:%M"), "pdf", sep = "."), sep = "_", collapse = "_"))
  
  #  plot(mrg.sorted$real, mrg.sorted$mean)
  # plot(mrg.sorted$real, mrg.sorted$sample, xlim = c(0, 1), ylim = c(0, 1))
  # points(mrg.sorted$real, mrg.sorted$mean, pch = 1, col = "red")
  # lines(mrg.sorted$real, mrg.sorted$mean, col = "red")
  
  #  plot(x = 1:length(sp.real), ylim = c(0, 1), type = "n")
  #  lines(1:length(sp.real), mrg.sorted$real, col = "red")
  #  lines(1:length(sp.real), mrg.sorted$mean, col = "black")
  #  points(1:length(sp.real), mrg.sorted$sample, col = "blue")
  #  legend("topleft", legend = c("real", "model"), col = c("red", "black"), lty = 1)
  
  # rl <- mrg[is.na(mrg$sample), "real"]
  # plot(ecdf(rl))
  # summary(ecdf(rl))
  
  ###### COMPARE REAL, MODELED AND SAMPLED #######
  
  probs <- data.frame(unlist(probs))
  names(probs) <- NULL
  rownames(probs) <- NULL
  
  ## Construct a working directory where files will be saved.
  work.dir <- pars$work.dir
  # work.dir <- paste(dirname(work.dir), basename(work.dir), sep = "")
  summary.file <- paste(work.dir, pars$summary.file, sep = "/")
  
  # Collapse capture history
  cap.hist <- apply(object$sample$capture, 1, function(x) {
    paste(x, collapse = "")
  })
  cap.hist <- as.data.frame(cap.hist)
  
  mat <- cbind(cap.hist = cap.hist, probs = probs) # , group = group)
  
  # prepare data.frame for printing
  par.df <-	data.frame(
    #		walk_dens = pars$dens, # walker density
    # cont_supop = supop["supop", ], # contribution area of super population
    # cont_sample = supop["sample", ], # contribution area of sampling area
    num_of_walkers_supop = object$sample$in.out, # number of walkers in super population
    sessions = nchar(as.character(cap.hist[1, ])), # number of sessions
    sampling_area_r = pars$sap, # r of the sampling area
    num_of_sampled_walkers = nrow(cap.hist), # number of sampled walkers
    generated_walkers = pars$num.walker, # number of generated walkers
    area_size = pars$area, # are of the sampling area (for resolution 1)
    home_range = pars$home.range, # home range of walker
    effect_distance = object$contribs$effect.distance, # effect distance used
    capture_prob = pars$prob, # probability of capturing a walker
    boots = pars$num.boots, # number of boots
    work_dir = pars$work.dir, # working directory where magic takes place
    #	num_cores = pars$num.cores, # number of cores used
    res = pars$rsln, # resolution of raster aggregation
    sim.dist = pars$sim.dist, # which type of individual contributes was calculated, normal od empirical?
    seed = pars$seed # number of seed used
  )
  
  file.to.write <- paste(work.dir, "/", file.name, sep = "")
  
  # Send everything down the drain
  # first write down the header
  tdf <- (Sys.time() - pars$start.date)
  cat("/* File created on", date(), "\n", file = file.to.write, append = TRUE)
  cat("Simulation run:", tdf, attr(tdf, "units"), "\n", file = file.to.write, append = TRUE)
  
  # write computer-readable parameters
  cat("read start", "\n", file = file.to.write, append = TRUE)
  write.table(t(colnames(par.df)), file = file.to.write,
              append = TRUE, col.names = FALSE, row.names = FALSE)
  write.table(par.df, file = file.to.write, row.names = FALSE,
              col.names = FALSE, append = TRUE)
  cat("read stop", "\n", file = file.to.write, append = TRUE)
  
  # write human readable data (duplicate of the par.df above)
  #  cat("Walker density:", pars$walk.dens, "\n", file = file.to.write, append = TRUE)
  # cat("Contribution area of super population:", supop["supop", ], "\n", file = file.to.write, append = TRUE)
  # cat("Contribution area of sampling area:", supop["sample", ], "\n", file = file.to.write, append = TRUE)
  cat("Number of walkers in super population:", object$sample$in.out, "\n", file = file.to.write, append = TRUE)
  cat("Capture history has", nchar(as.character(cap.hist[1, ])),
      "sessions and", nrow(cap.hist), "walkers", "\n", file = file.to.write, append = TRUE)
  cat("Number of initial walkers:", pars$num.walker, "\n", file = file.to.write, append = TRUE)
  cat("Number of steps per walker:", pars$n.steps, "\n", file = file.to.write, append = TRUE)
  cat("Area size:", pars$area, "\n", file = file.to.write, append = TRUE)
  cat("Home range (2r):", pars$home.range, "\n", file = file.to.write, append = TRUE)
  cat("Effect distance:", object$contribs$effect.distance, "\n", file = file.to.write, append = TRUE)
  cat("Capture probability used:", pars$prob, "\n", file = file.to.write, append = TRUE)
  cat("Number of sessions:", nchar(as.character(cap.hist[1, ])), "\n", file = file.to.write, append = TRUE)
  cat("Number of bootstraps:", pars$num.boots, "\n", file = file.to.write, append = TRUE)
  cat("Working directory:", pars$work.dir, "\n", file = file.to.write, append = TRUE)
  cat("Number of cores for parallel:", pars$num.cores, "\n", file = file.to.write, append = TRUE)
  cat("Resolution of raster:", pars$rsln, "\n", file = file.to.write, append = TRUE)
  cat("Distribution used for individual contribution:", pars$sim.dist, "\n", file = file.to.write, append = TRUE)
  cat("Seed:", pars$seed, "\n", file = file.to.write, append = TRUE)
  cat("Comment:", pars$comment, "\n", file = file.to.write, append = TRUE)
  cat("Distribution used to calculate contribution:", pars$sim.dist, "\n", file = file.to.write, append = TRUE)
  cat("Columns: cap.hist - group - probs */\n\n", file = file.to.write, append = TRUE)
  
  # write capture history with individual covariates
  write.table(mat, file = file.to.write, append = TRUE, 
              col.names = FALSE, row.names = FALSE, quote = FALSE)
  
  # Write some parameters to a centralized data base to give an overview of
  # files available.
  # area - home.range - num.walker - file - comment
  write.out <- data.frame(pars$area, pars$home.range, pars$num.walker, file.name,
                          pars$comment)
  
  if (file.exists(summary.file)) {
    write.table(x = write.out, file = summary.file, append = TRUE, sep = "\t",
                col.names = FALSE, row.names = FALSE)
  } else {
    write.table(x = write.out, file = summary.file,	append = FALSE, sep = "\t",
                col.names = TRUE, row.names = FALSE)
  }
  
  message("Done writing MARK file ", file.name)
  
  #	out <- data.frame(
  #			area.sample = supop["sample"], # contribution area of sampling area
  #			area.supop = supop["supop"], # contribution area of super population
  #			area.sample.circ = 2 * pi * bbox(sap)[3],
  #			area.sample.area = sap@polygons[[1]]@area,
  #			r.of.sap = bbox(sap)[1, "max"], # r of sampling area
  #			sim.walkers.supop = object$calculate.walkers$walk.sample$in.out, # number of walkers in super population
  #			sim.dens = pars$walk.dens, # simulated walker density
  #			sim.dens.supop = object$calculate.walkers$walk.sample$in.out / supop["supop"], # density of walkers in the area of super population
  #			sim.catch = pars$prob, #simulated probability of capture
  #			home.range = pars$home.range, # home range of walker
  #			effect.dist = object$parameters$effect.distance, # effect distance used as a cut-off
  #			sessions = pars$sessions, # number of sessions used to capture walkers
  #			filename = file.name # file name of the MARK inp file
  #	)
  #	rownames(out) <- NULL
  
  return(par.df)
}