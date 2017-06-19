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
#' @param pars A list of parameters used to simulate the data.
#' @param seed This is used to construct a unique filename.
#' 
#' @author Roman Luštrik (\email{roman.lustrik@gmail.com})
# roxygenize()

writeINP <- function(object, supop = NULL, pars, seed) {
  
  if (is.null(pars$comment)) pars$comment <- NA
  if (is.null(supop)) supop <- ""
  
  file.name <- sub(pattern = ".txt", replacement = "", x = pars$file.name) # remove .txt
  file.name <- paste(file.name,"_", seed, ".inp", sep = "")
  
  #FOR TESTING: prepare grouping variable
  # group <- object$sample$include
  
  probs <- data.frame(object$contribs$cona$weight.yes$probs)
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
  
  mat <- cbind(cap.hist = cap.hist, probs = probs)
  
  # prepare data.frame for printing
  par.df <-	data.frame(
    num_of_walkers_supop = object$sample$in.out, # number of walkers generated in super population
    sessions = nchar(as.character(cap.hist[1, ])), # number of sessions
    sampling_area_r = pars$sap, # r of the sampling area
    num_of_sampled_walkers = nrow(cap.hist), # number of sampled walkers
    generated_walkers = pars$num.walker, # number of generated walkers
    area_size = pars$area, # are of the sampling area (for resolution 1)
    home_range = pars$home.range, # home range of walker
    effect_distance = object$contribs$effect.distance, # effect distance used, max walked distance
    capture_prob = pars$prob, # probability of capturing a walker
    boots = pars$num.boots, # number of boots
    work_dir = pars$work.dir, # working directory where magic takes place
    res = pars$rsln, # resolution of raster aggregation
    SD = pars$SD, # standard deviation used in simulation
    sim.dist = pars$sim.dist, # which type of individual contributes was calculated, normal od empirical?
    seed = pars$seed, # number of seed used
    hazard_fun_sigma = object$contribs$cona$weight.yes$hazard_fun_params["sigma"],
    hazard_fun_b = object$contribs$cona$weight.yes$hazard_fun_params["b"],
    hazard_fun_mx = object$contribs$cona$weight.yes$hazard_fun_params["mx"]
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
  
  return(par.df)
}