#' Function performs sampling of walkers inside the sampling area.
#' 
#' @param walk A \code{list} of \code{data.frame}s with sampled coordinates of
#' 				of individual walkers.
#' @param sessions Integer. Number of sessions in which sampling is done.
#' @param prob Numeric. Values between 0 and 1, indicating the probability
#' 				of capture in each session.
#' @param sap SpatialPolygons. A \code{SpatialPolygons} object of
#' 				the sampling area.
#' @param SD Standard deviation (square root of variance) for walker's walk. Used to generate
#'        points around walker centroids.

sampleWalkers <- function(walk, sessions, prob, sap, SD) {
  
  st <- Sys.time()
  
  # sample per session, result is a list of
  # the same length as the number of sessions with each element
  # within session representing a captured walker.
  out.sampled <- lapply(as.list(seq_len(sessions)), 
                        FUN = function(xs, walkers, shape = sap, prob) {
                          
                          lapply(X = walkers, FUN = function(x, prob, shape) {
                            # sample a point inside the sampling area?
                            check <- sample(0:1, size = 1, prob = c(1-prob, prob))
                            if (check == 1)  { # if caught, sample a point
                              centers <- coordinates(x)
                              sampled.pt <- data.frame(x = rnorm(1, mean = centers[, 1], sd = SD),
                                                       y = rnorm(1, mean = centers[, 2], sd = SD))
                              sampled.pt <- SpatialPoints(sampled.pt) # needs to be a SpatialPoints
                              # and see if it falls inside the sampling polygon
                              out <- coordinates(sampled.pt)
                              out <- cbind(out, capt = 1)
                              colnames(out) <- c("x", "y", "capt")
                              
                              is.inside <- any(gCrosses(sampled.pt, shape) | gContains(sampled.pt, shape) | gCovers(shape, sampled.pt))
                              # record it if point is within the sampling area, else
                              # return an empty result
                              if (!is.inside) {
                                out[, "capt"] <- 0 # assumes only one row
                                return(out)
                              }
                              return(out)
                            } else {
                              out <- data.frame(x = NA, y = NA, capt = 0)
                              return(out)
                            }
                          }, shape = shape, prob = prob)
                          
                        }, walkers = walk, prob = prob)
  
  # head(out.sampled)
  #  [[1]]
  #  [[1]]$`2_1`
  #  x         y capt
  #  [1,] 16.40674 -18.43756    1
  #  
  #  [[1]]$`4_1`
  #  x        y capt
  #  [1,] -23.27989 -32.5211    1
  #  
  #  [[1]]$`5_1`
  #  x         y capt
  #  [1,] 55.71639 -25.37155    0
  
  # Do "rbind" for each session
  session.data <- lapply(out.sampled, function(x) do.call(rbind, x))
  
  # plot(sap, xlim = c(-300, 300), ylim = c(-300, 300), border = "red")
  # sapply(walk, plot, add = TRUE)
  
  #  > head(session.data)
  #  [[1]]
  #                  x           y capt
  #  2_1   -54.8722636  -25.624823    0
  #  4_1   -31.2483763   15.972316    1
  #  5_1    60.5537378  -53.169607    0
  #  [ reached getOption("max.print") -- omitted 48 rows ]
  #  
  #  [[2]]
  #                 x          y capt
  #  2_1   -49.415922 -32.354032    0
  #  4_1   -35.548297  11.499849    1
  #  5_1    49.220400 -54.285402    0
  #  [ reached getOption("max.print") -- omitted 48 rows ]
  
  st <- Sys.time() - st
  message(paste("Sampling took", st, attr(st, "units")))
  
  #  points(out[[1]][, 1:2], col = "blue")
  #  points(out[[2]][, 1:2], col = "red")
  #  points(out[[3]][, 1:2], col = "green")
  #  points(out[[4]][, 1:2], col = "yellow")
  #  points(out[[5]][, 1:2], col = "brown")
  
  # Extract samplings per walker. List elements are walkers,
  # and within each walker, each row in a data.frame is a session.
  # As a check, each data.frame within a walker should have the same
  # number of rows as there are sessions.
  walker.data <- lapply(session.data, function(y) {
    y[, "capt"]
  })
  capture.df <- do.call(cbind, walker.data)
  # table(rowSums(capture.df)) # TODO: WTF, kam grejo vsi ulovi iste živali?
  
  #> capture.df
  #	     [,1] [,2] [,3] [,4] [,5]
  #	[1,]    1    1    1    1    0
  #	[2,]    0    0    0    0    0
  #	[3,]    0    1    0    1    1
  #	[4,]    0    0    0    0    0
  
  # Create a list where each element is a walker with each
  # line in a data.frame representing data from sampling sessions.
  
  captured.walkers <- as.data.frame(do.call(rbind, session.data))
  captured.walkers$walker <- unlist(sapply(strsplit(
    rownames(captured.walkers), "_"), "[", 1))
  captured.walkers <- split(captured.walkers, f = captured.walkers$walker)
  
  captured.walkers <- lapply(1:length(walk), function(y) {
    do.call("rbind", lapply(session.data, FUN = function(m) {
      m[y, , drop = FALSE]
    }))
  })
  
  #  > head(captured.walkers)
  #  [[1]]
  #  x         y capt
  #  11_1  -48.59758 -2.698293    0
  #  11_1  -46.33887 18.799103    0
  #  11_1 -104.20080 21.042796    0
  #  [ reached getOption("max.print") -- omitted 2 rows ]
  #  
  #  [[2]]
  #  x        y capt
  #  12_1 -46.54251 67.73398    0
  #  12_1 -76.49958 57.07864    0
  #  12_1 -40.63284 45.58006    0
  #  [ reached getOption("max.print") -- omitted 2 rows ]
  
  # Remove any walkers that were never sampled.
  empty.cases <- rowSums(capture.df, na.rm = TRUE)
  capture.df <- capture.df[empty.cases != 0, ] # remove those walkers with no capture
  
  message(paste("sampleWalkers:", length(walk) - nrow(capture.df), " walkers were never sampled within the sampling area"))
  
  # omit those that have never been caught even once
  captured.walkers <- sapply(captured.walkers, FUN = function(x) {
    cds <- apply(x, MARGIN = 1, FUN = function(m) !any(is.na(m)))
    
    x[(x$capt * cds) != 0, ]
  }, simplify = FALSE)
  find.nodata <- sapply(captured.walkers, nrow)
  
  captured.walkers <- captured.walkers[find.nodata != 0]
  
  # Find information whether walker comes from the border or core of the sampling area
  # find.pos <- unlist(lapply(strsplit(names(walk), "_"), "[[", 2))
  # find.pos <- find.pos[which(empty.cases != 0)]
  
  # Convert data for each walker into a data.frame.
  captured.walkers <- lapply(captured.walkers, function(p) {
    rownames(p) <- 1:nrow(p)
    data.frame(p)
  })
  
  result <- list(
    capture = capture.df, # a matrix of captures/non-captures
    sample = captured.walkers # a list of data.frames of walkers. each data.frame holds locations to captures/non-captures
  )
  
  # result is a list of two (or just capture if there are no missing walkers (unlikely but possible)
  # $capture (a matrix, columns are sessions, rows are walkers)
  #     [,1] [,2] [,3] [,4] [,5]
  # [1,]    1    1    0    0    0
  # [2,]    0    0    0    1    0
  # [3,]    1    0    0    0    0
  # [4,]    1    0    0    0    1
  # [5,]    1    0    0    0    1
  # ...
  # $sample :List of 26
  # ..$ :'data.frame':	5 obs. of  3 variables:
  #	.. ..$ x   : num [1:5] -7.18 -20.65 NA NA NA
  # .. ..$ y   : num [1:5] 97.5 88.3 NA NA NA
  # .. ..$ capt: num [1:5] 1 1 0 0 0
  # ...
  # $ included: chr [1:172] "1" "1" "1" "1" ...
  result
}
