sampleWorld <- function(walk, sap.poly, sessions, prob, SD) {
  
  message("Performing in/out test on ", startdate <- Sys.time())
  # Román alert: Ker je računanje gCrosses kar zahtevno, smo se odločili, da bomo
  # računali samo s konveksnimi hulli individualnih walkerjev. Ker gre pri tem za
  # poenostavitev walkerja obstaja verjetnost, da se zabeleži prisotnost walkerja
  # znotraj sampling area kljub temu, da ga tam ni (povezava dveh zunanjih točk
  # seka stranico, stranice pa ne seka walker sam) - false positive. VENDAR, če
  # si zamislimo, da imamo tak robni primer, kjer hull seka sampling area, ne pa
  # tudi walker, na drugi strani pa tak primer, ko je walker čist za las znotraj
  # sampling area - ali je verjetnost, da bom ujel tega slednjega res tako velika
  # v primerjavi z verjetnostjo 0, ki velja za tistega walkerja, ki sam ne vstopa
  # v sampling area, smo pa ga uvrstili, da se nahaja znotraj ker njegov hull se-
  # ka sampling area? Po mojem sta ta dva primera zelo podobna in se lahko ta dva
  # walkerja obranava identično. Če se odločimo, da bomo zakompliciral, se lahko
  # poišče problematične walkerje in na njih požene gCrosses in se dokončno določi,
  # če spadajo v sampling area ali ne.
  
  #	walkers.inout <- sfLapply(x = walk, fun = function(x, shape) {
  walkers.inout <- lapply(walk, function(x, shape) {
    inout.over <- gOverlaps(x, shape)
    inout.cont <- gContains(x, shape)
    inout.cov <- gCovers(shape, x)
    out <- any(inout.over, inout.cont, inout.cov)
    
    # Find those that touch the border of sampling area.
    ifelse(out, return(TRUE), return(FALSE))
  }, shape = sap.poly)
  
  #	sfStop()
  
  # A little benchmark.
  duration <- Sys.time() - startdate
  message(paste("In/out test done in", duration, attr(duration, "units"), sep = " "))
  
  # Exclude walkers that never come in contact with the sampling area,
  # and are thus not detectable.
  #	walk.subset <- walk[unlist(walkers.inout)]
  
  # Sample walkers that come in contact with the sampling area are
  # passed on to function sampleWalkers.
  # CHANGED: use all walkers, no matter how many occur inside/outside
  #	walk.sample <- sampleWalkers(walk = walk.subset, sessions = sessions, prob = prob,
  #		sap = sap.poly, ...)
  
  # browser()
  # plot(sap.poly, xlim = c(-500, 500), ylim = c(-500, 500))
  # sapply(walk, plot, add = TRUE)
  
  walk.sample <- sampleWalkers(walk = walk, sessions = sessions, prob = prob,
                               sap = sap.poly, SD = SD)
  
  # Calculate pairwise distances within each walker's sampled points. 
  walk.pair <- lapply(X = walk.sample[["sample"]], FUN = function(x) {
    remove.na <- x[x$capt == 1, ]
    pairwise.distances <- dist(x = remove.na, method = "euclidean")
    out <- as.numeric(pairwise.distances)
    out
  })
  
  #	walk.sample$in.out <- length(walk.subset)
  walk.sample$in.out <- length(walk)
  walk.sample$walk.pair <- unlist(walk.pair)
  #  walk.sample$actual.ratio <- actual.ratio
  
  walk.sample
}


