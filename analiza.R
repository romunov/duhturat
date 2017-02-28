library(RMark)

#setwd("d:/layers")
work.dir <- "d:/layers/"
owd <- getwd()
#setwd("d:/workspace/Texas Ranger/")
#source("d:/workspace/Texas Ranger/strelisce.R")

# ANALYZE .inp FILES
setwd(work.dir)
result.file <- "results.txt"
list.mark.files <- list.files(pattern = "mark")

mark.results <- sapply(X = list.mark.files,	FUN = extractMarkResults,
  wd.dump = paste(work.dir, "analiza", sep = ""),	wd.source = work.dir,
  out = result.file)

# import results and plot/table them
rz <- read.table(file = result.file, header = TRUE)
rz$ocenjena.gostota <- rz$derived.estimate / rz$cont_supop

x <- rz[c(
#    "walk_dens", 
    "ocenjena.gostota",
    "home_range",
    "sampling_area_r",
    "real.estimate",
    "real.ucl",
    "real.lcl",
    "num_of_walkers_supop",
    "derived.estimate",
    "dAIC",
    "model",
    "weight",
    "curve"
  )]


# zajebal izpis zgodovine za v MARK, treba malo spremenit
setwd("d:/workspace/help/ranger")
setwd("d:/layers")
x <- list.files(pattern = ".inp")

sapply(x, FUN = function(d = x) {
    rlo <- readLines(d) #preberi podatke
    
    find.empty <- which(rlo == "") # najdi prelom
    rl.length <- length(rlo)
    
    rl <- data.frame(old = rlo[(find.empty+1):rl.length]) # subsetaj samo capture history
    rl <- apply(rl, MARGIN = 1, FUN = function(z) { # premeči podatke, da bo ch, group in cv
        s <- unlist(strsplit(z, " "))
        return(
          data.frame(
            ch = as.character(s[1]),
            group = as.factor(s[3]), 
            cv = as.numeric(s[2])))
      })
    rl <- do.call("rbind", rl)
    rownames(rl) <- NULL
    
    top <- rlo[1:find.empty] # najdi glavo (zakomentiran del)
    
    # skupine moras kodirat kot
    # 1 0 (1. skupina)
    # 0 1 (2. skupina)
    # (ali obratno). glej 2-4 prirocnika
    grp <- rl$group
    levels(grp) <- rev(levels(rl$group))
    rl$group2 <- as.numeric(as.character(grp))
    
    # premeči zadeve, da bo najprej capture history, polej skupine in
    # sele nato individualna kovariata
    rl <- data.frame(ch = rl$ch, group1 = rl$group, group2 = rl$group2, cv = rl$cv)
    
    # dodaj podpicje, masnico in gasa
    bottom <- paste(rl$ch, rl$group1, rl$group2, paste(rl$cv, ";", sep = ""))
    
    # ne vem tocno zakaj sm dal tule writeLines polej pa write... artisstique al ka?
    writeLines(top, con = d) 
    write(bottom, file = d, append = TRUE)
  })

# v fajlnejmu ne sme biti pik (razen za končnico)
file.dots <- list.files(pattern = ".inp")
fls <- sub("-inp", ".inp", gsub(pattern = "\\.", replacement = "-", x = file.dots))
file.rename(from = file.dots, to = fls)

# izračunaj mean po skupinah
get.mean.x <- list.files(pattern = ".inp")
get.mean.x <- c("mark-2012-08-02-14-35-mean-weight-yes.inp", "mark-2012-08-02-14-35-mean-weight-no.inp")

get.x <- sapply(get.mean.x, FUN = function(d, print = FALSE) {
    
    rlo <- readLines(d) #preberi podatke
    
    find.empty <- which(rlo == "") # najdi prelom
    rl.length <- length(rlo)
    
    purged <- rlo[(find.empty+1):rl.length]
    purge.split <- sapply(purged, FUN = strsplit, split = " ", USE.NAMES = FALSE)
    
    purge.group.subset <- lapply(purge.split, FUN = "[", c(2, 3))
    groups <- as.factor(unlist(lapply(purge.group.subset, FUN = paste, collapse = "")))
    
    purged.sp.subset <- unlist(lapply(purge.split, FUN = "[", c(4)))
    sp <- as.numeric(gsub(";", "", purged.sp.subset))
    
    purged.ch <- lapply(purge.split, FUN = "[", 1)
    ch <- lapply(lapply(purged.ch, strsplit, ""), FUN = function(m) sum(as.numeric(m[[1]])))
    ch <- unlist(ch)
    
    out.mean <- tapply(sp, groups, mean)
    out.sd <- tapply(sp, groups, sd)
    
    if (print) {
      cat("/* groups: ", paste(levels(groups), collapse = " "), "*/\n", file = d, append = TRUE)
      cat("/* mean: ", paste(out.mean, collapse = " "), "*/\n", file = d, append = TRUE)
      cat("/* sd: ", paste(out.sd, collapse = " "), "*/\n", file = d, append = TRUE)
    }
    
    return(list(groups = groups, sp = sp, nch = ch))
  }, simplify = FALSE)

out.mean <- tapply(get.x$sp, get.x$groups, mean)
out.sd <- tapply(get.x$sp, get.x$groups, sd)

# nariši capture history
ceh <- data.frame(nch = get.x[[1]]$nch, sp = get.x[[1]]$sp)
ggplot(ceh, aes(x = as.factor(nch), y = sp)) + geom_boxplot() + geom_jitter(position=position_jitter(width=0.1))
hist(ceh$nch)
summary(mmm <- lm(sp ~ nch + I(nch^2), data = ceh))
par(mfrow = c(2,2))
plot(mmm)

enke <- "mark-2012-06-07-14-39.mean.weight.yes.inp"