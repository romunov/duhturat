# This code will show the way R and MARK estimate parameters. Hopefully the results match.

library(RMark)
source("markAnalysis.R")

calculateMarkFast <- function(x) {
  
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  tmp <- tempdir()
  on.exit(unlink(tmp), add = TRUE)
  setwd(tmp)
  
  # Run MARK analysis using the Huggins model.
  tr.process <- process.data(x, model = "Huggins")
  tr.ddl <- make.design.data(tr.process)
  
  # https://sites.google.com/site/workshoponcmr/home/sche/10-closed-capture/huggins
  p.cequal.dot <- list(formula = ~ 1, share = TRUE)
  
  # collect models and run
  cml <- create.model.list("Huggins")
  tr.result <- mark.wrapper(cml, data = tr.process, ddl = tr.ddl, adjust = FALSE,
                            invisible = TRUE)
  setwd(oldwd)
  tr.result
}

 
est.mark <- markAnalysis(fn = "../data/mark-2017-06-26-10-44_normal_961_r.inp")

# > est.mark$est.der.pars
#   N.Population.Size.estimate N.Population.Size.se model.name     AICc
#                     709.5983             53.21387   p(~1)c() 1523.715
#                     807.1248             84.84580  p(~sp)c() 1508.481
# > est.mark$real.fun.pars
#           estimate        se model.name     AICc
#   g1 t1  0.1610066 0.0139060   p(~1)c() 1523.715
#   g1 t11 0.1495291 0.0142513  p(~sp)c() 1508.481

# In MARK, model c=p(.) design matrix is
# B1
#  1

#  results:
# Estimates of Derived Parameters
# Population Estimates of {c=p(.)}
# 95% Confidence Interval
#  Group        N-hat        Standard Error      Lower           Upper
# ---------  --------------  --------------  --------------  --------------
#   1         709.59836       53.213857       619.78526       830.22446   

# Real Function Parameters of {c=p(.)}
# 95% Confidence Interval
#    Parameter                  Estimate       Standard Error      Lower           Upper
# --------------------------  --------------  --------------  --------------  --------------
#      1:p                     0.1610066       0.0139060       0.1355765       0.1901573       



# and c=p(sp) design matrix is
# B1 Param  B2
#  1   1:p  sp

# results:

# Estimates of Derived Parameters
# Population Estimates of {c=p(sp)}
# 95% Confidence Interval
#   Group       N-hat        Standard Error      Lower           Upper
# ---------  --------------  --------------  --------------  --------------
#    1         807.12478       84.845827       669.14875       1006.2850   


# Real Function Parameters of {c=p(sp)}
# Following estimates based on unstandardized individual covariate values:
#   Variable   Value         
# ---------  ------------- 
#   SP         0.7575902     
# 95% Confidence Interval
# Parameter                      Estimate       Standard Error      Lower           Upper
# --------------------------  --------------  --------------  --------------  --------------
#   1:p                          0.1495291       0.0142513       0.1236895       0.1796600     