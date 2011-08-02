## bfast code folder

## Author: Achim Zeileis, Jan Verbesselt
####

require(strucchange)


## convenience function for time series pre-processing
## to response plus regressors (linear time trend, season
## dummies, and harmonic season by default with order 1)
tspp <- function(y, order = 1) {

  ## data with trend and season factor
  rval <- data.frame(
    time = as.numeric(time(y)),
    response = y,
    trend = 1:length(y),
    season = factor(cycle(y))
  )

  ## set up harmonic trend matrix as well
  freq <- frequency(y)
  harmon <- outer(2 * pi * as.vector(time(y)), 1:order)
  harmon <- cbind(apply(harmon, 2, cos), apply(harmon, 2, sin))
  colnames(harmon) <- if(order == 1) {
    c("cos", "sin")
  } else {
    c(paste("cos", 1:order, sep = ""), paste("sin", 1:order, sep = ""))
  }
  if((2 * order) == freq) harmon <- harmon[, -(2 * order)]
  rval$harmon <- harmon

  ## omit missing values
  rval <- na.omit(rval)
  ## return everything
  return(rval)
}

# ###############
# ########## Set parameters for simulation
# createts <- function(datats) {
#   return(ts(datats, f=23, s=c(2000,4)))
# }

########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################

## JV A technique to verify whether or not the historical period is stable or not
## reversely order sample and perform
## recursive CUSUM test
roc <- function(y, order = 3, level = 0.05) {
  y_orig <- tspp(y, order = order)
  n      <- nrow(y_orig)
  y_rev  <- y_orig[n:1,]
  y_rev$response <- ts(y_rev$response) ## , start = -tail(time(y), 1), frequency = frequency(y))
  y_rcus <- efp(response ~ trend + harmon, data = y_rev, type = "Rec-CUSUM")

  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }

  rval <- y_orig$time[y_start]
  c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
}
