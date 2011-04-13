## convenience function for time series pre-processing
## to response plus regressors (linear time trend, season
## dummies, and harmonic season by default with order 1)
tspp <- function(y, order = 1) {

  ## data with trend and season factor
  rval <- data.frame(
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

  ## return everything
  return(rval)
}

########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################

## JV A technique to verify whether or not the historical period is stable or not
## reversely order sample and perform
## recursive CUSUM test

roc <- function(y, order = 3, level = 0.05, plot = TRUE) {
  y_orig <- tspp(y, order = order)
  n      <- nrow(y_orig)
  y_rev  <- y_orig[n:1,]
  y_rev$response <- ts(y_rev$response, start = -end(y), frequency = frequency(y))
  y_rcus <- efp(response ~ trend + harmon, data = y_rev, type = "Rec-CUSUM")
  if(plot) plot(y_rcus)
  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  rval <- as.numeric(time(y)[y_start])
  if(plot & sctest(y_rcus)$p.value < level) abline(v=-rval,col="red")
  c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
}
