# bfast code folder
## convenience function for time series pre-processing
## to response plus regressors (linear time trend, season
## dummies, and harmonic season by default with order 1)
require(strucchange)
require(zoo)
#   tsdata <- tsndvi
#   startmonitor <- c(2009,1)
#   order <- 3
#   title <- TRUE

realtime <- function(tsdata, startmonitor, order, title) {
  ## first define the history period
  tshistory <- window(tsdata, end = startmonitor)

  ## 1. check for stability in the history period
  subset_start <- roc(tshistory, order = order, level = 0.05) # searching for a stable period 
  print(subset_start) ## stable history ** output 1!

  ## 2. model the history period
  stableHistory <- window(tshistory, start = subset_start)
  lstablehist <- length(stableHistory)/frequency(stableHistory)
  print(lstablehist) ## ** output 2: amount of years within the stable history
  test_tspp <- tspp(stableHistory, order = order)
  test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,  type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
  test_lm <- lm(response ~ trend + harmon, data = test_tspp)

  ## 3. monitor for change in the monitoring period
  test_tspp <- tspp(window(tsdata, start = subset_start),  order = order)
  test_mon <- monitor(test_mefp)
  if (is.na(test_mon$breakpoint)) { tbp <- NA} else { tbp <- test_tspp$time[test_mon$breakpoint]}

  ## 4. plot 
  plot(tsdata,type='n', main = if (title) {
    if (!is.na(tbp[1])) { 
        paste("Time of detected break is", format(tbp,digits=6))} else
        { "no breakpoint detected"}
    }, ylab = 'NDVI'
  )
  lines(tshistory) # history period
  lines(stableHistory,col='green',type="p",pch=19,cex=1)
  abline(v = tbp, lty = 2, col='red', lwd=2) # time of the breakpoint detected by the monitoring process!
  lines(window(tsdata,start=startmonitor),col='red',type='l',pch=19, cex=0.5)
  test_pred <- predict(test_lm, newdata = test_tspp)
  test_pred <- as.ts(zoo(test_pred,test_tspp$time))
  lines(test_pred, col = 4,lwd=1.5)
  
  legend("bottomleft",
  c("History","Stable History","Monitoring","fit based on stable history", "Time of Detected Break")
    ,lty=c(1,NA,1,1,2),col=c(1,'green','red','blue','red'),pch=c(NA,19,NA,NA,NA))  
  return(c(subset_start,lstablehist,tbp))  
}
    
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

########## Set parameters for simulation
createts <- function(datats) {
  return(ts(datats, f=23, s=c(2000,4)))
}

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