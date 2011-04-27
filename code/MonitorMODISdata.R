#################
## Concept: Analyse MODIS data for changes occuring in 2006 using the stableHistory and Monitoring Methodology
## Objective
## 1) Verify how long the stable period needs to be for a accurate fit and then change detection 
## ?(could we set a threshold on the fit? R2 or RMSE required for a good change detection)
## 2) Detect near real time changes just after the monitoring period
## ? could we use BFAST to compare with real changes detected in this section
## setwd('/Users/janvb/Documents/R/bfast/code')

require(bfast)
require(zoo)
## load functions # roc / tspp / time series simulation / sos
   					
source("ts_sim_seas_6x00.R")

## read in Satellite data time series

data <- read.csv("mtsNDVI.csv") 
names(data) <- as.character(0:120)

## change the number here ## which corresponds to the plot number
output <- data.frame(plots=1:120,percNA=NA,signaltonoise=NA,
  Lhistory=NA,historylmfit.adjr2=NA,timebp=NA)

i <- 117
# for (i in 1:120) {

  tsNDVI <- ts(data[,as.character(i)],start=c(2000,4),frequency=23)
  plot(tsNDVI)
  
  ## determine the percentage of NA's within a time series
  output$percNA[i] <- length(which(is.na(tsNDVI)))/length(tsNDVI)
  
  ## fill gaps #### check for amount of NA's
  ftsNDVI <-ts(na.spline(tsNDVI)) # bicubic interpolation 
  ## we have to be carefull here as new/ maybe not realistic data is created here
  tsp(ftsNDVI) <- tsp(tsNDVI)
  ## illustrates the data filling procedure
  plot(ftsNDVI,col='red')
  lines(tsNDVI,lwd=2)
  
  ## select data window until 2008
  ## to limit the data amount and avoid spline interpolation errors at the end of a time series
  ftsNDVI <- window(ftsNDVI,end=c(2007,1))
  plot(ftsNDVI, main="History period - maybe not stable")
  
  ## Determine the signal to noise ratio using the range of the stl components
  stlfit <- stl(ftsNDVI, s.window="periodic", robust=TRUE)
#   plot(stlfit)
#   plot(ftsNDVI)
#  lines(stlfit$time.series[,"trend"]+stlfit$time.series[,"seasonal"],col='red')
  
  signal <- diff(range(stlfit$time.series[,"trend"]+stlfit$time.series[,"seasonal"]))
  noise <- diff(range(stlfit$time.series[,"remainder"]))
  output$signaltonoise[i] <- signal/noise
  
  ## identify history period
  NDVIhistory <- window(ftsNDVI,end=c(2006,1))
  lines(NDVIhistory,col='blue') # history period
  
  ## verify the stability of the history period
  subset_start <- roc(NDVIhistory) # searching for a stable period 
  subset_start # not a long stable period is identifie
  
  ## subset stable section within the history part
  stableHistory <- window(NDVIhistory, start = subset_start)
  print(length(stableHistory)/frequency(ftsNDVI)) 
  output$Lhistory[i] <- length(stableHistory)/frequency(ftsNDVI) # write length (nr of years)
  
  plot(NDVIhistory) # visualise just a short section of the full time series
  lines(NDVIhistory,type='p',pch=20,cex=0.5)
  lines(stableHistory,col='blue',lwd=2)
  
  ## create a tspp object - trend and harmonic seasonal model
  order <- 3
  test_tspp <- tspp(stableHistory, order = order)
  
  ## History model
  test_lm <- lm(response ~ trend + harmon, data = test_tspp) 
  output$historylmfit.adjr2[i] <- summary(test_lm)$adj.r.squared
  test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
    	type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
  
  ## monitor
  test_tspp <- tspp(window(ftsNDVI, start = subset_start),  order = order)
  test_mon <- monitor(test_mefp)
  plot(test_mon, functional = NULL)
  if (is.na(test_mon$breakpoint)) { tbp <- NA} else {
    tbp <- time(test_tspp$response)[test_mon$breakpoint]
  }
  
  # ## COMPARE WITH BFAST
#   require(bfast)
#   h <- (1.5*23)/length(ftsNDVI)
#   fit <- bfast(ftsNDVI, h=h, season = c("harmonic"), max.iter = 1)
#   plot(fit)
#   # output
#   niter <- length(fit$output) # nr of iterations
#   out <- fit$output[[niter]]  
  
  
  ## plot and visualise
  plot(ftsNDVI,main = 
    if (!is.na(tbp[1])) { 
        paste("Time of detected break is", format(tbp,digits=6))} else
        { "no breakpoint detected"})
  lines(test_tspp$response,type='p', pch=19, cex=0.5)
  test_pred <- predict(test_lm, newdata = test_tspp)
  tsp(test_pred) <- tsp(test_tspp$response)
  lines(as.ts(test_pred), col = 'blue')
  lines(stableHistory,col='blue',lwd=2,type="p",pch=19,cex=0.5)
  abline(v = tbp, lty = 2, col='green') # time of the breakpoint detected by the monitoring process!
#   lines(out$Tt,col='purple',lty=3) 
  
  ## output
  output$timebp[i] <- tbp
#  }

#write.csv(output,"output.csv")
#fix(output)
# A? BFAST trend output - could this be used as a validation method?

########
## Final Comments: for the paper writing discussion section
########

## the great thing about the monitoring approach is that is Interpolation is not necessary!!!
## the users just need to define a period in time where they want to use this function to detect changes

## by applying this scrip on real data you can see that a short history period 
## does not enable a good fit of the data

## Assess the length of the history period and the quality of the fit
## a break is detected in 2006 which corresponds to a dry period in the plantation forest.
# for the manuscript plot number 2 is a great illustration of the principle!

############
##### end of script

