###########
## Tools ##
###########
# Written by Achim Zeileis and Jan Verbesselt

## package and data
library("bfast")

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

#######################################
## Set variables                     ##
#######################################
(hshort <- 10/length(harvest)) # ratio of distance between breaks (time steps) and length of the time series 
order <- 3

#######################################
## Simulating time series            ##
#######################################
# Set parameters for simulation
createts <- function(datats) {
	return(ts(datats, f=23, s=c(2000,4)))
}

## TO DO: set up simulation analysis and save results of determining the time of change
## --> What is the influence of noise on the time series?
## --> Especially the noise robustness and importance of other structural change in 
## the history time period will be important.

## TEST IF LENGHT OF STABLE PERIOD CHANGES WHEN RANDOM NOISE LEVEL INCREASES

nrobs <- 208
sdnoise <- 0.01
dip <- -0.3;
noisef <- 3
a <- 0.1
dfend <- 1
set.seed(1234) # we should remove this - to access randomness.

# for (a in c(0.1,0.3,0.5)) {  
#   for (noisef in round(seq(1,6,by=1),2) ) {  
# #	   for (adelta in c(0,0.1,0.2,0.3)) {    
#     for (dip in round(-c(0.3,0.2,0.1,0),3) ) {
# #		 for (c1delta in c(0,10,20,30) ) {  
#         for (dfend in 1:8) {

onoise <- rnorm(nrobs, mean=0, sd=sdnoise)
noise <- onoise*noisef        # multiply the noise with a factor
iclouds <- as.integer(runif(5,min=0,max=nrobs))      
noise[iclouds] <- -0.1     # with VI specific noise
ts.sim.noise  <- createts(noise)
#n.range <- sum(abs(range(ts.sim.noise)))
plot(ts.sim.noise)
setwd('/Users/janvb/Documents/R/bfast/code')							
source("ts_sim_seas_6x00.R") 

sim <- simulatets(nrobs = nrobs, a = a, adelta = 0, c1 = 5, c1delta = 0, c2 = 5,
		dip = dip,
		ts.sim.noise,
		averageNDVI = 0.7,
		t2 = nrobs-dfend)

simul <- cbind(seasonal = sim$ts.sim.s, abrupt = sim$ts.sim.a+sim$ts.sim.t, remainder = sim$ts.sim.n)
simul <- list(time.series = createts(simul))
class(simul) <- "stl"
plot(simul)

########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################
## AUTOMATICALLY DEFINE THE START DATE USING THE ROC FUNCTION

## 1 DETERMINE HOW MUCH DATA IS NEEDED BEFORE THE BREAKPOINT
## 2 DETERMINE A STABLE PERIOD IN THIS HISTORY PERIOD
## we will therefore visualise everything to understand beter what is going on.

plot(window(sim$ts.sim.d,start=2008)) # visualise just a short section of the full time series
lines(sim$ts.sim.d,type='p',pch=20,cex=0.5)
end(sim$ts.sim.d) # einde echt tijd serie
#abline(v=time(sim$ts.sim.d)[nrobs],col='blue',lty=2)

cycle(sim$ts.sim.d)[nrobs-dfend] # position of the simulated break point
time(sim$ts.sim.d)[nrobs-dfend] 
abline(v=time(sim$ts.sim.d)[nrobs-dfend],col='red',lty=2)

# A? It would be great if you could test this script for different  k's
# A: starts of the history period e.g. 3 until 10
# A: history period can per definition go until one time step before the break
k <- 2
thistory <- nrobs-dfend-k
thistyr <- floor(time(sim$ts.sim.d)[thistory])
thistdec <- cycle(sim$ts.sim.d)[thistory]
end = c(thistyr,thistdec) # determines untill where we have data = history period
abline(v=time(sim$ts.sim.d)[thistory],col='blue',lty=2)
tshistory <- window(sim$ts.sim.d, end = end)
lines(tshistory,col='blue',lwd=2)

# per definiation this could be untill close before that the breakpoint occurs.
subset_start <- roc(tshistory) # searching for a stable period 
subset_start # not a long stable period is identified
# the lenght differs a lot depending on where the end of the time series is defined
# A? This is something that needs to be tested
# This period maybe should be of a minimum length no?
# when applyin this in an operation context this might be a bottle neck - I will test this on real data


stableHistory <- window(sim$ts.sim.d, start = subset_start, end = end)
print(length(stableHistory)/23) # 1.4 year available as a stable model

plot(window(sim$ts.sim.d)) # visualise just a short section of the full time series
lines(sim$ts.sim.d,type='p',pch=20,cex=0.5)
abline(v=time(sim$ts.sim.d)[nrobs-dfend],col='red',lty=2)
abline(v=time(sim$ts.sim.d)[thistory],col='blue',lty=2)
lines(tshistory,col='blue',lwd=2)
lines(stableHistory,col='green',lwd=2)

test_tspp <- tspp(stableHistory, order = 3)
# }
# }


## History model
test_lm <- lm(response ~ trend + harmon, data = test_tspp)
test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
		type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
## monitor
test_tspp <- tspp(window(sim$ts.sim.d, start = subset_start),  order = 3)
test_mon <- monitor(test_mefp)
plot(test_mon, functional = NULL)
tbp <- time(test_tspp$response)[test_mon$breakpoint]

## plot and visualise
plot(test_tspp$response,main = 
  if (!is.na(tbp[1])) { 
      paste("Time of detected break is", format(tbp,digits=6))} else
      { "no breakpoint detected"})
lines(test_tspp$response,type='p', pch=19, cex=0.5)
test_pred <- predict(test_lm, newdata = test_tspp)
tsp(test_pred) <- tsp(test_tspp$response)
lines(as.ts(test_pred), col = 4)
abline(v = tbp, lty = 2, col='green') # time of the breakpoint detected by the monitoring process!
abline(v = time(sim$ts.sim.d)[thistory], lty = 3, col='blue') # end of history period
abline(v=time(sim$ts.sim.d)[nrobs-dfend],col='red',lty=2) ## simulated breakpoint 
