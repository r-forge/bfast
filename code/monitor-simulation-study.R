###########
## Simulate Time series
## Access accuracy of stable history and monitor assessment
## Written by Achim Zeileis and Jan Verbesselt
###########

## package and data
library("bfast")

## load functions # ROC / tspp / time series simulation / sos
setwd('/Users/janvb/Documents/R/bfast/code')    					
source("ts_sim_seas_6x00.R")

#######################################
## Set variables                     ##
#######################################
#(hshort <- 10/length(harvest)) # ratio of distance between breaks (time steps) and length of the time series 
order <- 3

#######################################
## Simulating time series            ##
#######################################

## Objective: test break detection accuracy in the monitoring period
##  1) What is the influence of noise/amplitude (signal to noise ratio!) on the time series?
##  2) What with different change types (e.g. clouds versus breaks) could different tests be used e.g. MOSUM versus CUSUM?
##  3) breaks in the history period need to be taken into account but 
##   the history period needs to be of a minimum length
##  4) how close can the breakpoint be to the end of the history period
## 5) assess breaks in this history period - so that we can test the roc function -
## --> although this is not important as we want to detect the change as accurate as possible and not the 
## accuracy of the roc() function - (although this still needs to work of course)
## 6) verify whether the breakpoint is detected to early or not.

##   Extra: how much data points do we need in the monitoring period to be able to detect a break?
##   (rephrase this question) how much data is needed before a break can be detected in the monitoring period

nrobs <- 159
sdnoise <- 0.01
dip <- -0.1; # vary size of dip between 0 and 0.15 in steps of 0.01
noisef <- 3 # vary noise factor by 1 - 3 by steps of 0.05
a <- 0.1
dfend <- 4  # vary between 0 and 22 data points from the end
set.seed(1234) # remove this - to access randomness.

# for (a in c(0.1,0.3,0.5)) {  
#   for (noisef in round(seq(1,6,by=1),2) ) {  
##	   for (adelta in c(0,0.1,0.2,0.3)) {   # specific for seasonal change detection 
#     for (dip in round(-c(0.3,0.2,0.1,0),3) ) {
##		 for (c1delta in c(0,10,20,30) ) {  # specific for seasonal change detection
#         for (dfend in 1:8) {

onoise <- rnorm(nrobs, mean=0, sd=sdnoise)
noise <- onoise*noisef        # multiply the noise with a factor
iclouds <- as.integer(runif(5,min=0,max=nrobs))      
noise[iclouds] <- -0.1     # with VI specific noise
ts.sim.noise  <- createts(noise)
n.range <- sum(abs(range(ts.sim.noise)))  # determine the noise range

a/n.range # signal to noise range = amplitude versus noise

plot(ts.sim.noise)

##JV ADD CLOUDS TO THE MONITORING PERIOD (2007) INSTEAD OF AN ABRUPT CHANGE

sim <- simulatets(nrobs = nrobs, a = a, adelta = 0, c1 = 5, c1delta = 0, c2 = 5,
		dip = dip,
		ts.sim.noise,
		averageNDVI = 0.7,
		t2 = nrobs-dfend)

simul <- cbind(seasonal = sim$ts.sim.s, abrupt = sim$ts.sim.a+sim$ts.sim.t, remainder = sim$ts.sim.n)
simul <- list(time.series = createts(simul))
class(simul) <- "stl"
plot(simul)

## Visualise the full time seres and the breaks
ftsNDVI <- window(sim$ts.sim.d,end=c(2007,1))

plot(ftsNDVI) 
lines(ftsNDVI,type='p',pch=20,cex=0.5)


cycle(ftsNDVI)[nrobs-dfend] # position of the simulated break point
time(ftsNDVI)[nrobs-dfend] 
abline(v=time(ftsNDVI)[nrobs-dfend],col='red',lty=2)


# thistory <- nrobs-dfend-k
# thistyr <- floor(time(sim$ts.sim.d)[thistory])
# thistdec <- cycle(sim$ts.sim.d)[thistory]
# end = c(thistyr,thistdec) # determines untill where we have data = history period
# abline(v=time(sim$ts.sim.d)[thistory],col='blue',lty=2)

tshistory <- window(ftsNDVI, end = c(2006,1))
lines(tshistory,col='green',lwd=2)

#write.csv(tshistory,"tshistory.csv")
## could bfast be used instead of the roc() function
## to verify the stability of the history period?
test_tspp <- tspp(tshistory, order = 3)
print(breakpoints(response ~ trend + harmon, data = test_tspp))
## no breakpoints - are detected within the history period
## so why should we use the roc() function?


subset_start <- roc(tshistory) # searching for a stable period 
print(subset_start)


stableHistory <- window(tshistory, start = subset_start)
print(length(stableHistory)/23) # 1.4 year available as a stable model

plot(ftsNDVI) # visualise the full time series
lines(ftsNDVI,type='p',pch=20,cex=0.5)
abline(v=time(ftsNDVI)[nrobs-dfend],col='red',lty=2)
lines(tshistory,col='green',lwd=2)
lines(stableHistory,col='blue',lwd=2)
# browser()
# }
test_tspp <- tspp(stableHistory, order = 3)
# }
# }


## History model
test_lm <- lm(response ~ trend + harmon, data = test_tspp)
test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
		type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
## monitor
test_tspp <- tspp(window(ftsNDVI, start = subset_start),  order = 3)
test_mon <- monitor(test_mefp)
plot(test_mon, functional = NULL)

if (is.na(test_mon$breakpoint)) { tbp <- NA} else {
    tbp <- time(test_tspp$response)[test_mon$breakpoint]
  }

## plot and visualise
plot(ftsNDVI,main = 
  if (!is.na(tbp[1])) { 
      paste("Time of detected break is", format(tbp,digits=6))} else
      { "no breakpoint detected"})
lines(ftsNDVI,type='p', pch=19, cex=0.5)
lines(stableHistory,type='p', pch=19, cex=0.5,col='blue')
test_pred <- predict(test_lm, newdata = test_tspp)
tsp(test_pred) <- tsp(test_tspp$response)
lines(as.ts(test_pred), col = 4)
abline(v = tbp, lty = 2, col='green') # time of the breakpoint detected by the monitoring process!
abline(v = end(tshistory), lty = 2, lwd= 2, col='blue') # end of history period
abline(v=time(sim$ts.sim.d)[nrobs-dfend],col='red',lty=2) ## simulated breakpoint 

legend("bottomleft", c("Model fit through stable history period", "Stable History period",
"end history period", "Time of Simulated Break", "Time of Detected Break"),
lty=c(1,0,2,2,2), lwd=c(1,NA,2,1,1),col=c("blue","blue","blue","red","green"),pch=c(NA,19,NA,NA,NA))

## output for accuracy assessment
## simulation setting that are important to be saved to an output file

dip # vary size of dip between 0 and 0.15 in steps of 0.01
noisef # vary noise factor by 1 - 3 by steps of 0.05
n.range
a
a/n.range # signal to noise range = amplitude versus noise

## it is al about detection of the breakpoint yes or no
# time differnence
time(sim$ts.sim.d)[nrobs-dfend] # time of simulated break
tbp # time of detected break



