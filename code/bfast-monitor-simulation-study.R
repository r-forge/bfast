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
dip <- -0.2;
noisef <- 3

for (a in c(0.1,0.3,0.5)) {  
for (noisef in round(seq(1,6,by=1),2) ) {  
#		for (adelta in c(0,0.1,0.2,0.3)) {    
#       for (dip in round(-c(0.3,0.2,0.1,0),3) ) {
#			for (c1delta in c(0,10,20,30) ) {  
#
set.seed(1234)
onoise <- rnorm(nrobs, mean=0, sd=sdnoise)
noise <- onoise*noisef        # multiply the noise with a factor
iclouds <- as.integer(runif(5,min=0,max=nrobs))      
noise[iclouds] <- -0.1     # with VI specific noise
ts.sim.noise  <- createts(noise)
#n.range <- sum(abs(range(ts.sim.noise)))
plot(ts.sim.noise)
setwd('/Users/janvb/Documents/R/bfast/code')							
source("ts_sim_seas_6x00.R") 

sim <- simulatets(nrobs = nrobs, a = 0.2, adelta = 0, c1 = 5, c1delta = 0, c2 = 5,
		dip,
		ts.sim.noise,
		averageNDVI = 0.7,
		t2 = 204)

simul <- cbind(seasonal = sim$ts.sim.s, abrupt = sim$ts.sim.a+sim$ts.sim.t, remainder = sim$ts.sim.n)
simul <- list(time.series = createts(simul))
class(simul) <- "stl"
plot(simul)

#######################################
## Set variables                     ##
#######################################

(h <- 10/length(harvest)) 

## ratio of distance between breaks (time steps) 
## and length of the time series 



########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################

## 
## AUTOMATICALLY DEFINE THE START DATE USING THE ROC FUNCTION
##

plot(sim$ts.sim.d)
browser()
subset_start <- roc(window(sim$ts.sim.d, end = c(2008, 1))) # searching for a stable period 
subset_start # not a very long stable period is identified.
# This period maybe should be of a minimum length no?
# when applyin this in an operation context this might be a bottle neck
#
subsetSIM <- window(sim$ts.sim.d, start = subset_start, end = c(2008, 1))
print(length(subsetSIM)/23) # 1.4 year available as a stable model
test_tspp <- tspp(subsetSIM, order = 3)

}
}
## testing lenght of stable period for different amplitudes, and noise levels
## the noise level has not immediate influence
## amplitude

test_lm <- lm(response ~ trend + harmon, data = test_tspp)

## History model
test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
		type = "OLS-MOSUM", h = 0.25, alpha = 0.05)

#JV explain the h in the monitor function

## monitor
test_tspp <- tspp(window(sim$ts.sim.d, start = subset_start),  order = 3)
test_mon <- monitor(test_mefp)
plot(test_mon, functional = NULL)
tbp <- time(test_tspp$response)[test_mon$breakpoint]

## plot and visualise
plot(test_tspp$response,main = paste("Time of detected break is", format(tbp,digits=6)))

test_pred <- predict(test_lm, newdata = test_tspp)
tsp(test_pred) <- tsp(test_tspp$response)
lines(as.ts(test_pred), col = 4)
abline(v = tbp, lty = 2, col='red') # time of the breakpoint detected by the monitoring process!


