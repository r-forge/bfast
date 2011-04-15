
###########
## Tools ##
###########
# Written by Achim Zeileis and Jan Verbesselt

## package and data
library("bfast")
data("harvest", package = "bfast")

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

#######################################
## Set variables                     ##
#######################################

(hshort <- 10/length(harvest)) # ratio of distance between breaks (time steps) and length of the time series 
order <- 3

#######################################
## Perliminary analysis with bfast   ##
#######################################

fit <- bfast(harvest,h = hshort, season = "harmonic", max.iter = 1,breaks = 3)
## JV is this h the same as below?
plot(fit)

#######################################
## History period: 2000(4)--2004(12) ##
#######################################

## set up data
harvest_tspp <- tspp(window(harvest, end = c(2004, 12)), order = order)
plot(harvest_tspp$response)
## set up monitoring fluctuation process
## for linear model with trend and harmonic season
h <- 0.25
harvest_mefp <- mefp(response ~ trend + harmon, data = harvest_tspp,
  type = "OLS-MOSUM", h = h, alpha = 0.05)
harvest_mefp

## internally, this simply estimates the following
## model (-> compare the coefficients)
harvest_lm <- lm(response ~ trend + harmon, data = harvest_tspp)
coef(harvest_lm)
## visualization
plot(harvest_tspp$response)
harvest_pred <- predict(harvest_lm, newdata = harvest_tspp)
lines(ts(harvest_pred, start = c(2000, 4), freq = 23), col = 4)

#######################################
## Monitoring period: up to 2008(18) ##
#######################################

## all data becomes available -> update harvest_tspp
harvest_tspp <- tspp(harvest, order = order)

## conduct monitoring -> this automatically checks the harvest_tspp variable and recalculates
harvest_mon <- monitor(harvest_mefp)

tbp <- time(harvest)[harvest_mon$breakpoint]

plot(harvest_mon, functional = NULL)
plot(harvest_mon, functional = NULL)
## ?AZ Does this plot show the Partial Moving Sums - of the residuals of the fitted model
## !Z: Yes, it's the process of MOSUMs of OLS residuals, suitably scaled.

# JV the plot shows the significance level for break detection here, and not the Confidence interval for the time of break 
# JV (as explained in Achim's mail) the CI time of break detection is not estimated as this is mainly an online monitoring tool

harvest_mon

## internally this simply computes predictions from the
## history model for the monitoring period (and then a MOSUM
## of their residuals)
plot(harvest_tspp$response)
harvest_pred <- predict(harvest_lm, newdata = harvest_tspp)
lines(ts(harvest_pred, start = c(2000, 4), freq = 23), col = 4)
abline(v = 2004 + 11/23, lty = 2) 
## ?AZ what does this first abline indicate?
## !Z: The end of the history perido. 2004(12) with freq = 23 is
##     2004 + (12 - 1)/23.
abline(v = tbp, lty = 2, col='red') # time of the breakpoint detected by the monitoring process!

##########
## JV A technique to approximate the last section of the time series
## JV it can be used to evaluate the change type of the last section of the time series
##########

fm_last <- lm(response ~ trend + harmon, data = harvest_tspp[-(1:harvest_mon$breakpoint),])
harvest_pred_last <- predict(fm_last, newdata = harvest_tspp[-(1:harvest_mon$breakpoint),])
lines(ts(harvest_pred_last, start = tbp, freq = 23), col = "red", lwd = 2)


##JV This section based on the breakpoint detection is not required anymore
##JV since I will focus on the online monitoring aspect for forest change detection

bp.harvest <- breakpoints(response ~ trend + harmon, data = harvest_tspp, h = 23/length(harvest))
plot(bp.harvest)
breakpoints(bp.harvest)

fm1 <- lm(response ~ (trend + harmon) %in% breakfactor(bp.harvest), data = harvest_tspp)
plot(harvest)
lines(ts(fitted(fm1), start = c(2000,4), freq = 23), col = 4)
lines(bp.harvest)

fm_last <- lm(response ~ trend + harmon, data = harvest_tspp[
				-(1:tail(breakpoints(bp.harvest)$breakpoints, 1)),])
fm_last

# why are the NaN's produced in the time series? # can the fit be improved?
# I need to make sure that no structural changes are occuring in the history period before we start monitoring.


# See explanation in Achim's reply - some incremental test would be great.

#######################################
## Simulating time series            ##
#######################################
# Set parameters for simulation
createts <- function(datats) {
	return(ts(datats, f=23, s=c(2000,4)))
}

nrobs <- 208
sdnoise <- 0.01
dip <- -0.2;
noisef <- 3

#for (a in c(0.1,0.3,0.5)) {  
#	for (noisef in round(seq(1,6,by=1),2) ) {  
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

(h <- 10/length(harvest)) # ratio of distance between breaks (time steps) and length of the time series 

#######################################
## Perliminary analysis with bfast   ##
#######################################

fit <- bfast(sim$ts.sim.d,h = h, season = "harmonic", max.iter = 1)
plot(fit)
fit 
## bfast estimates the position of the breakpoint correctly and the result can be used to assess the type of change
## break is detected too late and not identified correctly

## output
niter <- length(fit$output) # nr of iterations
out <- fit$output[[niter]]  # output of results of the final fitted seasonal and trend models and nr of breakpoints in both
out$Vt.bp

#######################################
## The Monitor approach			     ##
#######################################

## MANUALLY SET UP HISTORY PERIOD
subset <- window(sim$ts.sim.d, start = c(2006,1), end = c(2008, 1))
plot(subset)
test_tspp <- tspp(subset,order = order)

## set up monitoring fluctuation process
## for linear model with trend and harmonic season
test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
		type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
test_mefp

## internally, this simply estimates the following
## model (-> compare the coefficients)
test_lm <- lm(response ~ trend + harmon, data = test_tspp)
coef(test_lm)

plot(test_tspp$response)
test_pred <- predict(test_lm, newdata = test_tspp)
tsp(test_pred) <- tsp(subset)
lines(as.ts(test_pred), col = 4)

#######################################
## Monitoring period: up to the end of the time series ##
#######################################

## all data becomes available
test_tspp <- tspp(window(sim$ts.sim.d, start = c(2006,1)), order = order)

## conduct monitoring -> this automatically checks the harvest_tspp variable and recalculates
test_mon <- monitor(test_mefp)

tbp <- time(test_tspp$response)[test_mon$breakpoint]

plot(test_mon, functional = NULL)
# JV the plot shows the confidence interval used to detect breakpoints and the detected breakpoint
# JV is it possible to characterize the change (e.g. slope/intercepts once it is detected?)
test_mon

## internally this simply computes predictions from the
## history model for the monitoring period (and then a MOSUM
## of their residuals)
plot(test_tspp$response,main = paste("Time of detected break is", format(tbp,digits=6)))
test_pred <- predict(test_lm, newdata = test_tspp)
tsp(test_pred) <- tsp(test_tspp$response)
lines(as.ts(test_pred), col = 4)
abline(v = tbp, lty = 2, col='red') # time of the breakpoint detected by the monitoring process!

## this section below does not always work because sometimes there is no data left at the end
## of the time series to fit a model
fm_last <- lm(response ~ trend + harmon, data = test_tspp[-(1:test_mon$breakpoint),])
test_pred_last <- predict(fm_last, newdata = test_tspp[-(1:test_mon$breakpoint),])
lines(ts(test_pred_last, start = tbp, freq = 23), col = "red", lwd = 2)
coef(fm_last)

## this illustrates that estimation coefficients of the last segment is not relevant as the
## aim of online monitoring just is to detect whether or not a break is occuring in the last 
## section of the segment
## this method really work on simulated data but I wonder whether it works on real MODIS data
## Especially robustness against noise will be an issue!!!
## The monitoring process is a great tool to be implemented in real time monitoring processes 
## but we need to make
## sure that no structural 
## change is occuring in the historical time series (used to set up the mefp()). 
## Could this be done automatically?

## TO DO: set up simulation analysis and save results of determining the time of change
## --> What is the influence of noise on the time series?
## --> Especially the noise robustness and importance of other structural change in 
## the history time period will be important.

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
  y_rev$response <- ts(y_rev$response, start = -tail(time(y), 1), frequency = frequency(y))
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

## history
harvest_start <- roc(window(harvest, end = c(2004, 12)), order = 3)
## I added something to add a line to the plot indicating when the recursive process crossess 
## the boundary.
## A? I noticed that a lower order (order = 1) process indicates a longer stable period
## then when we choose for an higher order harmonic (order = 3).
## I would expect it to be the other way around
## !Z: No, not necessarily. First of all, a strict point of view would be
##     that the model is then misspecified and hence the results are invalid.
##     But, of course, we could employ a more pragmatic point of view and
##     say that the model does not fit very well. Then the residual variance will be
##     very large and all shifts/breaks/etc. may seem very small compared to
##     the huge variance and hence lead to non- (or at least less) significant results.

## employ Bai & Perron breakpoint estimation instead
## with LWZ criterion
harvest_start2 <- bp(window(harvest, end = c(2004, 12)), order = 3, h = 24)
## or BIC criterion
harvest_start3 <- bp(window(harvest, end = c(2004, 12)), order = 3, h = 24, ic = "bic")

## comparison: LWZ selects approximately the same as ROC, BIC somewhat smaller
harvest_start - harvest_start2
harvest_start - harvest_start3


harvest_tspp  <- tspp(window(harvest, start = harvest_start, end = c(2004, 12)), order = 3)
harvest_mefp <- mefp(response ~ trend + harmon, data = harvest_tspp,
  type = "OLS-MOSUM", h = 0.25, alpha = 0.05)

## monitor
##harvest_tspp <- tspp(harvest, order = 3) 
## ?AZ I corrected this as I think we should keep the start data
## as identified by the ROC method

harvest_tspp <- tspp(window(harvest, start = harvest_start), order = 3)

harvest_mon <- monitor(harvest_mefp)
plot(harvest_mon, functional = NULL)

##########################################################
## Apply this on the simulated data
##########################################################

## 
## AUTOMATICALLY DEFINE THE START DATE USING THE ROC FUNCTION
##
plot(sim$ts.sim.d)
subset_start <- roc(window(sim$ts.sim.d, end = c(2008, 1))) # searching for a stable period 
subset_start # not a very long stable period is identified.
# This period maybe should be of a minimum length no?
# when applyin this in an operation context this might be a bottle neck
#
subsetSIM <- window(sim$ts.sim.d, start = subset_start, end = c(2008, 1))
length(subsetSIM)/23 # 1.4 year available as a stable model
test_tspp <- tspp(subsetSIM, order = 3)

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



