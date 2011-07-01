## this script can be run via
# Rscript monitor-simulation-study.R 1 100
# args <- commandArgs(TRUE)
# print(args)
# tellerstart <- args[1]
# tellerstop <- args[2]
###########
## Simulate Time series
## Access accuracy of stable history and monitor assessment
## Written by Achim Zeileis and Jan Verbesselt
###########
# library(foreach)
##  parallell processing in R
# library(doMC)
# registerDoMC(2) # specify two cores
# getDoParWorkers() # double checks the number of cores!!!
# This does not improve the speed of the simulations

# library("bfast")
#require(monash) # package for saving plots for publication
require(strucchange)
source("ts_sim_seas_6x00.R")
order <- 3

#######################################
## Simulating time series            ##
#######################################

## Objective: test break detection accuracy in the monitoring period
##  1) What is the influence of noise/amplitude (signal to noise ratio!) on the time series?
##  2) What with different change types (e.g. clouds versus breaks) could different tests be used e.g. MOSUM versus CUSUM?
##  3) breaks in the history period need to be taken into account but 
##   the history period needs to be of a minimum length
## this has to be of length > 1.5 year
##  4) how close can the breakpoint be to the end of the history period
## 5) assess breaks in this history period - so that we can test the roc function -
## --> although this is not important as we want to detect the change as accurate as possible and not the 
## accuracy of the roc() function - (although this still needs to work of course)
## 6) verify whether the breakpoint is detected to early or not.
## JV* ADD CLOUDS TO THE MONITORING PERIOD (2007) INSTEAD OF AN ABRUPT CHANGE
##   Extra: how much data points do we need in the monitoring period to be able to detect a break?
##   (rephrase this question) how much data is needed before a break can be detected in the monitoring period

nrobs <- 159
sdnoise <- 0.01
dip <- -0.3; # vary size of dip between 0 and 0.15 in steps of 0.01
noisef <- 6 # vary noise factor by 1 - 3 by steps of 0.05
a <- 0.2
dfend <- 12  # vary between 0 and 22 data points from the end
# for(i in 1:100) {
set.seed(48) # remove this - to access randomness.
# print(i)
teller <- 1

# for (teller in tellerstart:tellerstop) {
writefirst <- TRUE

#   for (a in c(0.1,0.3,0.5)) {  
#    for (noisef in round(seq(0,8,by=0.5),2) ) {  
#      for (dip in round(-c(0.5,0.4,0.3,0.2,0.1,0),3) ) {
###       for (dfend in 1:22) { # this determine the distance of the added break from the end of the time series
          onoise <- rnorm(nrobs, mean=0, sd=sdnoise)
          noise <- onoise*noisef        # multiply the noise with a factor
          iclouds <- as.integer(runif(5,min=0,max=nrobs))      
          noise[iclouds] <- -0.1     # with VI specific noise
          ts.sim.noise  <- createts(noise)
          n.range <- sum(abs(range(ts.sim.noise)))  # determine the noise range
          #a/n.range # signal to noise range = amplitude versus noise
          
          tsp(ts.sim.noise)
          
          sim <- simulatets(nrobs = nrobs, a = a, adelta = 0, c1 = 5, c1delta = 0, c2 = 5,
          		dip = dip,
          		ts.sim.noise,
          		averageNDVI = 0.7,
          		t2 = nrobs-dfend)
          
          simul <- cbind(seasonal = sim$ts.sim.s, trend = sim$ts.sim.a+sim$ts.sim.t, noise = sim$ts.sim.n)
          simul <- list(time.series = createts(simul))
          class(simul) <- "stl"
        require(monash)
        saveeps("../papers/figs/Sim_Monitoringsetup",height=14)
          plot(simul)  
#           abline(v=time(ts.sim.noise)[nrobs-dfend],col='red',lty=2)
#           abline(v=2006,col='red',lty=2)
        dev.off()
          ## Determine how much data we have for the monitoring and for detecting a break
          (start <- cycle(ts.sim.noise)[nrobs-dfend]) # position of the simulated break point
          time(ts.sim.noise)[nrobs-dfend]
          startmonitor <- start+1
          startmonitor <- start+5 
#            for (startmonitor in start:(start+5)) { # we start the for loop for the time we just started the break
           (nrdatamonitor <- startmonitor-(start-1)) # nr of datapoints in the monitoring period           
           ftsNDVI <- window(sim$ts.sim.d,end=c(2006,startmonitor)) # full time series
            
            ## determine stable history
#             tshistory <- window(ftsNDVI,end=c(2005,23))
            tshistory <- window(ftsNDVI,end=c(2006,start-1))
            ## determine the signal to noise ratio of the History period
            stlfit <- stl(tshistory, s.window="periodic", robust=TRUE)
            signal <- diff(range(stlfit$time.series[,"trend"]+stlfit$time.series[,"seasonal"]))
            noise <- diff(range(stlfit$time.series[,"remainder"]))
            sn <- signal/noise
  
            subset_start <- roc(tshistory, order = 3, level = 0.05) # searching for a stable period 
            print(subset_start)
          
            stableHistory <- window(tshistory, start = subset_start)
            print(length(stableHistory)/frequency(stableHistory)) # 1.4 year available as a stable model
            
#             title <- FALSE
#             plot(ftsNDVI,type='n', ylab='NDVI', 
#               main= if(title) {paste("nr of data points in the monitoring period = ",nrdatamonitor,sep="")}) 
#             abline(v=time(ftsNDVI)[nrobs-dfend],col='red',lty=2)
# #             tshistory <- window(ftsNDVI, end = c(2006,start-1))
#             lines(tshistory)
#             lines(stableHistory,col='blue',type='p',pch=19, cex=0.6)
# #             lines(window(ftsNDVI,start=c(2006,start)),col='red',type='p',pch=19, cex=0.5)
#             legend("bottomleft",c("History","Stable History","Monitoring"),
#               lty=c(1,NA,NA),pch=c(NA,19,19), col=c(1,'blue','red'))

            test_tspp <- tspp(stableHistory, order = 3)
            ## History model
            test_lm <- lm(response ~ trend + harmon, data = test_tspp)
            test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
            		type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
            ## monitor
            # check
            print(length(window(ftsNDVI, start = subset_start))-length(stableHistory)) # nr of observations after the change
            
            test_tspp <- tspp(window(ftsNDVI, start = subset_start),  order = 3)
            test_mon <- monitor(test_mefp)
            #plot(test_mon, functional = NULL)
            
            if (is.na(test_mon$breakpoint)) { tbp <- NA} else {
                tbp <- time(test_tspp$response)[test_mon$breakpoint]
              }
          
            ## plot and visualise
#             saveeps("../papers/figs/Sim_Monitoring",height=14)
              title = FALSE
              plot(ftsNDVI,type='n', main = if (title) {
                if (!is.na(tbp[1])) { 
                    paste("Time of detected break is", format(tbp,digits=6))} else
                    { "no breakpoint detected"}
                }, ylab = 'NDVI'
              )
              
               lines(tshistory) # history period
              lines(window(ftsNDVI,start=c(2006,1)),lty=2) # monitoring period
              lines(stableHistory,col='blue',type="p",pch=19,cex=0.6)
              test_pred <- predict(test_lm, newdata = test_tspp)
              tsp(test_pred) <- tsp(test_tspp$response)
              lines(as.ts(test_pred), col = 4)
#               abline(v = tbp, lty = 2, col='red', lwd=2) # time of the breakpoint detected by the monitoring process!
#               abline(v=time(sim$ts.sim.d)[nrobs-dfend],col='blue',lty=2,lwd=2) ## simulated breakpoint 
              lines(window(ftsNDVI,start=c(2006,start)),col='red',type='p',pch=3, cex=0.7)
              
              legend("bottomleft",
              c("History period","Monitoring period","n","Stable history","Stable history model")
                ,lty=c(1,2,NA,NA,1),col=c(1,1,'red','blue','blue'),pch=c(NA,NA,3,19,NA))
#             dev.off()

#               legend("bottomleft",
#               c("History","Stable History","Monitoring","fit based on stable history",
#               "Time of Simulated Break", "Time of Detected Break")
#                 ,lty=c(1,NA,NA,1,2,2),col=c(1,'blue','red','blue','blue','red'),pch=c(NA,19,19,NA,NA,NA))
# #             #dev.off()   
#             ## output for accuracy assessment
#            ## simulation setting that are important to be saved to an output file
           
          out <- data.frame(dip, noisef, nrange = round(n.range,digit=4),
            a, dfend, Lhistory = length(tshistory), LStablehistory = length(stableHistory),
            nrdatamonitor, Tsim = nrobs-dfend, Tmon = test_mon$breakpoint, 
              Dsim = time(sim$ts.sim.d)[nrobs-dfend], Dmon = tbp, sn)
            
#           fname <- paste("output1/outputsim_",teller,".csv",sep="")
#           if (writefirst) {
#                 write.table(out,fname, append=FALSE, sep=",", col.names= TRUE, row.names=FALSE)
#                 writefirst <- FALSE
#           } else write.table(out,fname, append=TRUE, sep=",", col.names= FALSE, row.names=FALSE)
#    
# browser()
#      } # amount of data in the monitoring period
#     } # dip
#   } # noisef
# }  # a 
# } # different iterations of the whole simulation set-up : I will do 500 to start with          
# }

# # vary size of dip between 0 and 0.15 in steps of 0.01
# noisef # vary noise factor by 1 - 3 by steps of 0.05
# n.range
# a
# a/n.range # signal to noise range = amplitude versus noise
# 
# ## it is al about detection of the breakpoint yes or no
# # time differnence
# time(sim$ts.sim.d)[nrobs-dfend] # time of simulated break
# tbp # time of detected break