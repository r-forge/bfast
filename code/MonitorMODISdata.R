setwd('/Users/janv/Documents/R/bfast/bfast/code/')
## Saved in bfast/code folder
##
#################
## Concept: Analyse MODIS data for changes occuring in 2006 using the stableHistory and Monitoring Methodology
## Objective
## 1) Verify how long the stable period needs to be for a accurate fit and then change detection 
## ?(could we set a threshold on the fit? R2 or RMSE required for a good change detection)
## 2) Detect near real time changes just after the monitoring period
## ? could we use BFAST to compare with real changes detected in this section

require(bfast)
require(zoo)
require(monash) # package for saving plots for publication
library(RMySQL)
require(zoo)

timeser <- function(index,dt) {
  z <- zoo(index,dt)
  yr <- as.numeric(format(time(z), "%Y"))
	jul <- as.numeric(format(time(z), "%j"))
	delta <- min(unlist(tapply(jul, yr, diff))) # 16
	zz <- aggregate(z, yr + (jul - 1) / delta / 23)
	(tso <- as.ts(zz))
	return(tso)	
}

multi.line.paste <-function (..., sep = "", collapse = NULL) 
{ 
    args <- list(...) 
    if (length(args) == 0) 
        if (length(collapse) == 0) 
            character(0) 
        else "" 
    else { 
        for (i in seq(along = args)) args[[i]] <- 
gsub("\n","",as.character(args[[i]])) 
        .Internal(paste(args, sep, collapse)) 
    } 
}

source("ts_sim_seas_6x00.R")

## read in Satellite data time series
data <- read.csv("mtsNDVI.csv") 
names(data) <- as.character(0:120)

## extra
gras <- read.csv("grass.csv")

## Query NDVI data from the database
## using a Query script
  ## connect to the database
  con <- dbConnect(MySQL(), user="verbe039", password="Jv100977", 
  dbname="Greenhills", host="10.75.1.112")
  dbListTables(con)
  
  gid <-44098
    
  ## query based on gid
  Query <- multi.line.paste("
  SELECT yr,dy,ndvi 
  FROM xymod13q1 AS a 
  LEFT JOIN datamod13q1 AS b ON ( a.gid = b.gid ) 
  WHERE a.gid = ",gid," and b.rel>=0 and b.rel<1 
  group by yr,dy 
  order by yr,dy 
  ") # use only good data! e.g. with reliability = 0
  
  res <- dbSendQuery(con, Query)
  data <- fetch(res, n = -1)
  data[1,]
  
  ## creata time series with the data
  datum <- as.Date(paste(data$yr,data$dy,sep=","),"%Y,%j") # Ayear,julianday
  tsNDVI <- timeser(data$ndvi/10000,datum) 
  plot(tsNDVI)

## change the number here ## which corresponds to the plot number
output <- data.frame(plots=1:120,percNA=NA,signaltonoise=NA,
  Lhistory=NA,historylmfit.adjr2=NA,timebp=NA)


# ## i <- 117  ## voorbeeld met cloud piekin the history period.
# # i <- 4  # tree mortality
#   # i <- 8  # harvest event
# #   i <- 35 # is also a harvest activity ook 36
# #  i <- 77 # harvest event!
# #   i <- 43 # regrowth effect that where a change is detected that is not really a change
# # i <- 8  # latlong 147.99E and -35.43N 
# i <- 120 # latlong 148.035E and -35.58N
# i <- 5 # latlong 
# # for (i in 1:120) {
# 
# str(gras)
#   tsNDVI <- ts(data[,as.character(i)],start=c(2000,4),frequency=23)
#   tsNDVI<- ts(gras$x,start=c(2000,4),frequency=23)
# plot(tsNDVI)
  
  ## determine the percentage of NA's within a time series
  output$percNA[i] <- length(which(is.na(tsNDVI)))/length(tsNDVI)

## Witout gap filling
ftsNDVI <- tsNDVI
## With gap filling
# ftsNDVI <-ts(na.spline(tsNDVI)) # bicubic interpolation

tsp(ftsNDVI) <- tsp(tsNDVI)
#   ftsNDVI <-tsNDVI
 
  ## illustrates the data filling procedure
#   plot(ftsNDVI,col='red')
#   lines(tsNDVI,lwd=2)
  
  ## select data window until 2007 so that we have one year for change detection
  ## to limit the data amount and avoid spline interpolation errors at the end of a time series
#  savepng('Timeseriessetup')
  ftsNDVI <- window(ftsNDVI,end=c(2007,1))

## Determine the signal to noise ratio using the range of the stl components
  stlfit <- stl(ftsNDVI, s.window="periodic", robust=TRUE, na.action=na.approx)
  plot(stlfit)
#   plot(ftsNDVI)
#  lines(stlfit$time.series[,"trend"]+stlfit$time.series[,"seasonal"],col='red')
  
  signal <- diff(range(stlfit$time.series[,"trend"]+stlfit$time.series[,"seasonal"]))
  noise <- diff(range(stlfit$time.series[,"remainder"]))
  print(noise)
#   plot(stlfit$time.series[,"remainder"])  
output$signaltonoise[i] <- signal/noise
  
  ## identify history period
  NDVIhistory <- window(ftsNDVI,end=c(2006,1))

# #  savepng("figs/Monitoringsetup")
#   plot(ftsNDVI,ylab='NDVI',type='n')
#   lines(NDVIhistory) # history period
#   lines(window(ftsNDVI,start=c(2006,1)),ylab='NDVI',lty=2) # monitoring period
#   legend("bottomleft",c("History","Monitoring"),lty=c(1,2))
# #  dev.off()
  
  ## verify the stability of the history period
  subset_start <- roc(NDVIhistory) # searching for a stable period 
  print(subset_start) # not a long stable period is identifie
  
  ## subset stable section within the history part
  stableHistory <- window(NDVIhistory, start = subset_start)
  print(length(stableHistory)/frequency(ftsNDVI)) 
  output$Lhistory[i] <- length(stableHistory)/frequency(ftsNDVI) # write length (nr of years)
  
# # savepng("figs/StableHistory")
#   plot(ftsNDVI,ylab='NDVI',type='n')
#   lines(NDVIhistory) # history period
#   lines(window(ftsNDVI,start=c(2006,2)),ylab='NDVI',lty=2) # monitoring period
#   lines(stableHistory,col='blue',lwd=2)
#   legend("bottomleft",c("History","Monitoring","Stable History"),lty=c(1,2,1),col=c(1,1,'blue'))
# #  dev.off()
    
  ## create a tspp object - trend and harmonic seasonal model
  order <- 3
  test_tspp <- tspp(stableHistory, order = order)
  
  ## History model
  test_lm <- lm(response ~ trend + harmon, data = test_tspp) #, na.action=na.omit

#       plot(test_tspp$response)
      test_pred <- predict(test_lm)
      tsp(test_pred) <- tsp(test_tspp$response)
#       lines(as.ts(test_pred),col=2)
#       plot(test_tspp$response-test_pred)
      output$stablenoise <- diff(range(test_tspp$response-test_pred))  
      print(output$stablenoise[i])
#       diff(range(residuals(test_lm)))
      output$stablesigmanoise[i] <-summary(test_lm)$sigma

output$historylmfit.adjr2[i] <- summary(test_lm)$adj.r.squared
  test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,
    	type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
  
  
  ## monitor
  test_tspp <- tspp(window(ftsNDVI, start = subset_start),  order = order)
  lengthNAs <- length(which(is.na(ftsNDVI)))
  print(lengthNAs)
  test_mon <- monitor(test_mefp)
#   plot(test_mon, functional = NULL)
  if (is.na(test_mon$breakpoint)) { tbp <- NA} else {
#     tbp <- time(test_tspp$response)[test_mon$breakpoint]
    tbp <- test_tspp$time[test_mon$breakpoint]
  }
names(test_tspp)  


setwd('/Users/janv/Documents/R/bfast/bfast/papers/figs')
saveeps(paste("shorthistoryperiod",sep=""), height=14)
  title <- FALSE
  plot(ftsNDVI,type='n', main = if (title) {
      if (!is.na(tbp[1])) { 
          paste("Plot nr",i," Time of detected break is", format(tbp,digits=6))} else
          {paste("Plot nr",i,"no breakpoint detected",sep="")}
      }, ylab='NDVI'
    )
  lines(NDVIhistory) # history period
  lines(window(ftsNDVI,start=c(2006,1)),ylab='NDVI',lty=2) # monitoring period
  lines(stableHistory,col='blue',type="p",pch=19,cex=0.3)
  
  test_pred <- predict(test_lm, newdata = test_tspp)
  pfit <- window(ftsNDVI, start = subset_start) # this is the final monitor period
  pfit[!is.na(pfit)] <- test_pred
  lines(pfit, col = 'blue')

   abline(v = tbp, lty = 2, col='red',lwd=2) 
#  lines(out$Tt,col='purple',lty=3) 
#   lines(confint(fitbp))
  legend("bottomleft",c("History","Monitoring","Stable History","fit based on stable history")
  ,lty=c(1,2,NA,1),col=c(1,1,'blue','blue'),pch=c(NA,NA,19,NA))
dev.off()
 
  ## output
#   output$timebp[i] <- tbp
#  }

#write.csv(output,"output.csv")
#fix(output)

length(which(is.na(ftsNDVI)))
length(test_pred)
length(ftsNDVI)



########
## Final Comments: for the paper writing discussion section
########

## the great thing about the monitoring approach is that is Interpolation is not necessary!!!
## the users just need to define a period in time where they want to use this function to detect changes

## Assess the length of the history period and the quality of the fit
## a break is detected in 2006 which corresponds to a dry period in the plantation forest.
# for the manuscript plot number 2 is a great illustration of the principle!

## when looking at i 117 you can see that the method is robust against real outliers - also the roc() function
## does not take these into account.
## Z? it is only strange that for 117 not change is detected at the end of the time series
############
##### end of script

