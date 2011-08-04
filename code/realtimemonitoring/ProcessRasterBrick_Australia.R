## CODE ON SVN R-FORGE
###
## Read in Raster Brick created from MODIS images
## Extract Time series and monitor
## Develop a function to monitor and use with raster calc()
## Author: Jan Verbesselt, Achim Zeileis
###

## Required packages
# library(doMC) # High Performance Computing
# registerDoMC(2)
library(raster)
library(rgdal)
library(zoo)
library(ggplot2)
library(sp)

## defined functions
timeser <- function(index,dt) {
  z <- zoo(index,dt)
  yr <- as.numeric(format(time(z), "%Y"))
  jul <- as.numeric(format(time(z), "%j"))
	delta <- min(unlist(tapply(jul, yr, diff))) # 16
	zz <- aggregate(z, yr + (jul - 1) / delta / 23)
	(tso <- as.ts(zz))
	return(tso)	
}

## read in raster Brick
modis <- stack("data/modis.grd")
dim(modis)
prj <- projection(modis)

## read in Layernames
lnames <- read.csv("data/layernames.csv")
tail(lnames$x)
layerNames(modis) <- as.character(lnames$x) # set the correct layer names

## get all the values from the modis file and read them into the memory
system.time(m <- getValues(modis))
str(as.matrix(m))
nrpix <- dim(m)[1]

## create a time series
i <- 11912
# for (i in 1:nrpix) {
  d <- as.vector(m[i,])
  datum <- as.Date(as.character(lnames$x))
  tsndvi <- timeser(d/10000,datum)
  tsndvi <- window(tsndvi,end=c(2009,23))  # just check whether something is happening in 2009...
  plot(tsndvi)
  ## a realtime monitor function would be handy for further development
  ## however there is a bug in the data

  source('functions/monitorfunctions.R')
  out <-realtime(tsdata = tsndvi, startmonitor = c(2009,1), order= 3, title=TRUE) ## 

## The section below is identical to the defined function however somehow the 'test_tspp'is not 
## recognized by the monitor() function within the realtime() function.

#     ## input
#     end <- c(2009,1)
#     ## first define the history period
#     tshistory <- window(tsndvi, end = end)
#     
#     ## 1. check for stability in the history period
#     subset_start <- roc(tshistory, order = 3, level = 0.05) # searching for a stable period 
#     print(subset_start) ## stable history ** output 1!
#     
#    ## 2. model the history period
#     stableHistory <- window(tshistory, start = subset_start)
#     print(lstablehist <- length(stableHistory)/frequency(stableHistory)) ## ** output 2: amount of years within the stable history
#     test_tspp <- tspp(stableHistory, order = 3)
#     test_mefp <- mefp(response ~ trend + harmon, data = test_tspp,  type = "OLS-MOSUM", h = 0.25, alpha = 0.05)
#     test_lm <- lm(response ~ trend + harmon, data = test_tspp)
#     
#     ## 3. monitor for change in the monitoring period
#     test_tspp <- tspp(window(tsndvi, start = subset_start),  order = 3)
#     test_mon <- monitor(test_mefp)
#     if (is.na(test_mon$breakpoint)) { tbp <- NA} else { tbp <- test_tspp$time[test_mon$breakpoint]}
#     
#     ## 4. plot 
#     title = TRUE
#     plot(tsndvi,type='n', main = if (title) {
#       if (!is.na(tbp[1])) { 
#           paste("Time of detected break is", format(tbp,digits=6))} else
#           { "no breakpoint detected"}
#       }, ylab = 'NDVI'
#     )
#     lines(tshistory) # history period
#     lines(stableHistory,col='green',type="p",pch=19,cex=0.3)
#     test_pred <- predict(test_lm, newdata = test_tspp)
#     test_pred <- as.ts(zoo(test_pred,test_tspp$time))
#     lines(test_pred, col = 4,lwd=1.5)
#     abline(v = tbp, lty = 2, col='red', lwd=2) # time of the breakpoint detected by the monitoring process!
#     lines(window(tsndvi,start=end),col='red',type='l',pch=19, cex=0.5)
#     
#     legend("bottomleft",
#     c("History","Stable History","Monitoring","fit based on stable history", "Time of Detected Break")
#       ,lty=c(1,NA,1,1,2),col=c(1,'green','red','blue','red'),pch=c(NA,19,NA,NA,NA))
    
#     ## output as data.frame
#     ## is there a break yes or no? When is the break  (maybe determine the type of break)
#     ## length of the history period  
#     if (i == 1) {out <- data.frame(tbp,lstablehist)} else {
#       out <- rbind(out,c(tbp,lstablehist))
#     }
# }

## Another idea is to use the calc() function from the RASTER package
## to apply realtime as a FUN on a RasterBrick
## Alternatively: Could we avoid using a for loop above when looping through all the pixels in this raster brick (i.e. x,y,time raster )
?calc

      ## save the output to a raster file
      # last <- raster("modis.grd",band=1)/10000
      # 
      # 
      # disturb <- setValues(last,out$tbp)
      # lhist <- setValues(last,out$lstablehist)
      # writeRaster(data/disturb,'disturb.grd',overwrite=TRUE)
      # writeRaster(data/lhist,'lhist.grd',overwrite=TRUE)

