##START OF GROWING SEASON FUNCTION
sos <- function(x, current_year, compInterval = 15, p = 4, n = 21, plot = TRUE, rmdips = TRUE){
  ## start values
  years <- unique(floor(time(x)))
  set_year <- current_year
  start_year <- years[1] 
  total_year <- start_year + length(years)
  year_length <- round (365/compInterval)
  freqSpac    <- trunc (365/compInterval)
  
  if ( freqSpac < year_length ){ 
    len_year <- 368 } else { len_year <- 365 }
  
  ## remove dips
  if (rmdips) {
    y <- removedips(x)
  } else {
    y <- na.approx(x, rule = 2)
  }
  
  # Fit Savitsky-Golay filter to smoothen the data
  sgd <- sgolayfilt(y, p = p, n = n)
  
  # Create time-series for smoothed data
  smooth <- ts(sgd, start = c(start_year, 1), frequency = frequency(x))
  
  # Extract one year from time-series
  onSm <- window(smooth, start=c(set_year, 1), end = c(set_year,frequency(x)))
  
  # Want to extract and append the first half values of the subsequent year
  # we need to do this so that the SOS function can also work in the areas where the growing season transverse two years
  # this cannot work for the year at end of the time series, however
  if (total_year < start_year) {
    onSm1 <- window(smooth, start = c(set_year+1, 1), 
                    end = c(set_year+1, round(frequency(x)*0.5)))
    onSm2 <- append(onSm, onSm1)
  } else {
    onSm2 <- onSm
  }
  
  # Create a daily dataset
  # first put the values at the correct date
  u <- matrix(nrow = len_year + round(len_year*0.5), ncol = 1)
  
  s <- compInterval 
  for(e in 1:length(onSm2))
  {
    u[s] <- onSm2[e] 
    s <- s +  compInterval 
  }
  
  # Now apply Cubic Spline interpolation to have day values
  g <- na.spline(u)
  
  # Convert daily dataset to a matrix
  daily <- matrix(unlist(g), ncol=1, byrow=TRUE)
  
  # Create time-series of daily values
  oneyear  <- ts(as.numeric(daily), start=c(set_year,1), frequency=len_year)
  
  ## Normalise the values as required by Midpoint pixel method
  ## get position of the maximum 
  
  maxpo <- max(oneyear[1:len_year])
  posmaxi <- which(oneyear [1:len_year] == maxpo)
  posmaxi <- posmaxi[length(posmaxi)]
  posmax <- posmaxi
  
  ##get position of the minimum
  minpo <- min(oneyear[1:len_year])
  pomin <- which(oneyear[1:len_year] ==  minpo)
  
  ## check for length of the vector from the minimum to end of vector 
  minch <- oneyear[pomin:length (oneyear)]
  minq <- length(minch)
  
  ##check for length of the vector from the first element to the minimum
  minch2 <- oneyear[1:pomin]
  minq2 <- length(minch2)
  
  ##check if the length of the vector from first element to postion of the minimum is not NA
  fsmon <- length (oneyear[1:pomin])
  fsm <- is.na(fsmon)
  
  ##check if the maximun is before the minimum
  pmax_pmin <- posmax -  pomin
  
  ##if the pmax_pmin is negative and minq is longer than 50, then minimum is the true minimun 
  if (minq > 50 && pmax_pmin < 1 && minq2 < 300) {
    posmxs <- (oneyear[pomin: length(oneyear)])
    maxpo  <- max(posmxs)
    minpox <- min(posmxs)
    posminx <- which(posmxs == minpox)
    posmaxi <- which(posmxs == maxpo)
    posmaxi <- posmaxi[length(posmaxi)]
    posmax <- posmaxi
    firsthalf <- posmxs[posminx:posmax]  # extracting the values up to the new maximum value
    
  } else {firsthalf <- oneyear[1:posmax]} #extracting the values up to the maximum value
  
  
  ##Calculate ratios: normilising the values
  firsthalf_ratio <- (firsthalf - min(firsthalf , na.rm=TRUE))/(max(firsthalf , na.rm=TRUE) - min(firsthalf, na.rm=TRUE))
  ratios <- firsthalf_ratio
  roneyear <- ts(as.numeric(ratios), start=c(set_year,1), frequency=len_year)
  
  # Determine START of a growing season
  qx <- length(roneyear) 
  d <- 1
  qz <- roneyear
  m <- is.nan(mean(qz) )
  
  ## Find position left minimum
  posmin <- which(roneyear == min(roneyear))
  posmaxi <- which(roneyear == max(roneyear))
  leftmin <- which(roneyear == min(roneyear))
  
  if (length(roneyear) > 0 && m == FALSE) {
    detectedstart <- matrix(ncol=1, nrow=posmaxi)
    if(min(roneyear[1:posmaxi]) < 0.5) {posmin = leftmin} else {posmin = posmin}
    for(d in posmin:qx)
    {
      if(roneyear[d] > 0.5) {detectedstart[d] <- roneyear[d]} else  {detectedstart[d] <- NA}
    }
    
    # On which position 0.5 is encountered for the first time
    firststart <- which(detectedstart > 0.5)
    min(roneyear[1:posmax])
    # The first position is the first value above 0.5 (start of Season)
    # AFTER that position, set every value to NA 
    start <- firststart[1] + 1
    detectedstart[start:length(detectedstart)] <- NA
    
    sos <- ts(as.numeric(detectedstart), start=c(set_year,1), frequency=len_year) 
    
    realstart <- firststart[1]

  } else {(realstart <- NA)} # if no sos is found
  
  if (minq > 50 && pmax_pmin < 1 && minq2 < 300) {realstart <-  realstart + (length (oneyear[1:pomin])-1)
  } else {(realstart <- realstart)}
  
  # Plot to see the position of SOS
  if (plot == TRUE){ 
    OrigPlot<- window(x, start=c(set_year-1, 1), end = c(set_year +1,frequency(x)))
    SmoothPlot<- window(smooth, start=c(set_year-1 ,1), end = c(set_year +1,frequency(x)))
    sosP <- set_year + (realstart/len_year)
    yma <- signif(max (OrigPlot,na.rm = TRUE), digits = 2)
    plot(SmoothPlot, col = "blue", ylim = c(0, yma), ylab = "Data")
    lines(OrigPlot, col = "red")
    abline(v = sosP, col = "green",lwd=2,lty=2)
    
    codyear <- set_year+((realstart + 100)/len_year)
    legend(codyear,0.3, c("Original data", "Smoothed data", "Detected start-of-season"),
           col=c("red", "blue","green"),lty=c(1,1,2),lwd= c(1,1,2),cex=.5)
    Slabel <- paste("Start-of-season = ", realstart)
    text(codyear, 0.05, labels =Slabel, cex =.5) 
  }
  # calculate thedifference between  fitted and observed values
  # One can play with Savitsky-Golay filter to reduce the difference
  sd_sgd <- sd(sgd-y, na.rm = TRUE)
  print(paste("SD of the difference between savitsky-golay fit and input data =", round(sd_sgd,3)))
  print (paste("SOS = ", realstart))
  return (realstart)
}