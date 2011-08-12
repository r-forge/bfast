# bfast code folder
## convenience function for time series pre-processing
## to response plus regressors (linear time trend, season
## dummies, and harmonic season by default with order 1)
require("strucchange")
require("zoo")
#   data <- tsndvi
#   startmonitor <- c(2009,1)
#   order <- 3
#   title <- TRUE

bfastmonitor <- function(data, start,
  order = 3, history = c("ROC", "BP", "all"), period = 10, level = 0.05, hpc = "none",
  verbose = TRUE, plot = TRUE)
{
  ## PREPROCESSING
  ## two levels needed: 1. monitoring, 2. in ROC (if selected)
  level <- rep(level, length.out = 2)
  ## start on natural scale (if necessary)
  start2 <- if(length(start) > 1) start[1] + (start[2] - 1)/frequency(data) else start
  ## data needs to be ts
  if(!is.ts(data)) data <- as.ts(data)

  
  ## SELECT STABLE HISTORY  
  ## define start of history period
  ## (last observation before start of monitoring)
  tshistory <- window(data, end = start)
  tshistory <- window(tshistory, end = time(tshistory)[length(tshistory) - 1])

  ## find start of history period
  ## (may be specified via character, function, or time index directly)
  if(is.null(history)) {
    history <- start(data)
  } else if(all(is.character(history))) {
    history <- match.arg(history)
    history <- switch(history,    
      "none" = start(x),      
      "ROC" = roc(x, order = order, level = level[2]),
      "BP" = bplast(x, order = order, hpc = hpc)
    )
  } else if(all(is.function(history))) {
    history <- history(x)
  }

  ## compute subset
  stableHistory <- window(tshistory, start = history)
  lstablehist <- length(stableHistory)/frequency(stableHistory)

  ## output information (if desired)
  if(verbose) {
    cat("BFAST monitoring\n\n1. History period\n")
    cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n",
      start(stableHistory)[1], start(stableHistory)[2],
      end(stableHistory)[1], end(stableHistory)[2]))
    cat(sprintf("Length (in years): %f\n", lstablehist))
  }


  ## MODEL HISTORY PERIOD
  test_tspp <- tspp(stableHistory, order = order)
  test_mefp <- mefp(response ~ trend + harmon, data = test_tspp, 
    type = "OLS-MOSUM", period = period, h = 0.25, alpha = level[1])
  test_lm <- lm(response ~ trend + harmon, data = test_tspp)
  if(verbose) {
    cat("Model fit:\n")
    print(coef(test_lm))
  }

  ## MONITOR CHANGES IN THE MONITORING PERIOD
  test_tspp <- tspp(window(data, start = history), order = order)
  test_mon <- monitor(test_mefp, data = test_tspp, verbose = FALSE)
  tbp <- if(is.na(test_mon$breakpoint)) NA else test_tspp$time[test_mon$breakpoint]
  if(verbose) {
    cat("\n\n2. Monitoring period\n")
    cat(sprintf("Monitoring starts at: %i(%i)\n", floor(start2), round((start2 - floor(start2)) * frequency(data)) + 1))
    if(is.na(tbp)) {      
        cat("Break detected at: -- (no break)\n")
      } else {
        cat(sprintf("Break detected at: %i(%i)\n", floor(tbp), round((tbp - floor(tbp)) * frequency(data)) + 1))
    }
  }

  ## set up return object
  rval <- list(
    data = data,
    tspp = test_tspp,
    model = test_lm,
    mefp = test_mon,
    history = c(head(time(stableHistory), 1), tail(time(stableHistory), 1)),
    monitor = c(start2, tail(test_tspp$time, 1)),
    breakpoint = tbp
  )
  class(rval) <- "bfastmonitor"
  
  ## plot if desired
  if(plot) plot(rval)
  
  ## return object
  return(rval)
}

plot.bfastmonitor <- function(x, main = TRUE, ylab = "Data", ...)
{
  if(isTRUE(main)) main <- if(is.na(x$breakpoint)) {
    "No break detected"
  } else {
    sprintf("Break detected at: %i(%i)", floor(x$breakpoint),
      round((x$breakpoint - floor(x$breakpoint)) * frequency(x$data)) + 1)
  }

  plot(x$data, type = "n", main = main, ylab = ylab, ...)
  lines(window(x$data, end = x$history[2]), col = "black")
  lines(window(x$data, start = x$history[1], end = x$history[2]),
    col = "green", type = "p", pch = 19, cex = 1)
  lines(window(x$data, start = x$monitor[1]), col = "red")

  test_pred <- predict(x$model, newdata = x$tspp)
  test_pred <- as.ts(zoo(test_pred, x$tspp$time))
  lines(test_pred, col = "blue", lwd = 1.5)

  abline(v = x$monitor[1], lty = 2, col = "black", lwd = 1)
  abline(v = x$breakpoint, lty = 2, col = "red", lwd = 2)
    
  legend("bottomleft", bty = "n",
    c("History", "Stable history", "Monitoring", "Fit based on stable history", "Time of detected break"),
    lty = c(1, NA, 1, 1, 2),
    col = c("black", "green", "red", "blue", "red"),
    pch = c(NA, 19, NA, NA, NA)
  )
  invisible(x)
}


#########################
## Auxiliary functions ##
#########################
    
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

##################################
## Bai & Perron last breakpoint ##
##################################

bplast <- function(y, order = 3, h = NULL, hpc = "none") {
  y_orig <- tspp(y, order = order)
  n <- nrow(y_orig)
  ## rule of thumb for minimal segment size
  if(is.null(h)) h <- 2 * (order + 1) * 6

  ## conduct breakpoints estimation
  bp <- breakpoints(response ~ trend + harmon, data = y_orig, h = h, hpc = hpc)

  y_start <- tail(breakpoints(bp)$breakpoints, 1)
  y_start <- if(is.na(y_start)) 1 else y_start + 1

  rval <- y_orig$time[y_start]
  c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
}
