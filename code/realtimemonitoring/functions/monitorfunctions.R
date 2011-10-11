# bfast code folder
## convenience function for time series pre-processing
## to response plus regressors (linear time trend, season
## dummies, and harmonic season by default with order 3)
require("strucchange")
require("zoo")
#   data <- tsndvi
#   startmonitor <- c(2009,1)
#   order <- 3
#   title <- TRUE

bfastmonitor <- function(data, start,
  formula = response ~ trend + harmon,
  order = 3, lag = NULL, slag = NULL,
  history = c("ROC", "BP", "all"),
  end = 10, level = 0.05,
  hpc = "none", verbose = FALSE, plot = FALSE)
{
  ## PREPROCESSING
  ## two levels needed: 1. monitoring, 2. in ROC (if selected)
  level <- rep(level, length.out = 2)
  ## data needs to be ts
  if(!is.ts(data)) data <- as.ts(data)
  ## frequency of data
  freq <- frequency(data)
  ## start on natural scale (if necessary)
  time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x
  start <- time2num(start)

  ## full data
  data_tspp <- bfastpp(data, order = order, lag = lag, slag = slag)
    
  
  ## SELECT STABLE HISTORY  
  ## full history period
  history_tspp <- subset(data_tspp, time < start)

  ## find start of history period
  ## (may be specified via character, function, or time index directly)
  if(is.null(history)) {
    history <- start(history_tspp$response)
  } else if(all(is.character(history))) {
    history <- match.arg(history)
    history <- switch(history,    
      "none" = start(history_tspp$response),      
      "ROC" = history_roc(formula, data = history_tspp, level = level[2]),
      "BP" = history_break(formula, data = history_tspp, hpc = hpc)
    )
  } else if(all(is.function(history))) {
    history <- history(formula, data = history_tspp)
  }
  history <- time2num(history)

  ## compute subset
  history_tspp <- subset(history_tspp, time >= history)

  ## output information (if desired)
  if(verbose) {
    cat("\nBFAST monitoring\n\n1. History period\n")
    cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n",
      start(history_tspp$response)[1], start(history_tspp$response)[2],
      end(history_tspp$response)[1], end(history_tspp$response)[2]))
    cat(sprintf("Length (in years): %f\n", NROW(history_tspp)/freq))
  }


  ## MODEL HISTORY PERIOD
  test_tspp <- history_tspp
  test_mefp <- mefp(formula, data = test_tspp, 
    type = "OLS-MOSUM", period = end, h = 0.25, alpha = level[1])
  test_lm <- lm(formula, data = test_tspp)
  if(verbose) {
    cat("Model fit:\n")
    print(coef(test_lm))
  }

  ## MONITOR CHANGES IN THE MONITORING PERIOD
  test_tspp <- subset(data_tspp, time >= history)
  test_mon <- monitor(test_mefp, data = test_tspp, verbose = FALSE)
  tbp <- if(is.na(test_mon$breakpoint)) NA else test_tspp$time[test_mon$breakpoint]
  if(verbose) {
    cat("\n\n2. Monitoring period\n")
    cat(sprintf("Monitoring starts at: %i(%i)\n", floor(start), round((start - floor(start)) * freq) + 1))
    if(is.na(tbp)) {      
        cat("Break detected at: -- (no break)\n\n")
      } else {
        cat(sprintf("Break detected at: %i(%i)\n\n", floor(tbp), round((tbp - floor(tbp)) * freq) + 1))
    }
  }

  ## set up return object
  rval <- list(
    data = data,
    tspp = test_tspp,
    model = test_lm,
    mefp = test_mon,
    history = c(head(history_tspp$time, 1), tail(history_tspp$time, 1)),
    monitor = c(start, tail(test_tspp$time, 1)),
    breakpoint = tbp
  )
  class(rval) <- "bfastmonitor"
  
  ## plot if desired
  if(plot) plot(rval)
  
  ## return object
  return(rval)
}

print.bfastmonitor <- function(x, ...)
{
  freq <- frequency(x$data)
  cat("\nBFAST monitoring\n\n1. History period\n")
  cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n",
    floor(x$history[1]),
    round((x$history[1] - floor(x$history[1])) * freq) + 1,
    floor(x$history[2]),
    round((x$history[2] - floor(x$history[2])) * freq) + 1))
  cat(sprintf("Length (in years): %f\n", diff(x$history)))

  cat("Model fit:\n")
  print(coef(x$model))
  cat(sprintf("R-squared: %f\n", summary(x$model)$r.squared))

  cat("\n\n2. Monitoring period\n")
  cat(sprintf("Monitoring period assessed: %i(%i)--%i(%i)\n",
    floor(x$monitor[1]),
    round((x$monitor[1] - floor(x$monitor[1])) * freq) + 1,
    floor(x$monitor[2]),
    round((x$monitor[2] - floor(x$monitor[2])) * freq) + 1))
  cat(sprintf("Length (in years): %f\n", diff(x$monitor)))
  if(is.na(x$breakpoint)) {	
      cat("Break detected at: -- (no break)\n\n")
    } else {
      cat(sprintf("Break detected at: %i(%i)\n\n", floor(x$breakpoint), round((x$breakpoint - floor(x$breakpoint)) * freq) + 1))
  }

  invisible(x)
}

plot.bfastmonitor <- function(x, main = TRUE, ylab = "Data", ...)
{
  if(isTRUE(main)) main <- if(is.na(x$breakpoint)) {
    "No break detected"
  } else {
    sprintf("Break detected at: %i(%i)", floor(x$breakpoint),
      round((x$breakpoint - floor(x$breakpoint)) * frequency(x$data)) + 1)
  }

  y <- if(is.null(dim(x$data))) x$data else x$data[,1L]
  plot(y, type = "n", main = main, ylab = ylab, ...)
  lines(window(y, end = x$history[2]), col = "black")
  lines(window(y, start = x$history[1], end = x$history[2]),
    col = "green", type = "p", pch = 19, cex = 1)
  lines(window(y, start = x$monitor[1]), col = "red")

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
    
bfastpp <- function(y, order = 3,
  lag = NULL, slag = NULL, na.action = na.omit,
  stl = c("none", "trend", "seasonal"))
{
  ## STL pre-processing to try to adjust for trend or season
  stl <- match.arg(stl)
  if(stl != "none") {
    stl_adjust <- function(x) x - stats::stl(x, s.window = "periodic")$time.series[, stl]
    if(NCOL(y) > 1L) {
      for(i in 1:NCOL(y)) y[,i] <- stl_adjust(y[,i])
    } else {
      y <- stl_adjust(y)
    }
  }

  ## check for covariates
  if(NCOL(y) > 1L) {
    x <- coredata(y)[, -1L]
    y <- y[, 1L]
  } else {
    x <- NULL
  }

  ## data with trend and season factor
  rval <- data.frame(
    time = as.numeric(time(y)),
    response = y,
    trend = 1:NROW(y),
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

  ## add lags
  nalag <- function(x, k) c(rep(NA, k), head(x, -k))
  if(!is.null(lag)) {
    rval$lag <- sapply(lag, function(k) nalag(as.vector(y), k))
    colnames(rval$lag) <- lag
  }
  if(!is.null(slag)) {
    rval$slag <- sapply(slag * freq, function(k) nalag(as.vector(y), k))
    colnames(rval$slag) <- slag
  }
  
  ## add regressors
  rval$xreg <- x

  ## omit missing values
  rval <- na.action(rval)
  
  ## return everything
  return(rval)
}

########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################

## A technique to verify whether or not the historical period is stable or not
## reversely order sample and perform
## recursive CUSUM test
history_roc <- function(formula, data, level = 0.05) {
  n <- nrow(data)
  data_rev <- data[n:1,]
  data_rev$response <- ts(data_rev$response)
  y_rcus <- efp(formula, data = data_rev, type = "Rec-CUSUM")

  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  data$time[y_start]
}

##################################
## Bai & Perron last breakpoint ##
##################################

history_break <- function(formula, data, h = NULL, hpc = "none") {
  n <- nrow(data)
  ## rule of thumb for minimal segment size
  if(is.null(h)) h <- 6 * NCOL(model.matrix(formula, data = data[0,]))

  ## conduct breakpoints estimation
  bp <- breakpoints(formula, data = data, h = h, hpc = hpc)

  y_start <- tail(breakpoints(bp)$breakpoints, 1)
  y_start <- if(is.na(y_start)) 1 else y_start + 1
  data$time[y_start]
}
