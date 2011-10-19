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
