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

recresid2 <- function(x, y, start = ncol(x) + 1, end = nrow(x), ...)
{
    stopifnot(start > ncol(x) & start <= nrow(x))
    stopifnot(end >= start & end <= nrow(x))
    n <- end
    q <- start - 1
    w <- rep(0, (n-q))

    for(i in ((q+1):n))
    {
      xi1 <- x[1:(i-1), , drop = FALSE]
      yi1 <- y[1:(i-1)]
      fmi1 <- lm(yi1 ~ 0 + xi1)       
      xi <- x[i, , drop = FALSE]
      sigmai <- sqrt(1 + xi %*% summary(fmi1)$cov.unscaled %*% t(xi))
      w[i-q] <- (y[i] - sum(x[i,] * coef(fmi1))) / sigmai
    }

    return(w)
}

recresid3 <- function(x, y, start = ncol(x) + 1, end = nrow(x),
  tol = sqrt(.Machine$double.eps), ...)
{
  ## checks and data dimensions
  stopifnot(start > ncol(x) & start <= nrow(x))
  stopifnot(end >= start & end <= nrow(x))
  n <- end
  q <- start - 1
  k <- ncol(x)
  rval <- rep(0, n - q)

  ## initialize recursion
  y1 <- y[1:q]
  fm <- lm.fit(x[1:q, , drop = FALSE], y1)
  X1 <- chol2inv(qr.R(fm$qr))	 
  betar <- fm$coefficients
  xr <- as.vector(x[q+1,])
  fr <- as.vector((1 + (t(xr) %*% X1 %*% xr)))
  rval[1] <- (y[q+1] - t(xr) %*% betar)/sqrt(fr)

  ## check recursion agains full QR decomposition?
  check <- TRUE

  if((q+1) < n)
  {
    for(r in ((q+2):n))
    {
	## recursion formula
        X1 <- X1 - (X1 %*% outer(xr, xr) %*% X1)/fr
        betar <- betar + X1 %*% xr * rval[r-q-1] * sqrt(fr)

	## full QR decomposition
	if(check) {
	  y1 <- y[1:(r-1)]
	  fm <- lm.fit(x[1:(r-1), , drop = FALSE], y1)
	  ## keep checking?
	  if(mean(abs(betar - fm$coefficients)) < tol) check <- FALSE 
	  X1 <- chol2inv(qr.R(fm$qr))
          betar <- fm$coefficients
        }
        
        ## residual
        xr <- as.vector(x[r,])
	fr <- as.vector((1 + (t(xr) %*% X1 %*% xr)))
	rval[r-q] <- (y[r] - sum(xr * betar, na.rm = TRUE))/sqrt(fr)
    }
  }
  return(rval)
}


logkappa <- function(x, y, start = ncol(x) + 1, end = nrow(x), ...)
{
    stopifnot(start > ncol(x) & start <= nrow(x))
    stopifnot(end >= start & end <= nrow(x))
    n <- end
    q <- start - 1
    w <- rep(0, (n-q))
    for(i in ((q+1):n)) w[i-q] <- log(kappa(crossprod(x[1:(i-1), , drop = FALSE]), exact = TRUE))
    return(w)
}


library("strucchange")

x <- ts(read.csv("tshistory.csv")$x, start = c(2000, 4), frequency = 23)
x <- tspp(x, order = 3)
x <- x[nrow(x):1,]
xx <- model.matrix(response ~ trend + harmon, data = x)
yy <- x$response
rr <-  data.frame(
  strucchange =  recresid(xx, yy),
  byhand      = recresid2(xx, yy),
  hybrid      = recresid3(xx, yy)
)
write.csv(rr,"outputtest_debian.csv", row.names = FALSE)
