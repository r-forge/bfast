## A function to remove unrealistic dips
## approximate the upper envelope
removedips <- function(x) {
  x <- na.approx(x, rule = 2)
  y <- x
  leng <- length(x) - 2
  for(i in 1:leng)
  {
    ## moving window - check distance
    b <- i + 2
    c <- b - 1
    mida <- x[c] - x[i]
    midc <- x[c] - x[b] 
    
    # Find 20 percent
    threshold1 <- (-1/100) * x[i] 
    threshold2 <- (-1/100) * x[b]
    
    # check threshold
    if( mida < 0 & midc < 0 &  mida < threshold1 & midc < threshold2) {
      y[c] <- (x[b] + x[i]) / 2} 
  }
  return(y)
}

# ## example zone
# if (FALSE) {
#   require(bfast)
#   y <- as.ts(zoo(som$NDVI.a, som$Time))
#   test <- removedips(y)
#   plot(y, type = "l", col = "grey", lwd = 2)
#   lines(test, col = "red")
# }


