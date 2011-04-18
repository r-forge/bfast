require(bfast)

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

## Objective
## Dismantle ROC function in order to better understand how the function works
## I used some test data below from the BFAST package.

#roc <- function(y, order = 1, level = 0.05, plot = TRUE) {

order <- 3
y <- window(harvest, end = c(2004, 12))
plot(y)



plot <- TRUE
level <- 0.05
	y_orig <- tspp(y, order = order)
	n      <- nrow(y_orig)
	y_rev  <- y_orig[n:1,]
  
	y_rev$response <- ts(y_rev$response, start = -tail(time(y), 1), frequency = frequency(y))
  # !Z:this was wrong (but just affected the visualization)
  plot(y_rev$response)
	
  y_rcus <- efp(response ~ trend + harmon, data = y_rev, type = "Rec-CUSUM")
	plot(y_rcus)
  lines(y_rcus$process, type='p',pch=20,cex=0.5)
  length(y_rcus$process) 
  # ?A: I noticed that the length of the resulting process equals the lenght of the time series - nr of variables used in the model
  # does this influence the time estimation below?
  # !Z: Yes/No. The recursive CUSUM process is based on the cumultative sum of recursive residuals.
  # In other words: It's a cumulative sum of weighted 1-step forecasting errors. So for the first
  # "p" observations you cannot get such a forecast because you cannot estimate the coefficients.
  # For i > p, you can then compare y_i and \hat y_{i | i - 1}.
  # Due to the recursive ordering, you just make sure to count from the right direction, though:
  # The last point of y_rcus$process is the first point of y_orig!
  
  #if(plot) plot(y_rcus)
	
  y_start <- if(sctest(y_rcus)$p.value < level) {
				length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
			} else {
				1    
			}
      
  # ?A how could I plot the identified point on the y_rcus plot?
  # !Z: Just the way I did in my original roc() function:
  abline(v = -as.numeric(time(y)[y_start]), col = "red")
  
#   tpoint <- min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
#   points(time(y_rcus)[tpoint],y_rcus$process[tpoint],cex=1.2,col='red') 
  
  
  # ?A should it not be 
  length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) -1 ###? -1
  # !Z: No. If w = which(...) is the first point outside the boundary. Then w - 1 is
  # the last point inside the boundary. Then you need to reverse: n* - (w - 1) = n* - w + 1
  # where n* is the length of the process: n* = n - p + 1.
  
	rval <- as.numeric(time(y)[y_start])
  c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
	tp <- c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
	abline(v=-rval,col="red",lty=2)
  # A? just trying to understand but should this not be just before that the recursive progress
  # crosses the border - as shown on the graph?
  # !Z: As I said above: I just realized today that the graph is somewhat misleading
  # but I re-checked the computations and they appear to be correct.
  
  
#}

# the identified stable period within the history section!
plot(y)
lines(window(y,start=tp),col='blue')

# ## Load dataset "nhtemp" with average yearly temperatures in New Haven
# 
# data("nhtemp")
# 
# ## plot the data
# plot(nhtemp)
# 
# ## test the model null hypothesis that the average temperature remains constant
# ## over the years
# ## compute OLS-CUSUM fluctuation process
# temp.cus <- efp(nhtemp ~ 1, type = "OLS-CUSUM")
# plot(temp.cus)
# 
# ## plot the process without boundaries
# plot(temp.cus, alpha = 0.01, boundary = FALSE)
# 
# ## add the boundaries in another colour
# bound <- boundary(temp.cus, alpha = 0.01)
# lines(bound, col=4)
# lines(-bound, col=4)
# 
# #
# temp.cus <- efp(nhtemp ~ 1, type = "Rec-CUSUM")
# plot(temp.cus)

require(strucchange)
x <- rnorm(100)
x[51:100] <- x[51:100] + 2
rr <- recresid(x ~ 1)
plot(cumsum(rr), type = "l")

test <- efp(x ~ 1, type = "Rec-CUSUM")
plot(test)
length(test$process)

t <- 1:100
test <- efp(x ~ t, type = "Rec-CUSUM")
plot(test)
length(test$process)

# ok - good to know the resulting recursive process is a time series with a length of the time series - number of variables


