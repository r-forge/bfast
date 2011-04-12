require(bfast)

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
  
	y_rev$response <- ts(y_rev$response, start = -end(y), frequency = frequency(y))
  plot(y_rev$response)
	
  y_rcus <- efp(response ~ trend + harmon, data = y_rev, type = "Rec-CUSUM")
	plot(y_rcus)
  lines(y_rcus$process, type='p',pch=20,cex=0.5)
  length(y_rcus$process) 
  # ?A: Why is the length of the recursive process shorter then the length of the y time series?
  # does this influence the time estimation below?
  
  #if(plot) plot(y_rcus)
	
  y_start <- if(sctest(y_rcus)$p.value < level) {
				length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
			} else {
				1    
			}
      
  # ?A how could I plot the identified point on the y_rcus plot?
  tpoint <- min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  points(time(y_rcus)[tpoint],y_rcus$process[tpoint],cex=1.2,col='red') 
  
  
  # ?A should it not be 
  length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) -1 ###? -1
  
	rval <- as.numeric(time(y)[y_start])
  c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
	tp <- c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
	abline(v=-rval,col="red",lty=2)
  # A? just trying to understand but should this not be just before that the recursive progress
  # crosses the border - as shown on the graph?
  
  
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


