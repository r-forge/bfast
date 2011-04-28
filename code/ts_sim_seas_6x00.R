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

###############
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
roc <- function(y, order = 3, level = 0.05, plot = TRUE) {
  y_orig <- tspp(y, order = order)
  n      <- nrow(y_orig)
  y_rev  <- y_orig[n:1,]
  y_rev$response <- ts(y_rev$response, start = -tail(time(y), 1), frequency = frequency(y))
  y_rcus <- efp(response ~ trend + harmon, data = y_rev, type = "Rec-CUSUM")
  if(plot) plot(y_rcus)

  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  rval <- as.numeric(time(y)[y_start])
  if(plot & sctest(y_rcus)$p.value < level) abline(v=-rval,col="green")
  c(floor(rval), round((rval - floor(rval)) * frequency(y)) + 1)
}

###################################################################################################################
## A FUNCTION THAT SIMULATES A SEASONAL SIGNAL SIMILAR TO THE SEASONAL SIGNAL NDVI SIGNAL OF FOREST PLANTATIONS  ##
###################################################################################################################
SOS <- function(x){  # derive the start of the Season White et al. 1997
# frequency is hardcoded to 23!
	fr <- frequency(x)
	x <- as.vector(x)
	nx <- (x-range(x)[1])/(range(x)[2]-range(x)[1])  # normalize
	f <- splinefun(nx,method="fmm")  # derive a function
	(v <- (1:(fr*32))/32)  # 1 = 16 days, divided by 32 is  0.5 days time step
	test <- 0
	i <- 32 # we start from the first decade!!! because spline functions can be over sensitive in the tails
	while(test < 0.5 & v[i] < 24 ) { # search for the value just above the 0.5 mark (in the first half of the season!)
		test <- f(v[i])
		i <- i+1
	}
	return(v[i-1])
}

simulatets <- function
              (
                nrobs = 208, a = 0, adelta = 0, c1 = 15, c1delta = 0,c2 = 15,
                dip,
                ts.sim.noise,
                averageNDVI, t1 = 50, t2 = 100, t3 = 150
              )
{
	t <- 1:nrobs # LENGTH OF THE THE TIME SERIES
	# Assymmetric Gauss function to simulate seasonality
 	ags <- function(a,b,c1,c2) 
		{
		# a amplitude # b halfway point  # c1 and c2 width of left and right handside
	   	i <- 1:b
	   	Y1 <- a*exp(-( ((i-b)^2)/(2*c1) ))
	   	i <- b:22
	   	Y2 <- a*exp(-( ((b-i)^2)/(2*c2) ))
	   	Y <- c(Y1,Y2)
	  	return(Y)
	    }

    #a <- 0.15 #  c1 <- 15 #  c2 <- 15 
    g1 <- ts(ags(a,12,c1,c2),frequency=23)
    g2 <- ts(ags(a+adelta,12,c1+c1delta,c2),frequency=23)
	    
	#    (tex <- paste('SeasonalShape_a',a,'_adelta',adelta,'_c1delta',c1delta,'_shft',round(sim$dSOS,1),sep=''))
	#    saveeps(tex,pointsize=12, height= 12) 
	#    plot(g1,ylim=range(g1,g2),type='l',lty=2,ylab='')
	#    lines(g2,lty=1)
	#    dev.off()    
	#    length(g1);length(g2)
	#    tg1 <- g1-mean(g1)
	#    tg2 <- g2-mean(g2)
	#    plot(tg1,ylim=range(tg1,tg2))
	#    lines(tg2)
	#    SOS(g1)
	#    SOS(g2)
    
   	dSOS <- SOS(g1) - SOS(g2)
   	# the seasonal change occurs more or less half way!
	# the change is in c(2004,1)
	apart <- c(rep(g1,2),rep(g1,1)[1:18])
	#apart <- apart - mean(apart)  # centralized the simulated seasonal signal
	
	bpart <- c(rep(g1,1)[19:23],rep(g2,3),rep(g2,1)[1:12])
	#bpart <- bpart - mean(bpart)  # centralized the simulated seasonal signal
	
	cpart <- c(rep(g1,1)[13:23],rep(g1,2),g2[1:1])
	#cpart <- cpart - mean(cpart)
	
	S  <- c(apart,bpart,cpart)
	S <- S - mean(S)
  ts.sim.season <- createts(S[1:nrobs])
	#time(ts.sim.season) [length(c(g1[10:23],rep(g1,4),rep(g2,1)/2))] # time of the change more or less!
	#  plot(ts.sim.season)
	#  # test dSOS after translation (minus the mean)
	#  season1 <- window(ts.sim.season,start=c(2000,1),end=c(2000,23))
	#  season2 <- window(ts.sim.season,start=c(2005,1),end=c(2005,23))
	#  SOS(season1)-SOS(season2)
  	# --> trend
  	trend <- 3
  	slope <- 1/8 # change this factor to modify the slope of the log function ( the smaller this factor e.g. 1/8 the softer the slope )
    #opar <- par(mfrow=c(4,1),mar=c(0, 4, 3, 2))
	## --> trend 1: tree growth
	if (trend ==1) {
      	s.trend <- log(seq(1,90*slope,length.out=nrobs))
      	ts.sim.trend  <- createts((0.6*s.trend/max(s.trend)+0.37))
		plot(ts.sim.trend)
  	} else if (trend ==2) {
		## --> trend 2: long term trend
		A <- 0.2
		freq <- 1/46 # on cycle every 23 time points (seasonal cycle)
		s.trend <- A*cos(t*freq)+0.65
		ts.sim.trend  <- createts(s.trend)
		#par(mar=c(0, 4, 0, 2))
		#plot(ts.sim.trend)
	} else if (trend ==3) {
		## --> trend 3: No trend
		ts.sim.trend  <- createts(rep(averageNDVI, nrobs))
		#par(mar=c(3, 4, 0, 2))
		#plot(ts.sim.trend)
	} else if (trend ==4) {
		## --> trend 4: gradual trend
		(sslope <- (averageNDVI-(averageNDVI-0.2))/200) # slope variation
		gt <- (averageNDVI-0.2)+ sslope*t   #^2 #+ sslope/400*ti^2
		#plot(gt,type='l')
		ts.sim.trend  <- createts(gt)
		#par(mar=c(3, 4, 0, 2))
		#plot(ts.sim.trend)
	}
      #}
  	#par(opar)
	# Abrupt change  # Linearity
  	t <- 0:(nrobs-1) 
  	interc <-  dip
  	x <-  120
  	(slope <- (-dip/x)*23) # we derive the slope based on the dip
  	ldstb  <- interc + slope*(t/23)
  	ldstb[ldstb>0] <- 0
 	# plot(t, ldstb)

	  s.ach <- rep(0,nrobs)
	  addabrupt <- function(ta,s.a,disturb) 
	    {
	    per <- length(ta:nrobs)
	    s.a[ta:nrobs] <- s.a[ta:nrobs] + disturb[1:per]
	    return(s.a)
	    }
    
  	#s.ach <- addabrupt(t1,s.ach,ldstb)
  	s.ach <- addabrupt(t2,s.ach,ldstb)
  	#s.ach <- addabrupt(t3,s.ach,ldstb)
  
  	ts.sim.ach <- createts(s.ach)
#  plot(ts.sim.ach) 
  ts.sim.data <- (ts.sim.trend+ts.sim.ach)+ts.sim.noise+ts.sim.season
  
#ts.sim <- cbind(ts.sim.trend,ts.sim.ach,ts.sim.season,ts.sim.noise,ts.sim.data)
#  plot(ts.sim)
  return(structure(list(ts.sim.t=ts.sim.trend,
                        ts.sim.a=ts.sim.ach,
                        ts.sim.s=ts.sim.season,
                        ts.sim.n=ts.sim.noise,
                        ts.sim.d=ts.sim.data,
						dSOS=dSOS)
						,class="tssim"))
}