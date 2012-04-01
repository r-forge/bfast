## Example script 
## For a new light version of bfast 
## based on bfastpp

## Author Jan Verbesselt
require(bfast)

## simple BFAST
## without decomposition - just by fitting a seasonal trend model
## this might be ok for general purposes - and fast processing
## A bfast version that is flexible and deals with NA's

##########
## Data ##
##########

NDVIb <- as.ts(zoo(som$NDVI.b, som$Time))
plot(NDVIb)
NDVIb
## add random NA's to the time series!!!
i <- as.integer(runif(40,min=0,max=length(NDVIb)))      
NDVIb[i] <- NA     # with VI specific noise
which(is.na(NDVIb))
plot(NDVIb)

##############
## Modeling ##
##############

## set-up
d <- bfastpp(NDVIb, order=3)
# d$response <- na.omit(NDVIb) ## to retain 'ts' properties

## testing 
plot(efp(response ~ trend + harmon, data = d, type = "OLS-MOSUM", h = 0.125))

## dating
bp <- breakpoints(response ~ trend + harmon, data = d)
breakpoints(bp)
plot(bp)
confint(bp)

## check segmentation
d$seg <- breakfactor(bp)
levels(d$seg) <- seq_along(levels(d$seg))
m0 <- lm(response ~ trend + harmon, data = d)
m1 <- lm(response ~ seg/trend + harmon,   data = d)
m2 <- lm(response ~ seg/(trend + harmon), data = d)
anova(m1, m2) # trend and seasonal break model is better!
anova(m0, m1, m2) # how can you automatically derive from the anova model which model fits the data best?
summary(m2)  # the best model


## visualise
plot(response ~ time, data = d, type = "l", col = "gray", pch=19, cex=0.1) ## data gaps are not visible here
plot(NDVIb, col = "gray") ## here data gaps are visible
# lines(fitted(m0) ~ time, data = d, lwd = 2, col = 1, pch=19, cex=0.2, lty=3) # without break
# lines(fitted(m1) ~ time, data = d, lwd = 2, col = 'darkgreen', pch=19, lty=2) # with break in the trend
lines(fitted(m2) ~ time, data = d, lwd = 2, col = 'blue', pch=19, cex=0.5,lty=4) # with break in the trend
## breakpoints and confint
ci <- confint(bp)
timing <- d$time[ci$confint]
abline(v=timing[2], col='red')  # this is not correct

##
d2 <- d
d2$harmon <- d2$harmon * 0
lines(d2$time, predict(m2, d2), col = 4, lty = 2)
arrows(timing[1], 0.2, timing[3], 0.2, col= 'red', code=0, cex=2,lwd=2) # confidence interval

## to do
## 1) derive the magnitude of the detected trend breaks
## 2) check output of confint() for influence of NA's (is the time estimate correct?) and the plotting

## output needed
## 1) detect trend breaks and identify the biggest trend break (if multple trend breaks)
## 2) derive the time and magnitude of the all trend breaks
## 3) the confidence interval of the trend break

coef(m2)[1] # intercept 1
coef(m2)[2] # intercept 2

coef(m2)[3] # slope 1
coef(m2)[4] # slope 2

## what is the correct time of the breakpoint?
confint(bp)
bp

predict(m2,d2)[ci$confint[2]]
predict(m2,d2)[ci$confint[2]+1]

ti <-  # time of the breakpoint
y1 <- coef(m2)[1]+(timing[2]*coef(m2)[3])
y2 <- coef(m2)[2]+(timing[2]*coef(m2)[4])
y2-y1

## determine the magnitude of the breakpoints
Vt.nrbp <- length(bp.Vt$breakpoints)
co <- coef(fm1) # final fitted trend model
Mag <- matrix(NA,Vt.nrbp,3)
for (r in 1:Vt.nrbp) 
{
	if (r==1) 
		y1 <- co[1]+co[r+Vt.nrbp+1]*ti[Vt.bp[r]]
	else 
		y1 <- co[1]+co[r]+co[r+Vt.nrbp+1]*ti[Vt.bp[r]]
	y2 <- (co[1]+co[r+1])+co[r+Vt.nrbp+2]*ti[Vt.bp[r]+1]
	Mag[r,1] <- y1
	Mag[r,2] <- y2
	Mag[r,3] <- y2-y1   
}
index <- which.max(abs(Mag[,3]))
m.x <- rep(Vt.bp[index],2)
m.y <- c(Mag[index,1],Mag[index,2]) #Magnitude position
Magnitude <- Mag[index,3] # Magnitude of biggest change
Time <- Vt.bp[index]

## how to extract time of the break
## magnitude of the change


## BFAST - 1 iteration - based on bfastpp
## first remove seasonality and look at trend breaks
d <- bfastpp(ts.CS, order=3)  ## stl does not work since there are NA's

## (1) fit an harmonic model without breaks and remove the seasonality
m <- lm(response ~ harmon, data = d)
plot(ts.CS, type = "p", col = "gray")
lines(fitted(m) ~ time, data = d, col=1, type='p', pch=19, cex=0.5) # model with breakpoints

## First look at trend breaks
bp1 <- breakpoints(response ~ trend, data = d)
d$seg <- breakfactor(bp1)
m2 <- lm(response ~ seg/(trend), data = d) # model with breakpoints

## remove trend van data
d$detrend <- d$response-trend  ## fitted(m2) is piecewise linear model

## fit seasonal signal
bp3 <- breakpoints(detrend ~ harmon, data = d)
bp3 ## geen seasonal breaks!
m3 <- lm(detrend ~ harmon, data = d) # model with breakpoints

## plot the results
plot(response ~ time, data = d, type = "p", col = "gray")
points(fitted(m2) ~ time, data = d, lwd = 2, col = 1, pch=19, cex=0.2) ## trend 
points((fitted(m2) + fitted(m3)) ~ time, data = d, lwd = 2, col = 'darkgreen', pch=19, cex=0.2) ## trend + season

## predict?


