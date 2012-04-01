## Example script 
## For a new light version of bfast 
## based on bfastpp

## Jan Verbesselt

require(bfast)

## Super simple BFAST
## without decomposition - just by fitting a seasonal trend model
## this might be ok for general purposes

NDVIb <- as.ts(zoo(som$NDVI.b, som$Time))
plot(NDVIb)
NDVIb
## add random NA's to the time series!!!
which(is.na(NDVIb))

d <- bfastpp(NDVIb, order=3)
bp <- breakpoints(response ~ trend + harmon, data = d)
breakpoints(bp)

d$seg <- breakfactor(bp)
m <- lm(response ~ seg/trend, data = d)
m1 <- lm(response ~ seg/(trend + harmon), data = d) # model with breakpoints

## plot the results
plot(response ~ time, data = d, type = "l", col = "gray")
lines(fitted(m) ~ time, data = d, lwd = 2, col = 1, pch=19, cex=0.2) # without break
lines(fitted(m1) ~ time, data = d, lwd = 2, col = 'darkgreen', pch=19, cex=0.5) # with break

summary(m1)
summary(m)

anova(m,m1)




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


