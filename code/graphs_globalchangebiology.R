						## History plot for an example time series
              m <- lm(Nile ~ 1)
 						 omos <- efp(Nile ~ 1, type = "OLS-MOSUM", h = 0.25)
						  z <- merge(
						    y = as.zoo(Nile),
						    fit = zoo(fitted(m), as.vector(time(Nile))),
						    MOSUM = as.zoo(omos$process),
						    boundary1 = as.zoo(boundary(omos)),
						    boundary2 = -as.zoo(boundary(omos)))
						  plot(z, screen = c(1, 1, 2, 2, 2), col = c(1, 4, 1, 2, 2))

head(test_tspp)
            
          ## 
            tshistory  
          	test_tspp <- tspp(tshistory , order = 3)
						test_lm <- lm(response ~ trend + harmon, data = test_tspp)
						omos <- efp(response ~ trend + harmon, data = test_tspp,type = "OLS-MOSUM", h = 0.25)    
							plot(test_tspp$response)
              z <- merge(
							  NDVI = as.zoo(tshistory),
						    fit = zoo(fitted(test_lm),as.vector(test_tspp$time)),
						    MOSUM = as.zoo(omos$process),
						    boundary1 = as.zoo(boundary(omos)),
						    boundary2 = -as.zoo(boundary(omos))
							)
						plot(z, screen = c(1, 1, 2, 2, 2), col = c(1, 4, 1, 2, 2))
              
 					## is this correct???
						z <- merge(
							  NDVI = zoo(test_tspp$response,test_tspp$time),
						    fit = zoo(fitted(test_lm),test_tspp$time),
						    MOSUM = zoo(omos$process,test_tspp$time),
						    boundary1 = zoo(boundary(omos),test_tspp$time),
						    boundary2 = -zoo(boundary(omos),test_tspp$time)
							)
						plot(z, screen = c(1, 1, 2, 2, 2), col = c(1, 4, 1, 2, 2))
              
              
            									
            									
            									
            									
            									
 
              
  require(ggplot2)
	tsmon <- ts.union(test_pred,monitor,tshistory)
	tsmon
              			 
df <- data.frame(NDVI=tsmon[,'tshistory'], Time=time(tsmon), model=tsmon[,'test_pred'], mon=tsmon[,'monitor'])
p =	ggplot(df) + geom_line(aes(x=Time, y=NDVI)) + theme_bw() +
		geom_line(aes(x=Time, y=model), colour="blue",alpha=0.8) +
		geom_line(aes(x=Time, y=mon), colour="red") + xlab("") +
		geom_rect(aes(xmin=(time(monitor)[1]-1/23), xmax=max(Time), ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)
p

p + geom_vline(xintercept = tbp, colour='red')
  
  