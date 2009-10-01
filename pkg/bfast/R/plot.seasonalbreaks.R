plot.seasonalbreaks <-
function(x,realdata=TRUE,sim=NULL,type=c("data","final","components","acf","largest","all","extra"),title=TRUE, ...)
{                                                                                                      
    type <- match.arg(type)
    
    # PLOT ORIGINAL DATA
    if(type=="data")
        plot(x$Yt,main="Yt")
    niter <- length(x$output)
    if(type=="final")
        idx <- niter
    else if(type=="all")
        idx <- 1:niter
    if(type=="components" | type=="all")
    {
        if(type=="components") # adjusted!
            opar <- par(mfrow=c(2,1))
        else if(type=="all")
            opar <- par(mfrow=c(2,niter))
        for(i in idx)
        {
            out <- x$output[[i]]
            
            # PLOT TREND COMPONENT
            plot(out$Vt)
            lines(out$Tt, col = 4)
            Trend.bp <- !x$nobp$Vt
            if (Trend.bp)
            {
                title(paste("iteration_",i,"_YES_struc_trend.pdf",sep=''))
                lines(out$bp.Vt)
                lines(out$ci.Vt)
                legend("topright",paste("Time of BP(s)",paste(out$Vt.bp,collapse=",")),col=2)
            }
            else
                title(paste("iteration_",i,"_NO_struc_trend.pdf",sep=''))
            if (!realdata)
            {
                lines(sim$time.series[,'abrupt'], col=1, lty=2) # orginal data
                legend("bottomleft",c("estimated","simulated"), lty=c(1,2),col=1)
            }
            
            # PLOT SEASONAL COMPONENT
            plot(out$Wt)
            lines(out$St,col=2)
            Seas.bp <- !x$nobp$Wt
            if(Seas.bp)
            {
                title(paste("iteration_",i,"_YES_struc_season",sep=''))
                lines(out$bp.Wt)
                lines(out$ci.Wt)
                legend("topright",paste("Time of BP(s)",paste(out$Wt.bp,collapse=",")),col=2)
            }
            else
                title(paste("iteration_",i,"_NO_struc_season.pdf",sep=''))
            if (!realdata)
            {
                lines(sim$time.series[,'seasonal'], col=1, lty=2) # orginal data
                legend("bottomleft",c("first run seasonality","first run estimated","simulated"), lty=c(1,1,2),col=c(1,2,1))
            }  
        }
        par(opar)
    }
                         
    out <- x$output[[niter]]

    Tt <- out$Tt
    St <- out$St
    noise <- out$Nt

    if(type=="components" | type=="all" | type=="final")
    {
        ft  <- cbind(seasonal= out$St, trend=out$Tt,remainder=out$Nt)
        tsp(ft) <- tsp(x$Yt)
        ft <- list(time.series = ft)
        if(title==TRUE) main <- paste('no. iterations to estimate stable breakpoints:',niter) else main=''
        if (realdata)
        {
          seasonal(ft,out,sim=NULL,main=main) # plotting function based on STL structure. How can we use e.g. plot.seasonal ?
        }
        if (!realdata)
        {
        seasonal(ft,out,sim=sim,main=main)
        }      
    }
    
    if(type=="acf" | type=="all")
        acf(noise) # autocorrelation plot of noise component
    
    if(type=="largest" | type=="all")
    {
        if (Trend.bp)
        {
            plot(out$Vt)
            title(paste("iteration_",niter,"_YES_struc_trend_MostSignificant Change",sep=''))
            lines(out$Tt, col = 4)
            lines(out$bp.Vt)
            lines(out$ci.Vt)
            if (!realdata) 
            {  
               lines(sim$time.series[,'abrupt'], col=1, lty=2) # orginal data
                legend("bottomleft",c("estimated","simulated"), lty=c(1,2),col=c(4,1))
            }
            legend("bottomright",c("Magnitude of most sign change"), lty=c(1),col=6)
            lines(x$jump,col=6)
            points(x$jump, pch=14, cex=1,col=6)
        }
    }
    if(type=="extra")
    {    
    plot(out$Vt)
    lines(out$Tt, col = 2)
    lines(out$bp.Vt)
    lines(out$ci.Vt)
    }
}

