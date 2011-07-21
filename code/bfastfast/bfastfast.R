### BFASTFAST
### By: Jan Verbesselt
##########
library("compiler")
library("strucchange")
breakpoints <- cmpfun(breakpoints)
bfast2  <- function (Yt, h = 0.15, season = c("harmonic", "none"), 
    max.iter = 1, breaks = NULL, hpc = "none") 
{
    season <- match.arg(season)
    ti <- time(Yt)
    f <- frequency(Yt)
    if (class(harvest) != "ts") 
        stop("Not a time series object")
    output <- list()
    St <- stl(Yt, "periodic")$time.series[, "seasonal"]
    Tt <- 0
    if (season == "harmonic") {
        w <- 1/f
        tl <- 1:length(Yt)
        co <- cos(2 * pi * tl * w)
        si <- sin(2 * pi * tl * w)
        co2 <- cos(2 * pi * tl * w * 2)
        si2 <- sin(2 * pi * tl * w * 2)
        co3 <- cos(2 * pi * tl * w * 3)
        si3 <- sin(2 * pi * tl * w * 3)
        smod <- Wt ~ co + si + co2 + si2 + co3 + si3
    }
#     else if (season == "dummy") {
#         D <- seasonaldummy(Yt)
#         D[rowSums(D) == 0, ] <- -1
#         smod <- Wt ~ -1 + D
#     }
    else if (season == "none") {
    }
    else stop("Not a correct seasonal model is selected ('harmonic' or 'none') ")
    
    Vt.bp <- 0
    Wt.bp <- 0
    CheckTimeTt <- 1
    CheckTimeSt <- 1
    i <- 0
    while ((!identical(CheckTimeTt, Vt.bp) | !identical(CheckTimeSt, 
        Wt.bp)) & i < max.iter) {
        CheckTimeTt <- Vt.bp
        CheckTimeSt <- Wt.bp
        Vt <- Yt - St
        bp.Vt <- breakpoints(Vt ~ ti, h = h, breaks = breaks, 
            hpc = hpc)
        nobp.Vt <- is.na(breakpoints(bp.Vt)[1])
        if (nobp.Vt) {
            fm0 <- rlm(Vt ~ ti)
            Vt.bp <- 0
            Tt <- ts(fitted(fm0))
            tsp(Tt) <- tsp(Yt)
            ci.Vt <- NA
        }
        else {
            fm1 <- lm(Vt ~ breakfactor(bp.Vt)/ti)
            ci.Vt <- confint(bp.Vt, het.err = FALSE)
            Vt.bp <- ci.Vt$confint[, 2]
            Tt <- ts(fitted(fm1))
            tsp(Tt) <- tsp(Yt)
        }
        if (season == "none") {
            Wt <- 0
            St <- 0
            bp.Wt <- NA
            ci.Wt <- NA
            nobp.Wt <- TRUE
        }
        else {
            Wt <- Yt - Tt
# no breakpoint detection in the seasonal term
#             bp.Wt <- breakpoints(smod, h = h, breaks = breaks, 
#                 hpc = hpc)
#             nobp.Wt <- is.na(breakpoints(bp.Wt)[1])
            bp.Wt <- NA
            nobp.Wt <- TRUE  # no seasonal breaks
            if (nobp.Wt) {
                sm0 <- lm(smod)
                St <- ts(fitted(sm0))
                tsp(St) <- tsp(Yt)
                Wt.bp <- 0
                ci.Wt <- NA
            }
#             else {
#                 if (season == "dummy") 
#                   sm1 <- rlm(Wt ~ -1 + D %in% breakfactor(bp.Wt))
#                 if (season == "harmonic") 
#                   sm1 <- rlm(Wt ~ (co + si + co2 + si2 + co3 + 
#                     si3) %in% breakfactor(bp.Wt))
#                 St <- ts(fitted(sm1))
#                 tsp(St) <- tsp(Yt)
#                 ci.Wt <- confint(bp.Wt, het.err = FALSE)
#                 Wt.bp <- ci.Wt$confint[, 2]
#             }
        }
        i <- i + 1
        output[[i]] <- list(Tt = Tt, St = St, Nt = Yt - Tt - 
            St, Vt = Vt, bp.Vt = bp.Vt, Vt.bp = Vt.bp, ci.Vt = ci.Vt, 
            Wt = Wt, bp.Wt = bp.Wt, Wt.bp = Wt.bp, ci.Wt = ci.Wt)
    }
    if (!nobp.Vt) {
        Vt.nrbp <- length(bp.Vt$breakpoints)
        co <- coef(fm1)
        Mag <- matrix(NA, Vt.nrbp, 3)
        for (r in 1:Vt.nrbp) {
            if (r == 1) 
                y1 <- co[1] + co[r + Vt.nrbp + 1] * ti[Vt.bp[r]]
            else y1 <- co[1] + co[r] + co[r + Vt.nrbp + 1] * 
                ti[Vt.bp[r]]
            y2 <- (co[1] + co[r + 1]) + co[r + Vt.nrbp + 2] * 
                ti[Vt.bp[r] + 1]
            Mag[r, 1] <- y1
            Mag[r, 2] <- y2
            Mag[r, 3] <- y2 - y1
        }
        index <- which.max(abs(Mag[, 3]))
        m.x <- rep(Vt.bp[index], 2)
        m.y <- c(Mag[index, 1], Mag[index, 2])
        Magnitude <- Mag[index, 3]
        Time <- Vt.bp[index]
    }
    else {
        m.x <- NA
        m.y <- NA
        Magnitude <- 0
        Time <- NA
        Mag <- 0
    }
    return(structure(list(Yt = Yt, output = output, nobp = list(Vt = nobp.Vt, 
        Wt = nobp.Wt), Magnitude = Magnitude, Mags = Mag, Time = Time, 
        jump = list(x = ti[m.x], y = m.y)), class = "bfast"))
}
            
###
plot.bfast <- function (x, type = c("components", "all", "data", "seasonal", 
    "trend", "noise"), sim = NULL, largest = FALSE, main, ANOVA = FALSE, ...) 
{
    type <- match.arg(type)
    realdata <- is.null(sim)
    Trend.bp <- !x$nobp$Vt
    if (type == "largest" & !Trend.bp) 
        stop("No trend breakpoints")
    title <- !missing(main)
    niter <- length(x$output)
    out <- x$output[[niter]]
    Tt <- out$Tt
    St <- out$St
    noise <- out$Nt
    if (type == "data") {
        if (!title) 
            main <- "Yt"
        plot(x$Yt, main = main, ...)
    }
    else if (type == "components") {
        ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
        tsp(ft) <- tsp(x$Yt)
        ft <- list(time.series = ft)
        if (!title) 
            main <- paste("no. iterations to estimate breakpoints:", 
                niter)
        # fit = x passes the BFAST object to seasonal() for ANOVA
        if (ANOVA == TRUE) { seasonal(ft, out, sim = sim, main = main, fit = x) }
        else { seasonal(ft, out, sim = sim, main = main) }
    }
    else if (type == "noise") {
        require(forecast)
        if (!title) 
            main <- "Noise component"
        tsdisplay(noise, main = main, ...)
    }
    else {
        if (type == "all") {
            idx <- 1:niter
            opar <- par(mfrow = c(2, niter))
        }
        else idx <- niter
        for (i in idx) {
            out <- x$output[[i]]
            if (type != "seasonal") {
                if (type == "trend" & !title) 
                  main <- "Trend component"
                else if (!title) 
                  main <- paste("Iteration ", i, ": Trend", sep = "")
                plot(out$Vt, main = main, ylab = "Vt", ...)
                lines(out$Tt, col = 4)
                if (Trend.bp) {
                  lines(out$bp.Vt)
                  lines(out$ci.Vt)
                  legend("topright", paste("Time of BP(s)", paste(out$Vt.bp, 
                    collapse = ",")), col = 2)
                }
                if (!realdata) {
                  lines(sim$time.series[, "abrupt"], col = 1, 
                    lty = 2)
                  legend("bottomleft", c("estimated", "simulated"), 
                    lty = c(1, 2), col = 1)
                }
                if (largest) {
                  legend("bottomright", c("Magnitude of most sign change"), 
                    lty = c(1), col = 6)
                  lines(x$jump, col = 6)
                  points(x$jump, pch = 14, cex = 1, col = 6)
                }
            }
            if (type != "trend") {
                if (type == "seasonal" & !title) 
                  main <- "Seasonal component"
                else if (!title) 
                  main <- paste("Iteration ", i, ": Seasonal", 
                    sep = "")
                plot(out$Wt, main = main, ylab = "Wt", ...)
                lines(out$St, col = 2)
                Seas.bp <- !x$nobp$Wt
                if (Seas.bp) {
                  lines(out$bp.Wt)
                  lines(out$ci.Wt)
                  legend("topright", paste("Time of BP(s)", paste(out$Wt.bp, 
                    collapse = ",")), col = 2)
                }
                if (!realdata) {
                  lines(sim$time.series[, "seasonal"], col = 1, 
                    lty = 2)
                  legend("bottomleft", c("first run seasonality", 
                    "first run estimated", "simulated"), lty = c(1, 
                    1, 2), col = c(1, 2, 1))
                }
            }
        }
        if (type == "all") 
            par(opar)
    }
}

bfast2 <- cmpfun(bfast2)
### 
seasonal <- function (x, out, sim = NULL, labels = colnames(X), 
set.pars = list(tck = -0.01, mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0)), 
main = NULL, range.bars = FALSE, ..., col.range = "light gray", fit = NULL) 
{
     # define plot layout
     # notice: mfrow parameter removed from set.pars = list()
    layout(matrix(c(1,2,3,4),4,1,byrow=TRUE), heights = c(1,0.75,1.5,0.75), TRUE)        
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    #colnames(X) <- c("data", colnames(sers))
    colnames(X) <- c("Yt","St","Tt", "et")
    nplot <- ncomp + 1
    if (range.bars) 
        mx <- min(apply(rx <- apply(X, 2, range), 2, diff))
    if (length(set.pars)) {
        oldpar <- do.call("par", as.list(names(set.pars)))
        on.exit(par(oldpar))
        do.call("par", set.pars)
#         print("set old par on exit")
    }
    ## ANOVA
    if (is.null(fit) == F) {
      niter <- length(fit$output) # nr of iterations
      out <- fit$output[[niter]]  # output of results of the final fitted seasonal and trend models and nr of breakpoints in both.
      out_ANOVA <- array()
      out_breakdates <- array()
## line below is updated - JV - was causing problems
    if (out$Vt.bp[1] > 0) {breaks <- length(out$Vt.bp) } else {breaks <- 0}  # number of breaks
    if (breaks > 0) {
      breakdates <- out$Vt.bp # breakdates
      coefs <- coef(out$bp.Vt) # output coefficients per segment  
      sl <- coefs[,2] # slopes
   }
   
   TS_anova <- fit$Yt - out$St   # time series Yt - St for ANOVA test
   dataframe <- data.frame(TIME=c(1:length(fit$Yt)),DATA=TS_anova)

   # determine segment startpoint and endpoint, calculate ANOVA
   for (m in 1:(breaks+1)) {
     startpoint <- if(m==1)  1 else breakdates[[m-1]]
     endpoint <- if(m==(breaks+1)) length(fit$Yt) else breakdates[m]-1
     df2 <- dataframe[startpoint:endpoint,]  # subset of dataframe (section)
     model <- lm(DATA~TIME, data=df2)        # linear model
     modelAnova <- anova(model)              # ANOVA
     out_ANOVA[m] <- modelAnova$Pr[1]        # save p-value  
     if(breaks==0) {sl <- model$coefficients[2]}  ## JV updated -- this was causing problems !# slope Tt if breaks == 0
   }
}
## end ANOVA
        
    for (i in 1:nplot) {

        if(i == 4) {
          par(mar = c(0, 6, 0, 6))
        } 
        
        plot(X[, i], col = if(i == 1) "black"
           else "red",
          ylim = if (i == 1 | i == 3) 
            range(X[, 1])
        else range(X[, i], sim$time.series[, i - 1], na.rm = TRUE), 
            type = if (i < nplot) 
                "l"
            else "h", xlab = "", ylab = "", axes = FALSE, ...)
            
        if (range.bars) {
            dx <- 1/64 * diff(ux <- par("usr")[1:2])
            y <- mean(rx[, i])
            rect(ux[2] - dx, y + mx/2, ux[2] - 0.4 * dx, y - 
                mx/2, col = col.range, xpd = TRUE)
        }
        if (i == 1 && !is.null(main)) {
            #title(main, line = 2, outer = par("oma")[3] > 0)
            mtext(main,side=3,font=2,line=1.25,cex=1.1)
            #lines(X[, i], col = "black", type = "l")
            if (!is.null(sim)) {
                lines(X[, i + 1] + X[, i + 2], col = "red", type = "l")
                legend("bottom", c("input", "estimated seasonal + trend "), 
                  col = c("black", "red"), lty = 1)
            }
        }
        if (i == 2) {
            lines(sim$time.series[, "seasonal"], col = "black")
            lines(out$bp.Wt)
            lines(out$ci.Wt)
        }
        if (i == 3) {
            lines(sim$time.series[, "abrupt"], col = "black")
            lines(out$bp.Vt)
            lines(out$ci.Vt)
            
            ## plot ANOVA
            if(is.null(fit) == FALSE) {
              for(m in 1:(breaks+1)) {
                # coordinates based on start time series and breakpoints
                x_coor <-  out$bp.Vt$datatsp[[1]] ## V instead of W
                if(m > 1) { x_coor <- x_coor + breakdates[[m-1]] / frequency(fit$Yt) }
                y_range <- range(X[, 1])
                y_sl <- y_range[2] - (y_range[2] - y_range[1]) / 10 # 10% from top
                y_Pr <- y_range[2] - (y_range[2] - y_range[1]) / 5  # 20% from top
                # print slope
                beta <- formatC(sl[m],format="f",digits=3)
                text(x_coor,y_sl, bquote(beta == .(beta)), pos=4)  
                # print p-value
                Pr <- formatC(out_ANOVA[m],format="f",digits=3)
                text(x_coor,y_Pr, bquote(p == .(Pr)), pos=4)  
              }
            }
            ## end plot ANOVA
        }
        if (i == nplot) {
            abline(h = 0)
            lines(sim$time.series[, "remainder"], col = "black")
        }
        box()
        right <- i%%2 == 0
        axis(2, labels = !right)
        axis(4, labels = right)
        axis(1, labels = i == nplot)
        mtext(labels[i], side = 2, 3)
    }
    mtext("Time", side = 1, line = 3)
    invisible()
    #
    if (is.null(fit) == FALSE) { return(data.frame(slope = sl, prob = out_ANOVA))}
    #if (is.null(fit) == FALSE) { return(sl)}
    # reset plot layout
    layout(matrix(1))
}

            
