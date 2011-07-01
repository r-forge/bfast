# Name: 	Query MODIS data out of the DATABASE
# Editor: 	Jan Verbesselt
##########################################################################
rm(list=ls())
q.MOD13C1data <- function(mysqldb,geoid){
    # --> Define query
    myquery     <- paste(" 
    SELECT c.dagvhjaar as day, c.jaar as year, c.ndvi as NDVI,reliability
 #   ,ndviquality1 as q1, ndviquality2 as q2, ndviquality3 as q3, ndviquality4 as q4
#  ,ndviquality5 as q5, ndviquality6 as q6, ndviquality7 as q7, ndviquality8 as q8, ndviquality9 as q9, ndviquality10 as q10
#  ,ndviquality11 as q11, ndviquality12 as q12, ndviquality13 as q13, ndviquality14 as q14, ndviquality15 as q15, ndviquality16 as q16
FROM modisglobal.mod13c1_data as c
    WHERE  c.geoid=",geoid,"group by c.jaar,c.dagvhjaar"
    )
    # --> Execute query
    result      <- sqlQuery(mysqldb,myquery)
    return(result)
}

q.gooddata <- function(mysqldb){
    # --> Define query
    myquery     <- paste(" 
    SELECT geoid 
      FROM test.mod13c1_stl_stats_range
        WHERE dataquality<=0.10 and rseasonal>=0.1
    ")
    # --> Execute query
    result      <- sqlQuery(mysqldb,myquery)
    return(result)
}

plot.stl <- function(..., xlab = "Time") {

	mtext <- function(text, ...)
                  graphics::mtext(if (text == "time") xlab else text, ...)
	plot.stl <- stats:::plot.stl
	environment(plot.stl) <- environment()
	plot.stl(...)

}

   tsrange <- function(x) {
    ra <- range(x,na.rm=T)
    return(ra[2]-ra[1])
   }

q.geoid <- function(mysqldb,x,y){
    myquery     <- paste("
    SELECT geo_id,sample,line
      FROM modisglobal.mcd43_geoid a
        WHERE abs(a.xcoord_centre-",x,")<=0.025 and abs(a.ycoord_centre-",y,")<=0.025"
    ,sep="")
    # take image offset into account
    #Execute query
    result      <- sqlQuery(mysqldb,myquery)
    return(result)
}

## running BFAST on the simulated seasonal change!
setwd("C:/Local_data/SCRIPT/R/BFAST_SeasonalChangeDetection")
source("Function/functions_print_plot_1x00.r")
source("Function/functions_SINCOS_8x00.r")
#source("Function/functions_SeasDum_6x00.r") # with seasonal dummies
   
#--->________________________________________________
#--->
#--->   Load libraries for R with specific functions
#--->________________________________________________
    require(RODBC) # -->Databank interface
    require(zoo) # Library for the interpolation
    require(Hmisc)
    require(monash)
#--->________________________________________________
#--->
#--->   Set variables
#--->________________________________________________

# check also regulatemodis script!

#--->________________________________________________
#--->
#--->   Connect to the MySQL database
#--->________________________________________________
mysqldb  <- odbcConnect(dsn=c("modis_greenhills"))
mysqltest  <- odbcConnect(dsn=c("test"))
#--->________________________________________________
#--->
#--->   Run analysis
#--->________________________________________________

#allgeoid <- 619440
# query goeid list from processed table!!!
#goodgeoids <- q.gooddata(mysqldb)
dim(goodgeoids)

#for (i in  1:dim(goodgeoids)[1]) {
#i <- 10
#    geoid <- q.geoid(mysqldb,116.357574,-32.036020)    # perth 
#    geoid <- q.geoid(mysqldb,142.489929,-16.417644)    # Cape York    # very nice test series
#geoid <- q.geoid(mysqldb,145.071503,-16.782415); name <-  c("NDVI_BFAST_CapeYork_2007Break_145_16")    # Cape York    Seasonal Break in 2008
geoid <- q.geoid(mysqldb,143.259313,-37.877646); name <-  c("NDVI_BFAST_VIC_2007Break_143_39")

#    geoid <- q.geoid(mysqldb,137.065430,-25.135339)   # Simpson Desert
#    geoid  <- 521720 # fluxtower Tumbarumba
#   geoid <-  27407 # first seasonal break
#    geoid <- goodgeoids[i,1]
    
    # DATA IMPORT
    d    <- q.MOD13C1data (mysqldb,geoid)
    names(d);dim(d)
    
    # DATA CLEANING
    baddata <- d$reliability>0      #| d$q11==1 | d$q9==1
    d$NDVI[baddata] <- NA           # check for reliability and clouds
    
    ndvi <- ts(d$NDVI, start=c(2000,4), frequency=23)   # set as time series  # we have all the images in the stack to we do not need to worry about regularising the data
    plot(ndvi)

    # INTERPOLATION
    iNDVI <- ts(approxExtrap(1:length(ndvi),ndvi,na.rm=F,xout=1:length(ndvi))$y)
    tsp(iNDVI) <- tsp(ndvi) ;plot(iNDVI)
    
    # DATA PROCESSING
    (bdist <- (23)/length(iNDVI))
    system.time(fit <- bfastsin(iNDVI,ht=(bdist*2),hs=(bdist*1),max.iter=1,sbreaks=1 ))  #  ,sbreaks=1   ,sbreaks=1

    name <- paste(name,'_1sbreak',sep='')
    saveeps(name,height = 12,pointsize=12)
    plot(fit,realdata=T,sim=simul,type="final",title=FALSE)  # enhance the input
    dev.off()
    
    # Output
    niter <- length(fit$output)
    out <- fit$output[[niter]]
    # seasonal break yes not
    (seasbreak <- out$Wt.bp)  # seasonal breaks Julian date)
    #out$bp.Wt    #function
    # coefficients
    fit$scoef 
#   
#   if (seasbreak[1]!=0) { 
#    stats<- as.data.frame(t(c(geoid[1],seasbreak[1])))
#    names(stats) <- c("geoid","seasbreak")
#    sqlSave(mysqltest,stats, tablename=c("MOD13C1_BFAST_STATS"), rownames=F,append=TRUE,test=F)
#   }
#
#    #--->________________________________________________
#    #--->
#    #--->   cleaning up memory with the gc() function
#    #--->________________________________________________
#    memory.size() # --> # amount of memory used, before cleaning
#    gc() #- do it now
#    gcinfo(TRUE) #-- in the future, show when R does it
#    gcinfo(verbose = FALSE)#-- don't show it anymore
#    #memory.size() # --> # amount of memory used, after cleaning
#    #print(geoid)
#}
#
##--->________________________________________________
##--->
##--->   Disconnect to the MySQL database
##--->________________________________________________
#odbcClose(mysqldb)
#odbcClose(mysqltest)


