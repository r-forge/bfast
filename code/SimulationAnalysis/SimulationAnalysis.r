rm(list=ls())

# Packages Needed
require(monash)

#Analysis of Simulation Results
getwd()
setwd(c('../output1'))

name <- c("outputsim_")
iter <- 1000
p <- 1
for (p in 1:iter) 
{
  fname <- paste(name,p,".csv",sep="")
  if (p==1) 
  {
    input <- read.csv(fname,header=T)
    total <- input
  } else { 
    input <- read.csv(fname,header=T)
    total <- rbind(total,input)
  }
}
  
#  names(total) <- c('a','sdnoisef','nrange','dip','adelta','c1delta','SimTSb','simdSOS','EstSOS','EstA','EstNrSb','EstTimeSb')
#  names(total) <- c('teller','a','sdnoisef','nrange','dip','adelta','c1delta','SimNrSb','SimTSb','simdSOS','EstSOS','EstA','EstNrSb','EstTimeSb')
dim(total)
names(total)
head(total)
total$Tmon
  # amplitude
  # sd noise =0.1* a factor 1:6
  # adelta    difference in amplitude
  # c1delta     difference in c1 coefficient of the Guassian function
  
  # USE ONLY THE ONCE BELOW FOR RMSE ESTIMATION!!!!!
  # dA        difference between simulated and estimated amplitude
  # dshift    difference between simulated and estimated shift
  # dnrb      difference between simulated and estimated nr of breaks
  # dT        difference between sim and est time of break

# tot <- data.frame(a,sdn=sdnoisef,adelta,simdSOS,nrange,dnrb=SimNrSb-EstNrSb,dT=SimTSb-EstTimeSb)
tot <- data.frame(total,dT=total$Dsim-total$Dmon) ## dT1=total$Tsim-total$Tmon  # here is definitely something wrong with Tsim
names(tot)
tot$dip

# aggregate the data 
levels(factor(tot$a))
levels(factor(tot$noisef))
levels(factor(tot$dip))
levels(factor(tot$nrdatamonitor))

# we do not validate the accuracy of the stability method as this is based on a proven methodology
# the period might be a little longer than expected but ok

# only derive RMSE when n>0.5*iter    
#  Agg <- aggregate(tot, by=list(tot$a,tot$sdn,tot$adelta,tot$simdSOS),FUN = 
#    function(x){
#        if (length(which(!is.na(x))> iter/2)) sqrt(mean(x^2, na.rm=TRUE)) else x <- NA
#        } 
#    )
tot$dip
Agg <- aggregate(tot, by=list(tot$a,tot$noisef,tot$dip,tot$nrdatamonitor),FUN = 
  function(x){
      sqrt(mean(x^2, na.rm=TRUE))
      } 
)
dim(Agg)
head(Agg)

Ndata <- aggregate(tot, by=list(tot$a,tot$noisef,tot$dip,tot$nrdatamonitor),FUN = function(x){ length(which(!is.na(x)))} )   
# function to derive the amount of measurements
dim(Ndata)
head(Ndata)

Agg$Ndata <- Ndata$dT
names(Agg) 

Agg$sn <- Agg$a/Agg$nrange

#Agg$a <- as.factor(paste("a =", Agg$a))
Agg$dip <- as.factor(paste("dip = -", Agg$dip))
names(Agg);head(Agg)

require(lattice)
#lattice.options(default.theme = canonical.theme(color = TRUE))  
    
sp <- list(superpose.symbol = list(pch =c(18), cex = 0.4, col=c(1,2,3,4,5,6)),   # c(1,2,3,4)
             superpose.line = list(col = c(1,2,3,4,5,6), lty = c(1,2,4,6,7,8)) #lty = c(1,2,4,6)
             )

require(monash)

savepng(paste("RMSE_Time_",iter,sep=""), height = 16)  
print(
    xyplot(dT ~ (nrange) | dip , data=Agg, subset=((Ndata>iter/2) & (dip != "dip = - 0.1")), #) &(Group.3 >-4)
      groups=~nrdatamonitor,
      aspect="1",
      scales = list(relation="same", alternating=1, tck=c(T,F)),
      layout = c(4,1),
      ylab='RMSE',xlab='Noise',
      panel = function(x, y, type, ...) {
        panel.superpose(x, y, type=c("l"), ...)  
      },
      par.settings = sp,
      auto.key = list(columns = 6, lines = T, points = FALSE, between.columns=0.4
      )
    )
)       
dev.off()

saveeps(paste("NrDetections_Time_",iter,sep=""), height = 16)
print(
    xyplot(Ndata ~ (nrange) | dip , data=Agg, subset=(dip != "dip = - 0.4") & (dip != "dip = - 0.5"), #) &(Group.3 >-4)
      groups=~nrdatamonitor,
      aspect="1",
      scales = list(relation="same", alternating=1, tck=c(T,F)),
      layout = c(4,1),
      ylab='# of detected breaks',xlab='Noise',
      panel = function(x, y, type, ...) {
        panel.superpose(x, y, type=c("l"), ...)  
      },
      par.settings = sp,
      auto.key = list(columns =6, lines = T, points = FALSE, between.columns=0.4
      )
    )
)       
dev.off()

# the amplitude differences of the simulations are also accounted for and do not influence the RMSE of the time difference
# the length of the stable history does not influence immediately the accuracy of the break detection

print(
    xyplot(dT ~ (LStablehistory) | dip , data=Agg, subset=((Ndata>iter/2)& (dip != "dip = -0.1")), #) &(Group.3 >-4)
      groups=~nrdatamonitor,
      aspect="1",
      scales = list(relation="same", alternating=1, tck=c(T,F)),
      #layout = c(3,2),
      ylab='RMSE',xlab='Noise',
      panel = function(x, y, type, ...) {
        panel.superpose(x, y, type=c("l"), ...)  
      },
      par.settings = sp,
      auto.key = list(columns = 4, lines = T, points = FALSE, between.columns=0.4
      )
    )
)
# # saveeps(paste("RMSE_dnr_c1_0_",iter,sep=""),pointsize=12, height= 12) 
# print(
#     xyplot(dnrb ~(sdn*4) |a, data=Agg, subset= ((simdSOS== "Sim shift = 0") ),            #& (Ndata>iter/2)
#       groups=~adelta,
#       aspect="1",
#       scales = list(relation="free",x=list(alternating=1,rot=0)),
#       layout = c(3,1),
#       ylab='RMSE',xlab='Noise',
#       panel = function(x, y, type, ...) {
#         panel.superpose(x, y, type=c("l"), ...)  
#       },
#       par.settings = sp,
#       auto.key = list(columns = 4, lines = T, points = FALSE, between.columns=0.4
#       ,text=expression(paste(Delta,a," = ",0), paste(Delta,a," = ",0.1), paste(Delta,a," = ",0.2),paste(Delta,a," = ",0.3))
#       )
#     )
# ) 
# # dev.off()
# #
# saveeps(paste("RMSE_dT_SimdA_0_",iter,sep=""),pointsize=12, height= 12)  
# print(
#     xyplot(dT ~(sdn*4) |a, data=Agg, subset= ((adelta== "Sim delta A = 0") & (Ndata>iter/2)),    # Magnitude +     (dT<20) & (a!="a = 0.1")
#       groups=~simdSOS,
#       aspect="1",
#       scales = list(relation="free",x=list(alternating=1),rot=0),
#       #layout = c(3,1),
#       ylab='RMSE',xlab='Noise',
#       panel = function(x, y, type, ...) {
#         panel.superpose(x, y, type=c("l"), ...)  
#       },
#       par.settings = sp,
#       auto.key = list(columns = 2, lines = T, points = FALSE,between=0.5,between.columns=0.5,
#       text=expression(paste(symbol(D),"SOS = ",0), paste(Delta,"SOS = ",1.9), paste(Delta,"SOS = ",3.2),paste(Delta,"SOS = ",4.3))
#       )
#     )
# )       
# dev.off()
# 
# 
# Agg$Time <- Agg$dT
# Agg$Shift <- Agg$dshift
# 
# #saveeps(paste("RMSE_dT_dSOS_SimdA_0_a_03_",iter,sep=""),pointsize=12, height= 12) 
# #print(
# #    xyplot(Time+Shift ~(sdn*4) |a, data=Agg, subset= (adelta== "Sim delta A = 0" & Ndata>iter/2 & a== "a = 0.3"),   # Magnitude + 
# #      groups=~simdSOS,
# #      aspect="1",las=1,
# #      scales = list(relation="free",rot=0),
# #      layout = c(2,1),
# #      ylab='RMSE',xlab='Noise',
# #      panel = function(x, y, type, ...) {
# #        panel.superpose(x, y, type=c("l"), ...)  
# #      },
# #      par.settings = sp,
# #      auto.key = list(columns = 2, lines = T, points = FALSE,between.columns=0.5,
# #        text=expression(paste(Delta,c[1]," = ",0), paste(Delta,c[1]," = ",10), paste(Delta,c[1]," = ",20),paste(Delta,c[1]," = ",30))
# #        )
# #    )
# #)
# #dev.off()
# 
# saveeps(paste("RMSE_dT_SimdA_0_",iter,sep=""),pointsize=12, height= 12) 
# print(
#     xyplot(Time*16 ~(sdn*4) |a, data=Agg, subset= (adelta== "Sim delta A = 0" & Ndata>iter/2),   # Magnitude +   
#       groups=~simdSOS,
#       aspect="1",las=1,
#       scales = list(relation="same",rot=0, alternating=1,tck=c(T,F)),
#       #layout = c(2,1),
#       ylab='RMSE',xlab='Noise',
#       panel = function(x, y, type, ...) {
#         panel.superpose(x, y, type=c("l"), ...)  
#       },
#       par.settings = sp,
#       auto.key = list(columns = 2, lines = T, points = FALSE,between.columns=0.5,
#         text=expression(paste(Delta,c[1]," = ",0), paste(Delta,c[1]," = ",10), paste(Delta,c[1]," = ",20),paste(Delta,c[1]," = ",30))
#         )
#     )
# )
# dev.off()
# 
# saveeps(paste("RMSE_dT_Sim_c_0_",iter,sep=""),pointsize=12, height= 12)
# print( 
#     xyplot(Time ~(sdn*4) |a, data=Agg, subset= ((simdSOS== "Sim shift = 0") & Ndata>iter/2 ),   # Magnitude +   & a== "a = 0.3"
#       groups=~adelta,
#       aspect="1",las=1,
#       scales = list(relation="free",rot=0),
#       #layout = c(2,1),
#       ylab='RMSE',xlab='Noise',
#       panel = function(x, y, type, ...) {
#         panel.superpose(x, y, type=c("l"), ...)  
#       },
#       par.settings = sp,
#       auto.key = list(columns = 2, lines = T, points = FALSE,between.columns=0.5,
#         text=expression(paste(Delta,a," = ",0), paste(Delta,a," = ",0.1), paste(Delta,a," = ",0.2),paste(Delta,a," = ",0.3))
#         )
#     )
# )
# dev.off()
#     
# #par(mfrow=c(1,2))
# #print(
# #plot1,more=T)
# #print(plot2) 
# 
# ##dev.off()
# #
# #
# #
# #   xyplot(Time ~(sdn*4) |a, data=Agg, subset= (Ndata>iter/2 & a== "a = 0.3"),   # Magnitude +   & a== "a = 0.3"
# #      groups=~(simdSOS+adelta),
# #      aspect="1",las=1,
# #      scales = list(relation="free",rot=0),
# #      #layout = c(2,1),
# #      ylab='RMSE',xlab='Noise',
# #      panel = function(x, y, type, ...) {
# #        panel.superpose(x, y, type=c("l"), ...)  
# #      },
# #      par.settings = sp,
# #      auto.key = list(columns = 2, lines = T, points = FALSE,between.columns=0.5,
# #        text=expression(paste(Delta,c[1]," = ",0), paste(Delta,c[1]," = ",10), paste(Delta,c[1]," = ",20),paste(Delta,c[1]," = ",30))
# #        )
# #    )
#     