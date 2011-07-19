# ================================================
title <- "BFAST analysis on modeled Time Series"
#
# Wageningen University, Centre for Geo-Information
# Jan 2011: Rogier de Jong, Jan Verbesselt
#
# ================================================


#-----------------------------------------------
# Configuration
#-----------------------------------------------

# workdir <- "E://R-workspace//HPC-BFAST//"

## install and load required packages
#   required = c('bfast') 
#   installed = required %in% installed.packages()[, 'Package']
#   if (length(required[!installed]) >=1) {
#     install.packages(required[!installed]) 
#   }
  require(bfast)
#   setwd(workdir)

library(doMC) # High Performance Computing
registerDoMC(4)


#-----------------------------------------------
# Simulate time series (10 years)
#-----------------------------------------------

  T_array <- c(1:240)
  St_mod <- cos((T_array - 7) / 24 * 2 * pi) / 4     # cosine St amplitude is 0.25
  Et_mod <- rnorm(length(T_array), 0, 0.045)         # random error, StDev = 0.045
    Tt_1 <- c(1:75) / 75 / 50 + 0.55
    Tt_2 <- c(1:105) / 115 / 7 * -1 + 0.56
    Tt_3 <- c(1:60) / 60 / 6 + 0.49
  Tt_mod <- c(Tt_1,Tt_2,Tt_3)                        # Tt is combination of 4 segments
  #Tt_mod <- rep(0.5,length(T_array))                # Tt is invariant
  Yt_mod <- St_mod + Et_mod + Tt_mod                 # add all components to a modeled signal
  TS_mod <- ts(Yt_mod,start=c(2000,1),frequency=24)     # create time series
  plot(TS_mod, type="l")
    
  
#-----------------------------------------------
# Trend break analysis
#-----------------------------------------------
 
  # BFAST: h = 0.15 (default); max.iter = NULL (default)
?bfast

print(system.time(fit1 <- bfast(TS_mod,season="harmonic", h = 0.15, max.iter = 1)))
print(system.time(fit2 <- bfast(TS_mod,season="harmonic", h = 0.15, max.iter = 1, hpc="foreach")))
print(system.time(fit3 <- bfast2(TS_mod,season="harmonic", h = 0.15, hpc="foreach")))
plot(fit1, ANOVA=TRUE)
plot(fit2, ANOVA=TRUE) 
plot(fit3)
plot(fit3, ANOVA=TRUE)
?bfast
# remove all objects
#rm(list=ls(all=TRUE))
# ================END SCRIPT====================  