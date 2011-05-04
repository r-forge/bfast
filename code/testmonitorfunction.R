## required functions
source("ts_sim_seas_6x00.R")

tshistory <- read.csv("tshistory.csv")
tshistory <- createts(tshistory$x)
plot(tshistory)
test_tspp <- tspp(tshistory, order = 3)

require(strucchange)
print(breakpoints(response ~ trend + harmon, data = test_tspp))
## no breakpoints - are detected within the history period
## so why should we use the roc() function?


subset_start <- roc(tshistory) # searching for a stable period 
print(subset_start)

