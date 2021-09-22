
## Function to smooth the ACC axes and extract the static acceleration

accSmooth <- function(accAxis, ws){
  staticAcc <- rep(NA, length(accAxis))
  for(i in (ws+1):(length(accAxis)-ws)){
    staticAcc[i] <- mean(accAxis[(i-ws):(i+ws)],na.rm=T)}
  return(staticAcc)
}

# # Example of use:
#===================
# FreqHz=unique(oneTag$acc_samplFreq)
# smoothStepSec=3
# windowSize=round(FreqHz*smoothStepSec/2) #(mean value of 1.5 sec before and after each ACC point)
# accAxis=myaccX #acc axis I want to smooth
# 
# staticX <- accSmooth(myaccX, ws=windowSize)
