#library(readxl) #for read_xlsx

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp")

options(digits.secs = 3) #Really important in order not to lose the millisecond ;)
options(digits = 8)

### For each day, import and format the ACC files from the different devices ####
#___________________________________________________________________________________

dateFolders <- grep("2018-06|2018-07",list.dirs("Data_raw_SDcardOlivier_Rocamadour2018", recursive=F), value=T) # all daily folders

for(i in 1:length(dateFolders)){

  dayFolder <- dateFolders[[i]]
  oneDay <- gsub(".+/", "", dayFolder)
  print(oneDay)

  # Axy1
  allAXYs <- list.files(dayFolder, pattern="AXY.*csv", full.names = T)
  allAXYs <- allAXYs[file.size(allAXYs) > 1000000] #Select only files > 1 Mb (1000000 bytes)
  Axy_ls <- lapply(1:length(allAXYs), function(j){
    acc <- read.csv(allAXYs[[j]], as.is=T, na.strings = c("","NA"), sep="\t", header=F)
    colnames(acc) <- c("Date","Time","accX","accY","accZ")
    acc$Timestamp <- as.POSIXct(format(as.POSIXct(paste(acc$Date, acc$Time, sep=" "), format="%d/%m/%Y %H:%M:%OS", tz="UTC"), format="%Y-%m-%d %H:%M:%OS4"), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    acc$Date <- as.POSIXct(acc$Date, format="%d/%m/%Y", tz="UTC")
    fileInfo <- gsub(".+/|.csv", "", allAXYs[[j]])
    acc$fileID <- fileInfo
    if(as.integer(substr(fileInfo,4,5))<10){acc$deviceID <- paste0("AXY1_",substr(fileInfo,5,5))}
    if(as.integer(substr(fileInfo,4,5))==10){acc$deviceID <- paste0("AXY3_",substr(fileInfo,4,5))}
    acc <- acc[,c("fileID","Date","Time","Timestamp","accX","accY","accZ","deviceID")]
    return(acc)
  })
  names(Axy_ls) <- sapply(Axy_ls, "[", 1,"deviceID") #paste(oneDay,sapply(Axy_ls, "[", 1,"deviceID"),sep="_")
  print("AXYs done!")
  
  # AGM
  allAGMs <- list.files(dayFolder, pattern="AGM.*csv", full.names = T)
  Agm_ls <- lapply(1:length(allAGMs), function(j){
    acc <- read.csv(allAGMs[[j]], as.is=T, na.strings = c("","NA"))
    acc <- acc[-nrow(acc),]
    acc$Timestamp <- as.POSIXct(format(as.POSIXct(paste(acc$Date, acc$Time, sep=" "), format="%d/%m/%Y %H:%M:%OS", tz="UTC"), format="%Y-%m-%d %H:%M:%OS4"), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    acc$Date <- as.POSIXct(acc$Date, format="%d/%m/%Y", tz="UTC")
    fileInfo <- gsub(".+/|.csv", "", allAGMs[[j]])
    acc$fileID <- fileInfo
    acc$deviceID <- paste0("AGM_",substr(fileInfo,5,5))
    acc <- acc[,c("fileID","Date","Time","Timestamp","accX","accY","accZ","deviceID")]
    return(acc)
  })
  names(Agm_ls) <- sapply(Agm_ls, "[", 1,"deviceID")
  print("AGMs done!")
  
  # AxiTrek (both acc and gps)
  allAxytreks <- list.files(dayFolder, pattern="Axytrek.*csv|AxyTrek.*csv", full.names = T)
  Axytrek_ls <- lapply(1:length(allAxytreks), function(j){
    gpsAcc <- read.csv(allAxytreks[[j]], as.is=T, na.strings = c("","NA"))
    gpsAcc <- gpsAcc[-nrow(gpsAcc),]
    gpsAcc$Timestamp <- as.POSIXct(format(as.POSIXct(paste(gpsAcc$Date, gpsAcc$Time, sep=" "), format="%d/%m/%Y %H:%M:%OS", tz="UTC"), format="%Y-%m-%d %H:%M:%OS4"), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    gpsAcc$Date <- as.POSIXct(gpsAcc$Date, format="%d/%m/%Y", tz="UTC")
    fileInfo <- gsub(".+/|.csv", "", allAxytreks[[j]])
    gpsAcc$fileID <- fileInfo
    gpsAcc$deviceID <- paste0("AT_",substr(fileInfo,9,9))
    acc <- gpsAcc[,c("fileID","Date","Time","Timestamp","X","Y","Z","deviceID")]
    colnames(acc) <- c("fileID","Date","Time","Timestamp","accX","accY","accZ","deviceID")
    return(acc)
  })
  names(Axytrek_ls) <- sapply(Axytrek_ls, "[", 1,"deviceID")
  print("ATs done!")
  
  # Bind the three acc lists into one, all acc files have now the same format
  allACC_ls <- c(Axy_ls, Agm_ls, Axytrek_ls)
  #names(allACC_ls)
  
  # Save the tag list of formatted acc data in a new folder, by day
  save(allACC_ls, file=paste0("Data_correct/ACC_format/allACCdata_",oneDay,".RData"))
}


### Check matching times at the calibration points (start and end of each day) ####
#___________________________________________________________________________________
# 1. For each Tag, we are going to plot the values with a 40 sec buffer before and after the calibration and look for the calibration waves.
# 2. We make one plot at a time and store the time at which the waves start in the tag using the locator function.
# 3. We use the starting and ending time we find to "cut" the data so that all tags start exactly with the calibration.
# 4. Finally we use the accurate time of the axytrek (AT) and assign the same starting time to all of them.

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct")

# Import the file with infos about the calibration per tag and per day
tagCalibrInfos <- read.csv("Calibration_LoggersStartEnd.csv", as.is=T, na.strings=c("","NA"))

tagCalibrInfos$date <- as.POSIXct(tagCalibrInfos$date, format="%d/%m/%Y", tz="UTC")
tagCalibrInfos$timestamp.calibration.start <- as.POSIXct(paste(tagCalibrInfos$date,tagCalibrInfos$heure.UTC.calibr.start,sep=" "), format="%Y-%m-%d %H:%M:%S", tz="UTC")
tagCalibrInfos$timestamp.calibration.end <- as.POSIXct(paste(tagCalibrInfos$date,tagCalibrInfos$heure.UTC.calibr.end,sep=" "), format="%Y-%m-%d %H:%M:%S", tz="UTC")
calibACC <- tagCalibrInfos[grep(tagCalibrInfos$type.unit, patter="AXY|AGM|AT"),]
# Assign unique deviceID to the calibration data to match the deviceID in the ACC data
calibACC$deviceID <- paste(calibACC$type.unit,calibACC$numero.unit, sep="_")
# And store it
save(calibACC, file="calibrationInfos_onlyACC.RData")

# List the files containing the lists of ACC data per day
ACC_dailyLists <- list.files("ACC_format", full.names = T, pattern="allACCdata")

# Load each i file of the daily ACC lists
i=7
load(ACC_dailyLists[[i]])  # object allACC_ls
# Check that all is correct (Hz, deviceIDs, calibrations)
lapply(allACC_ls, head) 
lapply(allACC_ls, tail)
# Extract tag IDs and date
tagIDs <- names(allACC_ls)
oneDay <- gsub(".+_|.RData","",ACC_dailyLists[[i]])

# Subset the calibration file to the day of interest
calibSub <- calibACC[as.character(calibACC$date) == oneDay,]

# # Check that data are correct
# lapply(allACC_ls, function(acc)rbind(head(acc),tail(acc)))
# lapply(allACC_ls, str)


# 1. Select acc data 40 secs before and after the expected calibration time (from the watch) for comparison ----

# Store the calibration times recorded with the clock in the same order as the tag list
calibrTimes_clock <- lapply(1:length(allACC_ls), function(y){
  times <- c(as.character(calibSub$timestamp.calibration.start[calibSub$deviceID==names(allACC_ls)[y]]),
             as.character(calibSub$timestamp.calibration.end[calibSub$deviceID==names(allACC_ls)[y]]))
  names(times) <- c("start_calib_time", "end_calib_time")
  return(as.POSIXct(times, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
})
names(calibrTimes_clock) <- tagIDs

# Add a column to the ACC data with the calibration time correspoding to each Tag
allACC_ls <- lapply(1:length(allACC_ls), function(y) {
  allACC_ls[[y]]$clock_start_calibr <- calibrTimes_clock[[y]]["start_calib_time"]
  allACC_ls[[y]]$clock_end_calibr <- calibrTimes_clock[[y]]["end_calib_time"]
  return(allACC_ls[[y]])
})
names(allACC_ls) <- tagIDs
lapply(allACC_ls, head) #check that all is correct
#After adding the calibration columns save the ACC file, overwrite the imported one (i file)
save(allACC_ls, file=ACC_dailyLists[[i]])

# Use the calibration times recorded with the clock to subset the ACC data from each tag around that time
ACC_cal_start <- lapply(1:length(allACC_ls), function(y) {
  return(allACC_ls[[y]][which(allACC_ls[[y]]$Timestamp > (calibrTimes_clock[[y]]["start_calib_time"]-60) &
                          allACC_ls[[y]]$Timestamp < (calibrTimes_clock[[y]]["start_calib_time"]+60)),])})
ACC_cal_end <- lapply(1:length(allACC_ls), function(y) {
  return(allACC_ls[[y]][which(allACC_ls[[y]]$Timestamp > (calibrTimes_clock[[y]]["end_calib_time"]-60) &
                          allACC_ls[[y]]$Timestamp < (calibrTimes_clock[[y]]["end_calib_time"]+60)),])})

n <- length(allACC_ls)

# 2a. Plot start calibration, find start calibration time and store it in a list ----

library(zoom)
#X11(type = "Xlib")

lapply(ACC_cal_start, nrow)

range(sapply(ACC_cal_start, "[", , "accX"), na.rm=T)
range(sapply(ACC_cal_start, "[", , "accY"), na.rm=T)
range(sapply(ACC_cal_start, "[", , "accZ"), na.rm=T)

calibrStart_locatedTimes <- list()

w=10
if(nrow(ACC_cal_start[[w]])==0){calibrStart_locatedTimes[[w]] <- NA}

plot(ACC_cal_start[[w]][,"Timestamp"],ACC_cal_start[[w]][,"accX"], ylim=c(-2,3), type="n", xlab="Timestamp",ylab="Acceleration [G]")
abline(v=calibrTimes_clock[[w]]["start_calib_time"], lwd=2)
  lines(ACC_cal_start[[w]][,"Timestamp"],ACC_cal_start[[w]][,"accX"], col="red", lwd=1.5)
  lines(ACC_cal_start[[w]][,"Timestamp"],ACC_cal_start[[w]][,"accY"], col="green", lwd=1.5)
  lines(ACC_cal_start[[w]][,"Timestamp"],ACC_cal_start[[w]][,"accZ"], col="blue", lwd=1.5)
zm()

#dev.set(dev.list()[3]) #activate correct plot window
# Select point in the plot where calibration starts and store the corresponding timestamp in a list
calibrStart_locatedTimes[[w]] <- as.POSIXct(locator(1)$x, origin='1970-01-01', tz="UTC") 
# Assign to the list of calibration times the names of the tags and save them
names(calibrStart_locatedTimes) <- tagIDs
save(calibrStart_locatedTimes, file=paste0("ACC_format/StartCalibration_timesList_",oneDay,".RData"))

# 2b. Plot end calibration, find start calibration time and store it in a list ----

lapply(ACC_cal_end, nrow)

range(lapply(ACC_cal_end, "[", , "accX"), na.rm=T)
range(sapply(ACC_cal_end, "[", , "accY"), na.rm=T)
range(sapply(ACC_cal_end, "[", , "accZ"), na.rm=T)


calibrEnd_locatedTimes <- list()

w=10
if(nrow(ACC_cal_end[[w]])==0){calibrEnd_locatedTimes[[w]] <- NA}

plot(ACC_cal_end[[w]][,"Timestamp"],ACC_cal_end[[w]][,"accX"], ylim=c(-1,3), type="n", xlab="Timestamp",ylab="Acceleration [G]")
abline(v=calibrTimes_clock[[w]]["end_calib_time"], lwd=2)  
  lines(ACC_cal_end[[w]][,"Timestamp"],ACC_cal_end[[w]][,"accX"], col="red", lwd=1.5)
  lines(ACC_cal_end[[w]][,"Timestamp"],ACC_cal_end[[w]][,"accY"], col="green", lwd=1.5)
  lines(ACC_cal_end[[w]][,"Timestamp"],ACC_cal_end[[w]][,"accZ"], col="blue", lwd=1.5)
zm()

#dev.set(dev.list()[3]) #activate correct plot window
# Select point in the plot where calibration starts and store the corresponding timestamp in a list
calibrEnd_locatedTimes[[w]] <- as.POSIXct(locator(1)$x, origin='1970-01-01', tz="UTC")
# Assign to the list of calibration times the names of the tags and save them
names(calibrEnd_locatedTimes) <- tagIDs
save(calibrEnd_locatedTimes, file=paste0("ACC_format/EndCalibration_timesList_",oneDay,".RData"))


# 3. Use the stored times to "cut" the data, so that all start with the calibration ----

options(digits.secs = 2) #Really important in order not to lose the millisecond ;)
options(digits = 8)

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/ACC_format")

# List the files containing the lists of ACC data per day
ACC_dailyLists <- list.files(".", full.names = T, pattern="allACCdata")
# Load all calibration infos for the ACC files
load("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/calibrationInfos_onlyACC.RData")

# Load each i file of the daily ACC lists
i=7
load(ACC_dailyLists[[i]])  # object allACC_ls
# Extract tag IDs and date
tagIDs <- names(allACC_ls)
oneDay <- gsub(".+_|.RData","",ACC_dailyLists[[i]])
# Subset calibration infos for that day
calibSub <- calibACC[as.character(calibACC$date) == oneDay,]

#Load corresponding start and end calibration times (objects calibrStart_locatedTimes and calibrEnd_locatedTimes)
load(list.files(".", pattern=paste0("StartCalibration.+",oneDay), full.names = T))
load(list.files(".", pattern=paste0("EndCalibration.+",oneDay), full.names = T))

# Cut the data to the start and end calibration times
allACC_ls_sub <- allACC_ls
allACC_ls_sub <- lapply(1:length(allACC_ls_sub), function(y){
  if(!is.na(calibrStart_locatedTimes[[y]])){
           allACC_ls_sub[[y]] <- allACC_ls_sub[[y]][which(allACC_ls_sub[[y]]$Timestamp >= calibrStart_locatedTimes[[y]]),]}
  if(!is.na(calibrEnd_locatedTimes[[y]])){
           allACC_ls_sub[[y]] <- allACC_ls_sub[[y]][which(allACC_ls_sub[[y]]$Timestamp <= calibrEnd_locatedTimes[[y]]),]}
  return(allACC_ls_sub[[y]])
})
names(allACC_ls_sub) <- tagIDs

# But, if both calibrations are missing exclude the tag from the list
# allACC_ls_sub[[which(is.na(unlist(calibrStart_locatedTimes)) & is.na(unlist(calibrEnd_locatedTimes)))]] <- NULL

lapply(allACC_ls_sub,head)
lapply(allACC_ls_sub,tail)


# 4. Use the time of the Axytreks to assign the time to AGMs and AXYs tags ----

myModa <- function(x){names(table(x))[which(table(x)==max(table(x)))]}
# check the calibration of al axytrecks (having gps time)
(allATstarts <- calibrStart_locatedTimes[grep("AT", names(calibrStart_locatedTimes))])
(allATends <- calibrEnd_locatedTimes[grep("AT", names(calibrEnd_locatedTimes))])
# Check time lag in the axytreck
ATnrows <- lapply(allACC_ls_sub, nrow)[grep("AT", names(allACC_ls_sub))]
mean(sapply(1:length(ATnrows), function(i){(difftime(allATends[[i]],allATstarts[[i]], unit="secs"))/ATnrows[[i]]}))
# Select the most common among the gps times of the axytrecks
options(digits.secs = 2)
(ATstart <- as.POSIXct(myModa(sapply(allATstarts, as.character)), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
(ATend <- as.POSIXct(myModa(sapply(allATends, as.character)), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
# When the starting times are all different but they differ of milliseconds we take the average
(ATstart <- mean(ATstart))
(ATend <- mean(ATend))
# Now we use this as starting tiem and we re-assign a new timestamp based on that
allACC_ls_newTime <- lapply(allACC_ls_sub, function(acc){
  acc <- acc[complete.cases(acc[,c("accX","accY","accZ")]),]
  #freq <- calibSub$Frequence..Hz.[calibSub$deviceID == unique(acc$deviceID)]
  #acc$NewTimestamp <- seq.POSIXt(from=ATstart, by=1/freq, along.with = acc$Timestamp)
  acc$NewTimestamp <- seq.POSIXt(from=ATstart, by=0.039995898, along.with = acc$Timestamp)
  return(acc)
})
names(allACC_ls_newTime) <- names(allACC_ls_sub)

lapply(allACC_ls_newTime,head)
lapply(allACC_ls_newTime,tail)

#allACC_ls_newTime <- allACC_ls_newTime[-3]
#allACC_ls_newTime[["AT_H"]]$NewTimestamp <- allACC_ls_newTime[["AT_H"]]$Timestamp

save(allACC_ls_newTime, file=paste0("allACCdata_correctTimestamp_",oneDay,".RData"))

#test <- allACC_ls_newTime[["AT_Z"]] # on the 2018-07-25 AGM_5 has extra 4 sec at the end calibration
# difftime(test$NewTimestamp[length(test$NewTimestamp)],test$NewTimestamp[1], units = "secs")/length(test$NewTimestamp)
# difftime(test$Timestamp[length(test$Timestamp)],test$Timestamp[1], units = "secs")/length(test$Timestamp)
#test_diff <- difftime(test$Timestamp[-1], test$Timestamp[-length(test$Timestamp)], units = "secs")
#table(test_diff)
#test[-1,"Timestamp"][test_diff>0.2]

### Now that time is correct we can smooth the ACC data ####
#____________________________________________________________

options(digits.secs = 2) #Really important in order not to lose the millisecond ;)
options(digits = 8)
# source function for running mean
source("/home/mscacco/ownCloud/Martina/PHD/R_functions/ACCsmoothing_staticACC.R")
library(plyr)
library(parallel)
library(doMC)
registerDoMC(detectCores()-1)

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/ACC_format")

# Load all calibration infos to get frequency (Hz) of each device
load("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/calibrationInfos_onlyACC.RData")

# List the files containing the lists of time-corrected ACC data per day
ACC_dailyLists_correct <- list.files(".", full.names = T, pattern="correctTimestamp_2018")

llply(1:length(ACC_dailyLists_correct), .fun=function(i){  # per day
  load(ACC_dailyLists_correct[[i]])  # object allACC_ls_newTime
  # Extract tag IDs and date
  tagIDs <- names(allACC_ls_newTime)
  (oneDay <- gsub(".+_|.RData","",ACC_dailyLists_correct[[i]]))
  # Subset calibration infos for that day
  calibSub <- calibACC[as.character(calibACC$date) == oneDay,]
  
  allACC_newTime_smooth <- llply(allACC_ls_newTime, .fun=function(oneTag){  #per device
    (deviceID <- unique(oneTag$deviceID))
    oneTag$acc_samplFreq <- calibSub$Frequence..Hz.[calibSub$deviceID==deviceID]
    winSize3s <- round((unique(oneTag$acc_samplFreq)*3)/2) #mean value in a winSize of 3 s (75 or 150 points, 1.5 sec before and after each ACC point)
    winSize025s <- round((unique(oneTag$acc_samplFreq)/4)/2) #mean value in a winSize of 0.25 s (~ 6 or 12 points, 1 flap)
    winSize05s <- round((unique(oneTag$acc_samplFreq)/2)/2) #mean value in a winSize of 0.5 s (~ 12 or 24 points, 2 flapping bouts, since we mostly have 4 flaps/sec)
    X_dynamic3 <- oneTag$accX - (accSmooth(oneTag$accX, winSize3s))
    Y_dynamic3 <- oneTag$accY - (accSmooth(oneTag$accY, winSize3s))
    Z_dynamic3 <- oneTag$accZ - (accSmooth(oneTag$accZ, winSize3s))
    X_dynamic025 <- oneTag$accX - (accSmooth(oneTag$accX, winSize025s))
    Y_dynamic025 <- oneTag$accY - (accSmooth(oneTag$accY, winSize025s))
    Z_dynamic025 <- oneTag$accZ - (accSmooth(oneTag$accZ, winSize025s))
    oneTag$accX_static05s <- accSmooth(oneTag$accX, winSize05s) 
    oneTag$accY_static05s <- accSmooth(oneTag$accY, winSize05s)
    oneTag$accZ_static05s <- accSmooth(oneTag$accZ, winSize05s)
    oneTag$accX_dynamic05s <- oneTag$accX - oneTag$accX_static05s
    oneTag$accY_dynamic05s <- oneTag$accY - oneTag$accY_static05s
    oneTag$accZ_dynamic05s <- oneTag$accZ - oneTag$accZ_static05s
    # Calculate odba and vedba (vedba smoothed at 3 sec, 0.25 sec and 0.5 sec)
    oneTag$odba_smooth3s <- abs(X_dynamic3) + abs(Y_dynamic3) + abs(Z_dynamic3)
    oneTag$vedba_smooth3s_move <- sqrt((X_dynamic3)^2 + (Y_dynamic3)^2 + (Z_dynamic3)^2)
    oneTag$vedba_smooth025s_noise <- sqrt((X_dynamic025)^2 + (Y_dynamic025)^2 + (Z_dynamic025)^2)
    oneTag$vedba_smooth05s_flap <- sqrt((oneTag$accX_dynamic05s)^2 + (oneTag$accY_dynamic05s)^2 + (oneTag$accZ_dynamic05s)^2)
    # oneTag$osba_smooth <- abs(oneTag$accX_static) + abs(oneTag$accY_static) + abs(oneTag$accZ_static)
    # oneTag$vesba_smooth <- sqrt((oneTag$accX_static)^2 + (oneTag$accY_static)^2 + (oneTag$accZ_static)^2)
    # oneTag$veclength_static <- sqrt(oneTag$accX_static^2 + oneTag$accY_static^2 + oneTag$accZ_static^2)
    # oneTag$Pitch <- as.circular(asin(oneTag$accX_static), "angle", "radians")
    # oneTag$Sway <- as.circular(asin(oneTag$accY_static), "angle", "radians")
    oneTag$trunc_timestamp <- trunc.POSIXt(oneTag$NewTimestamp, 'secs')

    return(oneTag)
})
save(allACC_newTime_smooth, file=paste0("allACCdata_correctTimestamp_smoothedACC_",oneDay,".RData"))
})

### And finally aggregate the smoothed ACC data per second (creating "bursts"), and calculate ODBA/VeDBA ####
#_____________________________________________________________________________________________________________

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/ACC_format")
# List the files containing the lists of smoothed ACC data per day
ACC_dailyLists_smoothed <- list.files(".", full.names = T, pattern="correctTimestamp_smoothedACC")

llply(1:length(ACC_dailyLists_smoothed), .fun=function(i){  # per day
  load(ACC_dailyLists_smoothed[[i]])  # object allACC_newTime_smooth
  # Extract tag IDs and date
  tagIDs <- names(allACC_newTime_smooth)
  (oneDay <- gsub(".+_|.RData","",ACC_dailyLists_smoothed[[i]]))

  allACC_newTime_burstStats <- llply(allACC_newTime_smooth, .fun=function(oneTag){  #per device
    (deviceID <- unique(oneTag$deviceID))
    # use the timestamp truncated to the second to split the data in "artificial" bursts
    oneTag$trunc_timestamp <- as.character(oneTag$trunc_timestamp)
    burst_list <- split(oneTag, oneTag$trunc_timestamp) 
    #table(sapply(burst_list, nrow)) #all bursts have the same number of samples except first 2 and last 2 that we remove
    burst_list <- burst_list[3:(length(burst_list)-2)]
    
    ACC_df_oneTag <- do.call(rbind, llply(burst_list, .fun=function(oneBurst){  # per burst
      # Burst metrics
      nSamples <- nrow(oneBurst)
      burstDurationSec <- as.numeric(difftime(oneBurst$NewTimestamp[nrow(oneBurst)], oneBurst$NewTimestamp[1], units = 'secs'))
      samplFreq <- burstDurationSec/nSamples
      # Stat on the ACC values
      avgx <- mean(oneBurst$accX)
      avgy <- mean(oneBurst$accY)
      avgz <- mean(oneBurst$accZ)
      sdx <- sd(oneBurst$accX)
      sdy <- sd(oneBurst$accY)
      sdz <- sd(oneBurst$accZ)
      odba_burst <- abs(oneBurst$accX - avgx) + abs(oneBurst$accY - avgy) + abs(oneBurst$accZ - avgz)
      vedba_burst <- sqrt((oneBurst$accX - avgx)^2 + (oneBurst$accY - avgy)^2 + (oneBurst$accZ - avgz)^2)
      # Fit a wave to the 3 axes separetely (frequency of behavior in the three axes)
      mX <- Mod(fft(oneBurst$accX))
      mY <- Mod(fft(oneBurst$accY))
      mZ <- Mod(fft(oneBurst$accZ))
      #plot(mX, type="l");lines(mY, col="red");lines(mZ, col="blue")
      #this is the burst frequency of the wave fitted to the data
      peaksX <- which.max(head(mX[-1], round(length(mX)/2))) + 1
      peaksY <- which.max(head(mY[-1], round(length(mY)/2))) + 1
      peaksZ <- which.max(head(mZ[-1], round(length(mZ)/2))) + 1
      #transform the burst frequency in time frequency dividing by the burst duration
      freqX <- peaksX/burstDurationSec
      freqY <- peaksY/burstDurationSec
      freqZ <- peaksZ/burstDurationSec
      #get the amplitude of the wave
      amplitudeX <- max(mX)/(length(mX)/2)
      amplitudeY <- max(mY)/(length(mY)/2)
      amplitudeZ <- max(mZ)/(length(mZ)/2)
      # Wave metrics on the PCA of the three axes, to extract wing beat frequency
      pc <- prcomp(cbind(oneBurst$accX, oneBurst$accY, oneBurst$accZ), scale. = FALSE)
      #same as bove but on the first principal component (PC1) [useful when using collars and legbends that make the axes rotate]
      fftpc1 <- fft(pc$x[,"PC1"])
      mpc1 <- Mod(fftpc1)
      mpc2 <- Mod(fft(pc$x[,"PC2"]))
      mpc3 <- Mod(fft(pc$x[,"PC3"]))
      peaks1 <- which.max(head(mpc1, round(length(mpc1)/2)))
      fftpc1[-peaks1] <- 0
      ffiltpc1_2 <- Re(fft(fftpc1, inverse = TRUE))/nSamples
      beatsSec <- peaks1/burstDurationSec #freq of the "flapping" wave per second
      amplitude <- max(mpc1)/(length(mpc1)/2) #amplitude of the "flapping" wave per second
      #eigenvector is a direction, eigenvalue is how much variance there is in the data in that direction
      eigenValsPC1wave <- eigen(cov(cbind(mpc1, mpc2, mpc3)))$values[1] #the eigenvector with the highest eigenvalue is the principal component
      #find the variance explained by the three PC [we expect the PC1 to explain most of it, if not it means that the wing beat cannot be captured]
      propExplPC1 <- summary(pc)$importance[2,"PC1"]
      propExplPC2 <- summary(pc)$importance[2,"PC2"] 
      propExplPC3 <- summary(pc)$importance[2,"PC3"] 
      # # Finally plot each ACC burst in the segmentation folder
      # tmp <- trunc.POSIXt(oneBurst$NewTimestamp[1], 'secs')
      # png(paste0("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Segmentation_plots/ACC_flappingPlots/",deviceID,"_",tmp,".png"),
      #     7,7, units = "in", res=300)
      # plot(oneBurst$accX, col="green", type="l", ylim=c(-6,6), 
      #      main=paste0("vedbaSmooth=",round(mean(oneBurst$vedba_smooth),2),"; vedbaBurst=",round(mean(vedba_burst),2),"; flapFreq=",round(beatsSec,2),"; flapAmpl=",round(amplitude,2)))
      # lines(oneBurst$accY, col="blue")
      # lines(oneBurst$accZ, col="red")
      # dev.off()
      
      # Return a data.frame with a summary of all ACC infos (one entry per burst)
      return(data.frame(date = as.Date(oneDay), deviceID = as.character(deviceID),
                        correctTimestamp = as.character(oneBurst$trunc_timestamp[1]), 
                        burstDurationSec = burstDurationSec,
                        numberSamplesPerAxis = nSamples, 
                        samplingFreqPerAxis = samplFreq, 
                        smoothAvg_staticX_05s = mean(oneBurst$accX_static05s,na.rm=T), smoothAvg_staticY_05s = mean(oneBurst$accY_static05s,na.rm=T), smoothAvg_staticZ_05s = mean(oneBurst$accZ_static05s,na.rm=T),
                        smoothAvg_dynamicX_05s = mean(oneBurst$accX_dynamic05s,na.rm=T), smoothAvg_dynamicY_05s = mean(oneBurst$accY_dynamic05s,na.rm=T), smoothAvg_dynamicZ_05s = mean(oneBurst$accZ_dynamic05s,na.rm=T),
                        odbaAvg_smooth3s = mean(oneBurst$odba_smooth3s),
                        vedbaAvg_smooth3s_move = mean(oneBurst$vedba_smooth3s),
                        vedbaAvg_smooth05s_flap = mean(oneBurst$vedba_smooth05s_flap),
                        vedbaAvg_smooth025s_noise = mean(oneBurst$vedba_smooth025s_noise), vedbaCum_smooth025s_noise = sum(oneBurst$vedba_smooth025s_noise),
                        avgX = avgx, avgY = avgy, avgZ = avgz,
                        sdX = sdx, sdY = sdy, sdZ = sdz,
                        # odbaAvg_burst = mean(odba_burst), odbaMedian_burst = median(odba_burst), odbaSd_burst = sd(odba_burst),
                        # vedbaAvg_burst = mean(vedba_burst), vedbaMedian_burst = median(vedba_burst), vedbaSd_burst = sd(vedba_burst),
                        # waveFreqX = freqX, waveFreqY = freqY, 
                        waveFreqZ = freqZ, 
                        # waveAmplX = amplitudeX, waveAmplY = amplitudeY, 
                        waveAmplZ = amplitudeZ,
                        PC1Freq = beatsSec, PC1Ampl = amplitude, #flapping frequency and amplitude
                        propExplPC1 = propExplPC1 #, propExplPC2 = propExplPC2, propExplPC3 = propExplPC3,
                        # varWaveWingBeat = var(ffiltpc1_2), 
                        # varRestWaves = var(pc$x[,"PC1"] - ffiltpc1_2), 
                        # varOrigWave = var(pc$x[,"PC1"]), 
                        # eigenValuesPC1wave = eigenValsPC1wave
                        ))
    }, .parallel=T))
  })
  save(allACC_newTime_burstStats, file=paste0("allACCdata_correctTimestamp_burstMetricsPerSec_",oneDay,".RData"))
})


#__________________________________________________________________________
### Work on the gps files and separate the annotated flight sessions ####

options(digits.secs = 2) #Really important in order not to lose the millisecond ;)
options(digits = 8)

library(plyr)

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp")

# # Import the file with the description of the flight session per tag and per day
# flightSessions <- read.csv("FlightSessions.csv", as.is=T, na.strings=c("","NA"))
# groupComp <- read.csv("GroupComposition.csv", as.is=T, na.strings=c("","NA"))
# 
# flightSessions$matchID <- paste(flightSessions$date,flightSessions$individual,flightSessions$GPS_id, sep="_")
# groupComp$matchID <- paste(groupComp$date,groupComp$individual,groupComp$GPS_id, sep="_")
# groupComp <- groupComp[order(groupComp$date,groupComp$individual),]
# 
# flightSessions_harness <- merge(x=flightSessions, y=groupComp[,c("matchID","ACC_id","harness_type")], by="matchID", all.x=T)
# flightSessions_harness <- flightSessions_harness[,c(1:7,20:21,8:13)]
# flightSessions_harness <- flightSessions_harness[order(flightSessions_harness$date,flightSessions_harness$individual,flightSessions_harness$flight),]
# write.csv(flightSessions_harness, file="FlightSessions_harnessType_deviceID.csv", row.names=F)

# Import the corrected file with the description of the flight session per tag and per day
flightSessions <- read.csv("Data_correct/FlightSessions_harnessType_deviceID.csv", as.is=T, na.strings=c("","NA"))
flightSessions$date <- as.Date(flightSessions$date, format="%d/%m/%Y")
flightSessions$flightID <- paste0("gps",flightSessions$GPS_id,"_flight",flightSessions$flight)

# Read the annotated GPS files
dateFolders <- grep("2018-06|2018-07",list.dirs("Data_raw_SDcardOlivier_Rocamadour2018", recursive=F), value=T) # all daily folders

for(i in 1:length(dateFolders)){
  (oneDay <- gsub(".+/", "", dateFolders[[i]]))
  flightSessions_sub <- flightSessions[flightSessions$date==oneDay,]
  # Work on annotated GPS tags
  allGPSs <- list.files(paste0("Data_raw_SDcardOlivier_Rocamadour2018/",oneDay,"/Formated_and_annotated_files"), pattern="GPS.*csv|Gipsy.*csv|Axytrek.*csv|AxyTrek.*csv", full.names = T)
  GPStags_ls <- unlist(llply(1:length(allGPSs), .fun=function(j){
    gps <- read.csv(allGPSs[j], as.is=T, na.strings=c("","NA"))
    gps$Date <- as.Date(gps$Date, format="%d/%m/%Y")
    gps$Timestamp <- as.POSIXct(paste0(gps$Date," ",gps$Time), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    names(gps)[names(gps)%in%c("behaviour","behavior")] <- "flightSession"
    gps <- gps[,c("Date","Timestamp","Longitude","Latitude","height.above.msl","flightSession","satellite.count","hdop")]
    gps$GPS_id <- sub("GPS|GPS0|GPS-|Gipsy|Gipsy0|AxyTrek|Axytrek","",strsplit(allGPSs[j],"/|_")[[1]][10])
    print(unique(gps$GPS_id))
    gps$flightID <- paste0("gps",gps$GPS_id,"_flight",substr(gps$flightSession, nchar(gps$flightSession), nchar(gps$flightSession)))
      #split by flight sessions and remove non annotated data
    flight_ls <- split(gps, gps$flightSession)
    flight_ls_complete <- llply(flight_ls, .fun=function(oneFlight){
      oneFlight_new <- merge(x=oneFlight, y=flightSessions_sub[,c("flightID","group","age","species","individual","ACC_id","harness_type")],
                                              by="flightID", all.x=T)
      oneFlight_new$TAG_id <- paste0("gps",oneFlight_new$GPS_id,"_acc",oneFlight_new$ACC_id)
      return(oneFlight_new)})
    newNames <- as.character(sapply(flight_ls_complete, function(x) paste0(unique(x$TAG_id),"_",unique(x$flightSession))))
    names(flight_ls_complete) <- newNames
    return(flight_ls_complete)
  }), recursive=F) #flatten the list to have one list of flight sessions, independently from the tag

  # Work on non-annotated Ornitela tags
  allOrnis <- list.files(paste0("Data_raw_SDcardOlivier_Rocamadour2018/",oneDay,"/Formated_and_annotated_files"), pattern="Orni.*csv", full.names = T)
  if(length(allOrnis)==0){flightSessions_ls <- GPStags_ls} #if ornitela are missing stop the llply here and save directly
  if(length(allOrnis)>0){
    ORNItags_ls <- unlist(llply(1:length(allOrnis), .fun=function(j){
      gps <- read.csv(allOrnis[j], as.is=T, na.strings=c("","NA"))
      gps <- gps[which(gps$datatype=="GPS"),]
      gps$Timestamp <- as.POSIXct(gps$UTC_datetime, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      gps <- gps[,c("UTC_date","Timestamp","Longitude","Latitude","Altitude_m","flightSession","satcount","hdop","device_id")]
      colnames(gps) <- c("Date","Timestamp","Longitude","Latitude","height.above.msl","flightSession","satellite.count","hdop","GPS_id")
      print(unique(gps$GPS_id))
      gps$flightID <- paste0("gps",gps$GPS_id,"_flight",substr(gps$flightSession, nchar(gps$flightSession), nchar(gps$flightSession)))
      #split by flight sessions and remove non annotated data
      flight_ls <- split(gps, gps$flightSession)
      flight_ls_complete <- llply(flight_ls, .fun=function(oneFlight){
        oneFlight_new <- merge(x=oneFlight, y=flightSessions_sub[,c("flightID","group","age","species","individual","ACC_id","harness_type")],
                               by="flightID", all.x=T)
        oneFlight_new$TAG_id <- paste0("gps",oneFlight_new$GPS_id,"_acc",oneFlight_new$ACC_id)
        return(oneFlight_new)})
      newNames <- as.character(sapply(flight_ls_complete, function(x) paste0(unique(x$TAG_id),"_",unique(x$flightSession))))
      names(flight_ls_complete) <- newNames
      return(flight_ls_complete)
    }), recursive=F)
    # Put normal GPSs and Ornitelas in the same list at the same level
    flightSessions_ls <- c(GPStags_ls, ORNItags_ls)
  }
  # Save all annotated flight sessions for all tags as daily lists
  save(flightSessions_ls, file=paste0("Data_correct/GPS_format/allGPSdata_annotatedFlightSessions_",oneDay,".RData"))
}

### Resample all GPS data to 1 sec (Gipsy5 recorded at 4 or 5 Hz)
setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct")

GPS_dailyLists_flightSessions <- list.files("GPS_format", pattern="annotatedFlightSessions_2018", full.names = T)
for(i in 1:length(GPS_dailyLists_flightSessions)){
  (oneDay <- gsub(".+_|.RData", "", GPS_dailyLists_flightSessions[[i]]))
  load(GPS_dailyLists_flightSessions[i]) #object flightSessions_ls
  flightSessions_ls_1s <- llply(flightSessions_ls, .fun=function(gps){
    gps1sec <- gps[!duplicated(trunc.POSIXt(gps$Timestamp, 'secs')),] #trunc the timestamp to sec and remove duplicates
    return(gps1sec)
  })
  save(flightSessions_ls_1s, file=paste0("GPS_format/allGPSdata_annotatedFlightSessions_1secResample_",oneDay,".RData"))
}


### Finally we can match the GPS information with the aggregated ACC burst metrics ####
#_______________________________________________________________________________________


library(plyr)
library(parallel)
library(doMC)
registerDoMC(detectCores()-1)

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct")

# List the GPS rdata files containing the gps data per day, per tag and per flight session
GPS_dailyLists_flightSessions <- list.files("GPS_format", pattern="annotatedFlightSessions_1secResample", full.names = T)
# List the ACC rdata files containing the burst metrics per day and per tag
ACC_dailyLists_burstMetrics <- list.files("ACC_format", full.names = T, pattern="correctTimestamp_burstMetricsPerSec")

llply(1:length(GPS_dailyLists_flightSessions), function(i){
  (oneDay <- gsub(".+_|.RData", "", GPS_dailyLists_flightSessions[[i]]))
  # Load the GPS data
  load(GPS_dailyLists_flightSessions[i]) #object flightSessions_ls_1s
  # Load the ACC data for the corresponding day
  load(grep(ACC_dailyLists_burstMetrics, pattern=oneDay, value=T)) #object allACC_newTime_burstStats
  
  flightSessions_GpsAcc <- llply(flightSessions_ls_1s, .fun=function(gps){
    # List columns containing ACC infos to associate to GPS
    accCols <- colnames(allACC_newTime_burstStats[[1]])[-1] #all acc cols excluding date
    accColsToGps <- accCols
    accColsToGps[1:5] <- paste0("acc_",accColsToGps[1:5])
    gps[,accColsToGps] <- NA
    # For each gps flight session, select the corresponding acc device
    matchingAccTag <- which(gsub(".+_","",names(allACC_newTime_burstStats))==unique(gps$ACC_id))
    if(length(matchingAccTag)>0){
      acc <- allACC_newTime_burstStats[[matchingAccTag]]
      acc$correctTimestamp <- as.POSIXct(as.character(acc$correctTimestamp), format="%Y-%m-%d %H:%M:%S", tz="UTC")
      # And associate each GPS entry to the ACC info in 3 seconds interval (it should be the same second)
      for(h in 1:nrow(gps)){ 
        print(paste0("Associating ACC info to GPS observation ",h," of ",nrow(gps)))
        matchAccRows <- acc[acc$correctTimestamp >= gps$Timestamp[h]-1 & acc$correctTimestamp <= gps$Timestamp[h]+1,] 
        #there could be no acc data in that close interval, but if there are some (nrow > 1) then we can associate the gps entry to the closest one
        if(nrow(matchAccRows) >= 1){
          minTimeDiff <- which.min(abs(difftime(matchAccRows$correctTimestamp, gps$Timestamp[h], units="secs"))) #calculates the time difference
          matchAccRows$correctTimestamp <- as.character(matchAccRows$correctTimestamp)
          matchAccRows$deviceID <- as.character(matchAccRows$deviceID)
          gps[h, accColsToGps] <-  matchAccRows[minTimeDiff, accCols]
        }
      }
      gps$acc_correctTimestamp <- as.POSIXct(gps$acc_correctTimestamp, origin = "1970-01-01", tz = "UTC")
    }
    return(gps)
  }, .parallel=T)
  # Finally save the GPS data associated to ACC data in daily lists
  save(flightSessions_GpsAcc, file=paste0("ACC&GPS_format/allGPS&burstACC_allFlightSessions_",oneDay,"_accSummary.RData") )
})

### Calculate additional gps metrics using the move package and save it as list of dataframes ####
#__________________________________________________________________________________________________

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct")

library(plyr)
library(parallel)
library(doMC)
registerDoMC(detectCores()-1)

library(move)
direction360 <- function(x){ 
  return(ifelse(x < 0, 360 + x, x))} 

# List the GPS+ACC rdata files containing the daily lists of data (each element is one flight session)
data_dailyLists <- list.files("ACC&GPS_format", pattern="accSummary.RData", full.names = T) 

for(i in 1:length(data_dailyLists)){
  # Load the move obj
  load(data_dailyLists[[i]]) # object flightSessions_GpsAcc
  (oneDay <- unique(flightSessions_GpsAcc[[1]]$Date))
  flightSessions_gpsList <- llply(flightSessions_GpsAcc, .fun=function(gps){
    gps$flightSession <- substr(gps$flightSession, nchar(gps$flightSession), nchar(gps$flightSession))
    gps$tag_session_id <- paste0(oneDay,"_gps",unique(gps$GPS_id),"_acc",unique(gps$ACC_id),"_flight",unique(gps$flightSession))
    gps_move <- move(x=gps$Longitude,
                     y=gps$Latitude,
                     proj="+proj=longlat +ellps=WGS84",
                     time=gps$Timestamp,
                     animal=gps$tag_session_id,
                     data=gps)
    gps_move$timelag <- c(NA,timeLag(gps_move, units="secs"))
    gps_move$height.diff <- c(NA, gps_move$height.above.msl[-1] - gps_move$height.above.msl[-nrow(gps_move)])
    gps_move$grSpeed <- c(NA, speed(gps_move))
    gps_move$vertSpeed <- gps_move$height.diff/gps_move$timelag
    gps_move$turnAngle <- c(NA,NA, turnAngleGc(gps_move))
    gps_move$direction <- c(NA, direction360(angle(gps_move)))
    gps_move$stepLength <- c(NA, distance(gps_move))
    # Transform move in dataframe and remove extra columns
    gps <- as.data.frame(gps_move)
    if(is.na(unique(gps$acc_deviceID)) | unique(gsub(".+_", "", gps$acc_deviceID))==unique(gps$ACC_id)){
    gps[,c("coords.x1","coords.x2","optional","sensor","timestamps","acc_deviceID")] <- NULL
    gps$acc_correctTimestamp <- as.character(gps$acc_correctTimestamp)
    return(gps)}
    else if(unique(gsub(".+_", "", gps$acc_deviceID))!=unique(gps$ACC_id)){
      stop(paste0("ACC device ",unique(gps$tag_session_id)," not matched correctly"))}}, .parallel=T)
  # Save the result as daily lists of dataframes (one per flight session and per tag)
  save(flightSessions_gpsList, file=paste0("ACC&GPS_format/allGPS&burstACC_allFlightSessions_",oneDay,"_accSummary&moveVariables.RData"))
}


### Let's combine this final dataset with the original ACC data points (25 values per second) ####
#_______________________________________________________________________________________________________
setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct")

library(move)
library(lubridate)

data_dailyLists <- list.files("ACC&GPS_format", pattern="moveVariables", full.names = T) 

ACC_dailyLists_smoothed <- list.files("ACC_format", full.names = T, pattern="correctTimestamp_smoothedACC")

for(i in 1:length(data_dailyLists)){
  # Load the move obj
  load(data_dailyLists[i]) #object flightSessions_gpsList
  (oneDay <- unique(flightSessions_gpsList[[1]]$Date))
  # Load the compelte ACC data (25 Hz) for the corresponding day
  load(grep(ACC_dailyLists_smoothed, pattern=oneDay, value=T)) #object allACC_newTime_smooth
  # match gps and acc
  gpsList_fullACC <- llply(flightSessions_gpsList, .fun=function(gps){
    # Add a sensor column
    gps$sensor <- "gps"
    gps$trunc_timestamp <- as.character(trunc.POSIXt(gps$Timestamp, 'secs'))
    # For each gps flight session, select the corresponding acc device
    matchingAccTag <- which(gsub(".+_","",names(allACC_newTime_smooth))==unique(gps$ACC_id))
    if(length(matchingAccTag)==0){ #in case there is no corresponding acc device
      gps_missCol <- c("accX","accY","accZ","clock_start_calibr","NewTimestamp","acc_samplFreq","accX_static05s","accY_static05s",
                       "accZ_static05s","accX_dynamic05s","accY_dynamic05s","accZ_dynamic05s","odba_smooth3s","vedba_smooth3s_move",
                       "vedba_smooth025s_noise","vedba_smooth05s_flap")
      gps[,gps_missCol] <- NA
      return(gps)}
    else if(length(matchingAccTag)>0){
      acc <- allACC_newTime_smooth[[matchingAccTag]]
      acc$sensor <- "acc"
      # Keep only acc data in the same trunc timestamps as the gps and only columns of interest
      acc_sub <- acc[which(as.character(acc$trunc_timestamp)%in%unique(gps$acc_correctTimestamp)),c(5:7,9,11:24)]
      if(nrow(acc_sub)==0){ # if acc dataset corresponding to gps time range is empty
        gps_missCol <- c("accX","accY","accZ","clock_start_calibr","NewTimestamp","acc_samplFreq","accX_static05s","accY_static05s",
                         "accZ_static05s","accX_dynamic05s","accY_dynamic05s","accZ_dynamic05s","odba_smooth3s","vedba_smooth3s_move",
                         "vedba_smooth025s_noise","vedba_smooth05s_flap")
        gps[,gps_missCol] <- NA
        return(gps)} 
      if(nrow(acc_sub)>0){  # otherwise if acc data for that time range exist
      acc_sub$Timestamp <- acc_sub$NewTimestamp
      acc_sub$trunc_timestamp <- as.character(acc_sub$trunc_timestamp)
      acc_sub$clock_start_calibr <- as.character(acc_sub$clock_start_calibr)
      acc_sub$NewTimestamp <- as.character(acc_sub$NewTimestamp)
      # Add missing columns to the gps and the acc datasets before merging them
      gps_missCol <- names(acc_sub)[!names(acc_sub)%in%names(gps)]
      acc_missCol <- names(gps)[!names(gps)%in%names(acc_sub)]
      gps[,gps_missCol] <- NA
      acc_sub[,acc_missCol] <- NA
      # Rbind the GPS burst infos with the complete 25 Hz ACC info
      gps_acc <- rbind(gps, acc_sub)
      gps_acc <- gps_acc[order(gps_acc$Timestamp),]
      #length(unique(gps$Timestamp))==length(unique(gps_acc$Timestamp))
      # Slightly different approach using merge, in this case all gps values would be repeat each 25 times
      # gps_acc2 <- merge(x=gps, y=acc_sub, by.x="acc_correctTimestamp", by.y="trunc_timestamp", all=T)
      # Repeat the idData information (tag type, ids, flight session etc) also for the acc data points
      gps_acc_ls <- split(gps_acc, gps_acc$trunc_timestamp)
      #table(sapply(gps_acc_ls, nrow))
      gps_acc <- do.call(rbind, lapply(gps_acc_ls, function(ga){
        idCols <- c("flightID","Date","flightSession","GPS_id","group","age","species","individual","ACC_id","harness_type","TAG_id","tag_session_id")
        ga[,idCols] <- ga[ga$sensor=="gps",idCols] #associate the idData of the gps also to the acc
        ga <- ga[order(ga$sensor, decreasing=T),]
        return(ga)
      }))
    return(gps_acc)}
    }
  }, .parallel=T)
  save(gpsList_fullACC, file=paste0("ACC&GPS_format/allGPS&smoothedACC_",oneDay,"_allACCpoints25hz.RData"))
}


#___________________________________________________________________________________
### Merge all sessions and days in one complete dataframe and tidy up columns ####

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct")

library(move)
library(lubridate)

# List the files with the complete gps and acc information (25 Hz)
GPSfullACC_dailyLists <- list.files("ACC&GPS_format", pattern="allACCpoints25hz", full.names = T) 
#GPSsummACC_dailyLists <- list.files("ACC&GPS_format", pattern="moveVariables", full.names = T) 

complete_df <- do.call(rbind, lapply(GPSfullACC_dailyLists, function(gpsList){
  load(gpsList) #object gpsList_fullACC
  return(do.call(rbind, gpsList_fullACC))
}))

# Check coherence of individual names, harness types and species names
complete_df$species[which(complete_df$species=="RUPPEL")] <- "VR"
complete_df$species[which(complete_df$species=="Black Kite")] <- "BK"
complete_df$species[which(complete_df$species=="PYGTB")] <- "PYG"
complete_df$species[which(complete_df$individual=="SHARON")] <- "VOC"
colnames(complete_df)[colnames(complete_df)=="species"] <- "species_id"
table(complete_df$individual,complete_df$species_id)

species_df <- data.frame(species_id=c("VH","RAV","VR","CONDOR","VF","PYG","VOC","BK"),
           species=c("Gyps himalayensis", "Aquila rapax", "Gyps rueppelli", "Vultur gryphus", "Gyps fulvus", "Haliaeetus leucocephalus", "Haliaeetus vocifer", "Milvus migrans"),
           species_comm=c("Himalayan griffon vulture", "Tawny eagle", "Rueppell's griffon vulture", "Andean condor", "Griffon vulture", "Bald eagle", "African fish eagle", "Black kite"))

complete_df <- merge(x=complete_df, y=species_df, by="species_id", all.x=T)
table(complete_df$individual,complete_df$species_comm)
complete_df$species_comm <- as.character(complete_df$species_comm)
complete_df$species <- as.character(complete_df$species)

complete_df$harness_type[which(complete_df$harness_type%in%c("BackPack","BP"))] <- "Backpack"
complete_df$harness_type[which(complete_df$harness_type=="tail")] <- "Tail"
table(complete_df$harness_type)

complete_df$group[which(complete_df$group=="no group")] <- NA
table(complete_df$group) #only for vultures and tawny eagle (kite and bald eagles were not grouped)

complete_df$age[which(complete_df$age=="OLD")] <- "old"
table(complete_df$age,complete_df$group)
table(complete_df$age,complete_df$individual)

# Define as "control" the data collected the last day on Hercule (both backpack and legloop simultaneously on griffon Hercule)
complete_df$harnessExp <- "treatment"
complete_df$harnessExp[substr(complete_df$Timestamp,1,10)=="2018-07-01" & complete_df$individual=="HERCULE"] <- "control"
table(complete_df$harnessExp)
table(complete_df$tag_session_id[complete_df$harnessExp=="control"],complete_df$harness_type[complete_df$harnessExp=="control"])

# Add a column with the type/generation of the ACC device
complete_df$ACC_type <- "Orni"
complete_df$ACC_type[complete_df$ACC_id %in% c(1:4,10)] <- "AXY"
complete_df$ACC_type[complete_df$ACC_id %in% c(5:8)] <- "AGM"
complete_df$ACC_type[complete_df$ACC_id %in% LETTERS] <- "Axytreck"
table(complete_df$ACC_type, complete_df$ACC_id)

# Check everything once more
complete_df <- complete_df[order(complete_df$tag_session_id, complete_df$Timestamp),]
str(complete_df)
summary(complete_df)

#___________________________
### Save final outputs ####

# Save all flight sessions of all days together in one dataframe
write.csv(complete_df, file="ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&smoothACC25hz.csv", row.names = F)
save(complete_df, file="ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&smoothACC25hz.rdata")

# And now one dataset with all the GPS with associated ACC bursts information (1 acc summary per second)
complete_gps <- complete_df[which(complete_df$sensor=="gps"),c(1:53,70:73)]
write.csv(complete_gps, file="ACC&GPS_format/completeDF_allFlightSessions_allDays_allGPS&burstACC.csv", row.names = F)

# Same but removing all data without ACC info
complete_gps_noNa <- complete_gps[!is.na(complete_gps$vedbaAvg_smooth05s_flap),]
write.csv(complete_gps_noNa, file="ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_noAccNA.csv", row.names = F)

#_______
# Save separately the control data from Hercule and plots
dir.create("ACC&GPS_format/Control_Hercule_2018-07-01")

hercLL <- complete_df[complete_df$harnessExp=="control" & complete_df$harness_type=="LegLoop",]
hercLL_ls <- split(hercLL, hercLL$tag_session_id)
lapply(hercLL_ls, function(df){
  write.csv(df, file=paste0("ACC&GPS_format/Control_Hercule_2018-07-01/Hercule_",unique(df$tag_session_id),"_LegLoop.csv"), row.names=F)
  df$Time <- substr(df$Timestamp, 12, 22)
  df_sub <- df[which(df$sensor=="acc"),c("Date","Time","accX","accY","accZ")]
  write.csv(df_sub, file=paste0("ACC&GPS_format/Control_Hercule_2018-07-01/Hercule_",unique(df$tag_session_id),"_LegLoop_subACC.csv"), row.names=F)
})

hercBP <- complete_df[complete_df$harnessExp=="control" & complete_df$harness_type=="Backpack",]
hercBP_ls <- split(hercBP, hercBP$tag_session_id)
lapply(hercBP_ls, function(df){
  write.csv(df, file=paste0("ACC&GPS_format/Control_Hercule_2018-07-01/Hercule_",unique(df$tag_session_id),"_Backpack.csv"), row.names=F)
  df$Time <- substr(df$Timestamp, 12, 22)
  df_sub <- df[which(df$sensor=="acc"),c("Date","Time","accX","accY","accZ")]
  write.csv(df_sub, file=paste0("ACC&GPS_format/Control_Hercule_2018-07-01/Hercule_",unique(df$tag_session_id),"_Backpack_subACC.csv"), row.names=F)
})

lapply(hercLL_ls, function(df){
  df <- df[df$sensor=="gps",]
  open3d(windowRect=c(65,24,1920,1080)) #big image
  plot3d(x = df$Longitude, y=df$Latitude, z=df$height.above.msl,
         type="l", col="black")
  snapshot3d(filename=paste0("ACC&GPS_format/Control_Hercule_2018-07-01/Hercule_",unique(df$tag_session_id),".png"))
  rgl.close()
})
