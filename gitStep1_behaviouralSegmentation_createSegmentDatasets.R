
# Set working directory to the repository folder
setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment/ScriptsMartina_final/Rocamadour_HarnessExperiment")

# 1.1 Segmentation ####
#_______________________

#Add the milliseconds
options(digits.secs = 3)

# Import data and order by ID and timestamp
all <- read.csv("formatData/completeDF_allFlightSessions_allDays_GPS&smoothACC25hz.csv", as.is=T)

all$Timestamp <- as.POSIXct(all$Timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
all <- all[order(all$species_id, all$individual, all$tag_session_id, all$Timestamp),]

length(unique(all$tag_session_id[all$species_id %in% c('VF','VR','VH','BK','RAV')]))

library(rgl)
library(scales)

#dir.create("formatData/Segmentation_plots")

species_ind_ls <- lapply(split(all, all$species_id), function(x)return(split(x, x$tag_session_id)))
species <- names(species_ind_ls)
lapply(paste0("Segmentation_plots/",species), dir.create) #create one folder per species

tw=7 #define time window (15 seconds, 7 before and after each location)

all_classif <- do.call(rbind, lapply(species, function(s){

  flights <- names(species_ind_ls[[s]])
  
  flight_df <- do.call(rbind, lapply(flights, function(f){
    
    df <- species_ind_ls[[s]][[f]]
    df_burst <- df[df$sensor=="gps",]
    if(nrow(df_burst)>(tw*2)){ # run only if there are enough locations in the flight
      # Compute smoothed vertical speed in the time window
      #df_burst$turnAngle_smooth15 <- NA
      df_burst$vertSpeed_smooth15 <- NA
      for(i in (tw+1):(nrow(df_burst)-tw)){
        #df_burst$turnAngle_smooth15[i] <- abs(sum(df_burst$turnAngle[(i-tw):(i+tw)]))
        df_burst$vertSpeed_smooth15[i] <- mean(df_burst$vertSpeed[(i-tw):(i+tw)], na.rm=T)
      }
      gps <- df_burst[complete.cases(df_burst$vertSpeed_smooth15),] #remove NAs
      # GPS segmentation using kmeans
      kmeanV <- kmeans(gps$vertSpeed_smooth15, 2)   #Two clusters
      soarId <- which.max(aggregate(gps$vertSpeed_smooth15~kmeanV$cluster, FUN=mean)[,2]) # find the soaring cluster
      soarClust <- rep("glide", length(kmeanV$cluster))
      soarClust[which(kmeanV$cluster==soarId)] <- "soar"
      # Add GPS classification as columns to df
      gps$kmeanSoar <- factor(soarClust, levels=c("soar","glide"))
      
      # ACC segmentation using kmeans
      acc <- df_burst[complete.cases(df_burst[,c("vedbaAvg_smooth05s_flap")]),] #remove NAs from ACC
      if(nrow(acc)>0){
        kmeanACC <- kmeans(as.matrix(acc[,c("vedbaAvg_smooth05s_flap")]), 2)   #N. clusters
        flapId <- which.max(aggregate(acc$vedbaAvg_smooth05s_flap~kmeanACC$cluster, FUN=mean)[,2])
        flapClust <- rep("pass", length(kmeanACC$cluster))
        flapClust[which(kmeanACC$cluster==flapId)] <- "act"
        # Add ACC classification as columns to df
        acc$kmeanAct <- factor(flapClust, levels=c("act","pass"))
        
        # Merge GPS and ACC classification
        class <- merge(x=gps[,c("trunc_timestamp","vertSpeed_smooth15","kmeanSoar")], 
                       y=acc[,c("trunc_timestamp","kmeanAct")],by="trunc_timestamp",all=T)
        class$kmeanSoarAct <- factor(paste(class$kmeanSoar, class$kmeanAct, sep="_"), levels=c("soar_pass","soar_act","glide_pass","glide_act"))
        
        df_merge <- merge(x=df, y=class, by="trunc_timestamp", all.x=T)
      
      }else{ #if there are no acc data available for the ACC segmentation, we merge only the gps classification (no acc classification but we add two empty columns instead)
        class <- gps[,c("trunc_timestamp","vertSpeed_smooth15","kmeanSoar")]
        df_merge <- merge(x=df, y=class, by="trunc_timestamp", all.x=T)
        df_merge$kmeanAct <- NA
        df_merge$kmeanSoarAct <- NA
      }
      # # Plot GPS classification
      # open3d(windowRect=c(65,24,1920,1080))
      # plot3d(x = gpsPlotDF$Longitude, y=gpsPlotDF$Latitude, z=gpsPlotDF$height.above.msl,
      #        type="l", col=c("red","blue")[gpsPlotDF$kmeanSoar])
      # snapshot3d(filename=paste0("formatData/Segmentation_plots/",s,"/",unique(gpsPlotDF$individual),"_",f,"_soar.png"))
      # rgl.close()
      return(df_merge)
    }
    else{ # if there are no enough observation for any classification (< than the smoothing window) we add only empty columns
      df[,c("vertSpeed_smooth15","kmeanSoar","kmeanAct","kmeanSoarAct")] <- NA
      return(df)}
  }))
  return(flight_df) # store final df with added columns in the list
}))

# Some checks
classif_ls <- lapply(split(all_classif, all_classif$species_id), function(x)return(split(x, x$tag_session_id)))
sapply(classif_ls, length)
sapply(species_ind_ls, length)
nrow(all_classif)==nrow(all)
nrow(all_classif[all_classif$sensor=="gps",])==nrow(all[all$sensor=="gps",])

## Save classification results
# continuous ACC data:
save(all_classif, file="formatData/completeDF_allFlightSessions_allDays_GPS&smoothACC25hz_behavClassif.rdata")
# only ACC data associated to each gps fix:
all_classif_summ <- all_classif[all_classif$sensor=="gps",]
save(all_classif_summ, file="formatData/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata")

# 1.2. Assign an ID to each continuous behaviour ####
#____________________________________________________

load("formatData/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata") #object all_classif_summ

# Remove extra columns referring to the raw acc data
all_classif_summ <- all_classif_summ[,-c(54:69)]

head(all_classif_summ)

# Remove rows with NA behavioural classification (these should be only the first 15 points of each flight)
table(is.na(all_classif_summ$kmeanSoar))
15*169
# To make sure of it we remove NA and then re-calculate timelag between locations, it should still be about 1 sec
all_classif_summ <- all_classif_summ[!is.na(all_classif_summ$kmeanSoar),]

flights_ls <- split(all_classif_summ, all_classif_summ$tag_session_id)

lapply(flights_ls, function(f){
  f <- f[order(f$Timestamp),]
  return(table(as.numeric(difftime(f$Timestamp[-1], f$Timestamp[-nrow(f)]))))
})
# Timelag is between 0.75 and 2 seconds for all of the flights except for flight "2018-06-30_gpsH_accH_flight2"
# Let's see what's going on with that flight
test <- flights_ls[["2018-06-30_gpsH_accH_flight2"]]
test <- test[order(test$Timestamp),]
plot(test$Longitude, test$Latitude, type="b")
timeLags <- as.numeric(difftime(test$Timestamp[-1], test$Timestamp[-nrow(test)]))
plot(test$Timestamp[-1], timeLags, type="b")
timeLags[1:1000]
timeLags[1001:2000]
timeLags[2001:length(timeLags)]
# There are timelags of 5 and 6 seconds in the middle of the trajectory, but still acceptable
# And there are big timelags (> 30 sec) in the last segment so I remove the last 15 points and put it back in the list
test <- test[1:(nrow(test)-15),]
plot(test$Longitude, test$Latitude, type="b")
flights_ls[["2018-06-30_gpsH_accH_flight2"]] <- test
# Let's check all timelags again
table(unlist(sapply(flights_ls, function(f){
  return(as.numeric(difftime(f$Timestamp[-1], f$Timestamp[-nrow(f)])))
}))) #now max 5 sec, good

#So far we only removed a few columns and a few points with weird timelags so we simply overwrite the dataset saved in the previous step
all_classif_summ <- do.call(rbind, flights_ls)
save(all_classif_summ, file="formatData/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata")

### We give an ID to each individual behaviour
# Remove rows for which acc information are not available. 
all_classif_summ <- all_classif_summ[!is.na(all_classif_summ$vedbaAvg_smooth05s_flap),]
flights_ls <- split(all_classif_summ, all_classif_summ$tag_session_id)
all_classif_summ_behavID <- do.call(rbind, lapply(flights_ls, function(f){
  f <- f[order(f$Timestamp),]
  class1 <- as.character(f$kmeanAct)
  class2 <- as.character(f$kmeanSoarAct)
  class3 <- as.character(f$kmeanSoar)
  if(anyNA(class1)){stop("There are still NAs in the behavioural classification")}
  classID_1 <- c(0, cumsum(class1[-1] != class1[-length(class1)]))
  classID_2 <- c(0, cumsum(class2[-1] != class2[-length(class2)]))
  classID_3 <- c(0, cumsum(class3[-1] != class3[-length(class3)]))
  f$behavID_Act <- paste0(f$tag_session_id,"_",f$kmeanAct,"_",classID_1)
  f$behavID_SoarAct <- paste0(f$tag_session_id,"_",f$kmeanSoarAct,"_",classID_2)
  f$behavID_Soar <- paste0(f$tag_session_id,"_",f$kmeanSoar,"_",classID_3)
  return(f)
}))

# Save output
save(all_classif_summ_behavID, file="formatData/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.rdata")
write.csv(all_classif_summ_behavID, file="formatData/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.csv", row.names=F)


# 1.3. Prepare final summary datasets for analyses in step 2 and 3 ####
#______________________________________________________________________

dir.create("finalData")

load("formatData/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.rdata") #object all_classif_summ_behavID

length(unique(all_classif_summ_behavID$tag_session_id))
table(all_classif_summ_behavID$behavID_SoarAct)

all_classif_summ_behavID$trunc_timestamp <- as.POSIXct(all_classif_summ_behavID$trunc_timestamp, 
                                                       format="%Y-%m-%d %H:%M:%S", tz="UTC")

library(circular)
all_classif_summ_behavID$direction <- as.circular(all_classif_summ_behavID$direction, template="geographic", units="degrees", rotation="clock")

# a. Create a dataset with the averages values per segments (analyses of step 2) ----
#_____________________________________________________________________________________

segments_ls <- split(all_classif_summ_behavID, all_classif_summ_behavID$behavID_SoarAct)
# Keep only segments longer than 5 fixes
segments_ls <- segments_ls[which(sapply(segments_ls, nrow)>5)]

# Double check that timelag is always 1 sec before averaging values in the next step (timelag is either 1 or 2)
table(unlist(sapply(segments_ls, function(s){unique(as.numeric(difftime(s$trunc_timestamp[-1], s$trunc_timestamp[-nrow(s)], units = "secs")))})))

# Calculate summary stats per segment for the models and save it for the models
summarySegment <- do.call(rbind, lapply(segments_ls, function(seg){
  uniqueCols <- t(unlist(apply(seg[,c(2,39:48,52:ncol(seg))], 2, FUN=unique)))
  meanCols <- t(unlist(apply(seg[,c("Longitude","Latitude","height.above.msl","grSpeed","vertSpeed","odbaAvg_smooth3s","vedbaAvg_smooth3s_move","vedbaAvg_smooth05s_flap","vedbaAvg_smooth025s_noise","vedbaCum_smooth025s_noise")], 2, FUN=mean, na.rm=T)))
  colnames(meanCols) <- paste0(colnames(meanCols),".mean")
  segmDf <- data.frame(start.timestamp=min(seg$trunc_timestamp),
                       end.timestamp=max(seg$trunc_timestamp),
                       uniqueCols, 
                       meanCols,
                       trackDirection.mean=mean.circular(seg$direction, na.rm=T),
                       height.above.msl.max=max(seg$height.above.msl, na.rm=T),
                       horizDist.mean=mean(seg$stepLength, na.rm=T),
                       horizDist.cum=sum(seg$stepLength),
                       vertDist.cum=sum(seg$height.diff),
                       nFixes=nrow(seg))
  segmDf$segmDuration <- as.numeric(difftime(segmDf$end.timestamp, segmDf$start.timestamp, units="secs"))
  segmDf$glideRatio.segm <- segmDf$horizDist.cum/abs(segmDf$vertDist.cum)
  return(segmDf)
}))
# Some checks
summary(summarySegment$horizDist.cum[which(summarySegment$kmeanSoar=="glide")])
summary(summarySegment$vertDist.cum[which(summarySegment$kmeanSoar=="glide")])
summary(summarySegment$glideRatio.segm[which(summarySegment$kmeanSoar=="glide")])
# We get infinite for those gliding segments where there was no vertical drop (probably not real gliding)
summarySegment[which(summarySegment$glideRatio.segm==Inf),c("horizDist.cum","vertDist.cum")]
summarySegment$vertSpeed.mean[which(summarySegment$glideRatio.segm==Inf)]
# Set to NA the glide ratio values for segments that are not classified as gliding or for those with no drop (vertical dist > -0.1 metres)
summary(summarySegment$vertSpeed.mean[which(summarySegment$kmeanSoar!="glide")])
summarySegment$glideRatio.segm[which(summarySegment$kmeanSoar!="glide")] <- NA
summary(summarySegment$vertDist.cum[which(summarySegment$kmeanSoar!="glide")])
summarySegment$glideRatio.segm[which(summarySegment$vertDist.cum > -0.1)] <- NA

# Save the dataset for the models (step 2)
write.csv(summarySegment, "finalData/df_summaryValuesPerSegment.csv", row.names=F)


# b. Create a dataset with the summary values per flight session (analyses of step 3) ----
#__________________________________________________________________________________________

flights_ls <- split(all_classif_summ_behavID, all_classif_summ_behavID$tag_session_id)

summaryFunction <- function(seg){
  uniqueCols <- t(unlist(apply(seg[,c(2,39:42,45:48,56,57)], 2, FUN=unique)))
  sumCols <- t(unlist(apply(seg[,c("stepLength","odbaAvg_smooth3s","vedbaAvg_smooth3s_move","vedbaAvg_smooth05s_flap")], 2,FUN=sum, na.rm=T)))
  colnames(sumCols) <- paste0(colnames(sumCols),".sum")
  meanCols <- t(unlist(apply(seg[,c("vedbaAvg_smooth025s_noise","vedbaCum_smooth025s_noise")], 2, FUN=mean, na.rm=T)))
  colnames(meanCols) <- paste0(colnames(meanCols),".mean")
  flightDuration_sec <- sum(seg$timelag)
  propSoaring <- sum(seg$timelag[which(seg$kmeanSoarAct=="soar_pass")])/flightDuration_sec
  propActive <- sum(seg$timelag[which(seg$kmeanAct=="act")])/flightDuration_sec
  segmDf <- data.frame(start.timestamp=min(seg$trunc_timestamp),
                       end.timestamp=max(seg$trunc_timestamp),
                       flightDuration_sec,
                       propSoaring,
                       propActive,
                       uniqueCols, 
                       sumCols,
                       meanCols)
  return(segmDf)
}

# Apply the summary function to the list of flight sessions and rbind the results
summaryFlightSession <- do.call(rbind, lapply(flights_ls, summaryFunction))
# Save the dataset for the flight session analysis (step 3)
write.csv(summaryFlightSession, "finalData/flightSession_summaryDataset.csv", row.names=F)
