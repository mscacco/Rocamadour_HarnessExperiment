
library(ggplot2)
library(gridExtra)

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp")

df <- read.csv("Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_noAccNA.csv", as.is=T)

summary(df)
table(df$tag_session_id)
table(df$GPS_id)
table(df$ACC_id)
table(df$TAG_id)
table(df$tag_session_id,df$individual)
table(df$harness_type,df$individual)

control <- df[df$harnessExp=="control",]
unique(control$harness_type)

# Simple ACC plots ####
#_______________________

p1 <- ggplot(control, aes(x=ACC_type, y=avgStaticX, fill=harness_type)) + 
  geom_boxplot()
p2 <- ggplot(control, aes(x=ACC_type, y=avgDynamicX, fill=harness_type)) + 
  geom_boxplot()

p3 <- ggplot(control, aes(x=ACC_type, y=avgStaticY, fill=harness_type)) + 
  geom_boxplot()
p4 <- ggplot(control, aes(x=ACC_type, y=avgDynamicY, fill=harness_type)) + 
  geom_boxplot()

p5 <- ggplot(control, aes(x=ACC_type, y=avgStaticZ, fill=harness_type)) + 
  geom_boxplot()
p6 <- ggplot(control, aes(x=ACC_type, y=avgDynamicZ, fill=harness_type)) + 
  geom_boxplot()

pdf("Harness_experiment/plots/plots_control_Hercule.pdf")
ggplot(control, aes(x=ACC_type, y=vedbaAvg_smooth, fill=harness_type)) + 
  geom_boxplot()
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
dev.off()


treatment <- df[!df$harnessExp=="control" & !df$harness_type=="Tail",]
# remove tags for which ACC was not collected (e.g. Orni)
treatment <- treatment[!is.na(treatment$acc_correctTimestamp),]
# Remove tags for which one treatment is missing (backpack or legloop missing)
treatment <- treatment[!treatment$individual%in%c("DAKOTA","HERCULE","KHAN","KUZCO","DAGOBERT","LAFAYETTE","RANTANPLAN","SHARON"),]
table(treatment$ACC_type,treatment$harness_type)
table(treatment$individual,treatment$harness_type)


p1 <- ggplot(treatment, aes(x=ACC_type, y=avgStaticX, fill=harness_type)) + 
  geom_boxplot()
p2 <- ggplot(treatment, aes(x=ACC_type, y=avgDynamicX, fill=harness_type)) + 
  geom_boxplot()

p3 <- ggplot(treatment, aes(x=ACC_type, y=avgStaticY, fill=harness_type)) + 
  geom_boxplot()
p4 <- ggplot(treatment, aes(x=ACC_type, y=avgDynamicY, fill=harness_type)) + 
  geom_boxplot()

p5 <- ggplot(treatment, aes(x=ACC_type, y=avgStaticZ, fill=harness_type)) + 
  geom_boxplot()
p6 <- ggplot(treatment, aes(x=ACC_type, y=avgDynamicZ, fill=harness_type)) + 
  geom_boxplot()


pdf("Harness_experiment/plots/plots_treatments_allIndiv.pdf")
ggplot(treatment, aes(x=individual, y=vedbaAvg_smooth, fill=harness_type)) + 
  geom_boxplot()
ggplot(treatment, aes(x=ACC_type, y=vedbaAvg_smooth, fill=harness_type)) + 
  geom_boxplot()
grid.arrange(ggplot(treatment, aes(x=individual, y=avgStaticX, fill=harness_type)) + geom_boxplot(),
             ggplot(treatment, aes(x=individual, y=avgDynamicX, fill=harness_type)) + geom_boxplot(), nrow=2)
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
dev.off()


# Transform into a moveStack ####
#_________________________________

# options(digits.secs = 3) #Really important in order not to lose the millisecond ;)
# library(move)
# 
# setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp")
# 
# # Order by ID and timestamps
# all <- read.csv("Data_correct/GPS_format/completeDF_allFlightSessions_allDays_GPS&ACC.csv", as.is=T)
# all$Timestamp <- as.POSIXct(all$Timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
# 
# all <- all[order(all$species_id, all$individual, all$tag_session_id, all$Timestamp),]

# ms <- move(x=all$Longitude, y=all$Latitude,
#            time=as.POSIXct(all$Timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#            proj=CRS("+proj=longlat +ellps=WGS84"),
#            animal=all$tag_session_id,
#            data=all)
# 
# plot(ms, type="l")
# 
# library(zoom)
# m=ms[[3]]
# plot(m, type="l")#, xlim=c(1.61200, 1.61216), ylim=c(44.80176, 44.80190))
# plot(m$odbaAvg_smooth~timestamps(m), type="l")
# zm()

# Segmentation ####
#____________________

options(digits.secs = 3) #Really important in order not to lose the millisecond ;)

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp")

# Order by ID and timestamps
all <- read.csv("Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&smoothACC25hz.csv", as.is=T)
all$Timestamp <- as.POSIXct(all$Timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

all <- all[order(all$species_id, all$individual, all$tag_session_id, all$Timestamp),]

length(unique(all$tag_session_id[all$species_id %in% c('VF','VR','VH','BK','RAV')]))

#library(EMbC)
#library(ggplot2)
library(rgl)
library(scales)

dir.create("Segmentation_plots")

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
      
      # Compute smoothed ground speed, height, vertical speed and turning angle in the time window
      df_burst$turnAngle_smooth15 <- NA
      df_burst$vertSpeed_smooth15 <- NA
      df_burst$grSpeed_smooth15 <- NA
      df_burst$height_smooth15 <- NA
      for(i in (tw+1):(nrow(df_burst)-tw)){
        df_burst$turnAngle_smooth15[i] <- abs(sum(df_burst$turnAngle[(i-tw):(i+tw)]))
        df_burst$vertSpeed_smooth15[i] <- mean(df_burst$vertSpeed[(i-tw):(i+tw)], na.rm=T)
        df_burst$grSpeed_smooth15[i] <- mean(df_burst$grSpeed[(i-tw):(i+tw)], na.rm=T)
        df_burst$height_smooth15[i] <- mean(df_burst$height.above.msl[(i-tw):(i+tw)], na.rm=T)
      }
      
      gps <- df_burst[complete.cases(df_burst[,c("vertSpeed_smooth15","turnAngle_smooth15")]),] #remove NAs
      
      # GPS segmentation using kmeans
      kmeanV <- kmeans(gps$vertSpeed_smooth15, 2)   #Get the two clusters
      soarId <- which.max(aggregate(gps$vertSpeed_smooth15~kmeanV$cluster, FUN=mean)[,2]) # which one is the soaring one?
      soarClust <- rep("glide", length(kmeanV$cluster))
      soarClust[which(kmeanV$cluster==soarId)] <- "soar"
      
      kmeanT <- kmeans(gps$turnAngle_smooth15, 2)   #N. clusters
      circId <- which.max(aggregate(gps$turnAngle_smooth15~kmeanT$cluster, FUN=mean)[,2])
      circClust <- rep("line", length(kmeanT$cluster))
      circClust[which(kmeanT$cluster==circId)] <- "circ"
      
      # Add GPS classification as columns to df
      gps$kmeanSoar <- factor(soarClust, levels=c("soar","glide"))
      gps$kmeanSoarTurn <- factor(paste(soarClust, circClust, sep="_"), levels=c("soar_circ","soar_line","glide_circ","glide_line"))

      # ACC segmentation using kmeans
      acc <- df_burst[complete.cases(df_burst[,c("vedbaAvg_smooth05s_flap")]),] #remove NAs from ACC
      if(nrow(acc)>0){

      kmeanACC <- kmeans(as.matrix(acc[,c("vedbaAvg_smooth05s_flap")]), 2)   #N. clusters
      flapId <- which.max(aggregate(acc$vedbaAvg_smooth05s_flap~kmeanACC$cluster, FUN=mean)[,2])
      flapClust <- rep("pass", length(kmeanACC$cluster))
#      flapClust[which(kmeanACC$cluster==flapId & acc$vedbaAvg_smooth05s_flap>0.7)] <- "act"
      flapClust[which(kmeanACC$cluster==flapId)] <- "act"
      
      # Add ACC classification as columns to df
      acc$kmeanAct <- factor(flapClust, levels=c("act","pass"))
      
      # Merge GPS and ACC classification
      class <- merge(x=gps[,c("trunc_timestamp","grSpeed_smooth15","height_smooth15","vertSpeed_smooth15","turnAngle_smooth15","kmeanSoar","kmeanSoarTurn")], 
                     y=acc[,c("trunc_timestamp","kmeanAct")],by="trunc_timestamp",all=T)
      class$kmeanSoarAct <- factor(paste(class$kmeanSoar, class$kmeanAct, sep="_"), levels=c("soar_pass","soar_act","glide_pass","glide_act"))

      df_merge <- merge(x=df, y=class, by="trunc_timestamp", all.x=T)

      
      # Plot ACC segmentation
      #______________________
      # png(filename=paste0("Segmentation_plots/",s,"/",unique(df_merge$individual),"_",f,"_ACC2classes.png"))
      # hist(df_merge$vedbaAvg_smooth05s_flap[df_merge$kmeanAct=="pass" & df_merge$sensor=="gps"], col="grey", xlim=c(0,6), main="Passive/Active")
      # hist(df_merge$vedbaAvg_smooth05s_flap[df_merge$kmeanAct=="act" & df_merge$sensor=="gps"], col="black", add=T)
      # dev.off()
      # 
      # if(dir.exists(paste0("Segmentation_plots/",s,"/rawFlapPlots"))==F){dir.create(paste0("Segmentation_plots/",s,"/rawFlapPlots"))}
      # flapRaw <- df_merge[which(df_merge$kmeanAct=="act"),]
      # flapRaw_ls <- split(flapRaw, flapRaw$trunc_timestamp)
      # lapply(flapRaw_ls, function(flap){
      #   flapGps <- flap[which(flap$sensor=="gps"),]
      #   flapAcc <- flap[which(flap$sensor=="acc"),]
      #   if(nrow(flapAcc)>0){
      #     png(paste0("Segmentation_plots/",s,"/rawFlapPlots/",flapGps$individual,"_",
      #                substr(flapGps$trunc_timestamp,1,10),"_",substr(flapGps$trunc_timestamp,12,19),"_",
      #                flapGps$TAG_id,"_flight",flapGps$flightSession,".png"),
      #         7,7, units = "in", res=300)
      #     plot(flapAcc$accX, col="green", type="l", ylim=c(-10,10), lwd=1.5,
      #          main=paste0("vedba05s=",round(flapGps$vedbaAvg_smooth05s_flap,2),"; vedba3s=",round(flapGps$vedbaAvg_smooth3s_move,2),
      #                      "; vedba025s=",round(flapGps$vedbaCum_smooth025s_noise,2),
      #                      "\n Zampl=",round(flapGps$waveAmplZ,2),"; species=",flapGps$species_id,
      #                      "; height asl=",flapGps$height.above.msl,"; FlapFreq=",round(flapGps$PC1Freq,2)), #waveFreqZ
      #          ylab="Raw ACC [g]")
      #     lines(flapAcc$accY, col="blue", lwd=1.5)
      #     lines(flapAcc$accZ, col="red", lwd=1.5)
      #     legend("bottomleft", c("Acc X", "Acc Y", "Acc Z"), col=c("green","blue","red"), lty=1, lwd=1.5, bty="n")
      #     dev.off()
      #   }
      # })

      # Plot ACC+GPS segmentation
      # gpsPlotDF <- df_merge[df_merge$sensor=="gps",]
      # open3d(windowRect=c(65,24,1920,1080))
      # plot3d(x = gpsPlotDF$Longitude, y=gpsPlotDF$Latitude, z=gpsPlotDF$height.above.msl,
      #        type="l", col=c("red","black","lightblue","blue")[gpsPlotDF$kmeanSoarAct], lwd=2,
      #        xlab="Longitude", ylab="Latitude",zlab="Elevation a.s.l. [m]")
      # snapshot3d(filename=paste0("Segmentation_plots/",s,"/",unique(gpsPlotDF$individual),"_",f,"_soarflap.png"))
      # rgl.close()
      
      }else{ #if there are no acc data available for the ACC segmentation, we merge only the gps classification (no acc classification but we add two empty columns instead)
        class <- gps[,c("trunc_timestamp","grSpeed_smooth15","height_smooth15","vertSpeed_smooth15","turnAngle_smooth15","kmeanSoar","kmeanSoarTurn")]
        df_merge <- merge(x=df, y=class, by="trunc_timestamp", all.x=T)
        df_merge$kmeanAct <- NA
        df_merge$kmeanSoarAct <- NA
      }

      # # Plot GPS classification
      # gpsPlotDF <- df_merge[df_merge$sensor=="gps",]
      # ggplot(gpsPlotDF, aes(x=Timestamp, y=height.above.msl, col=kmeanSoarTurn)) +
      #   geom_path(size=1.2, col="black") + geom_point() +
      #   scale_colour_manual(values=c("red","green","grey","blue"))

      # open3d(windowRect=c(65,24,1920,1080)) #big image
      # plot3d(x = gpsPlotDF$Longitude, y=gpsPlotDF$Latitude, z=gpsPlotDF$height.above.msl,
      #        type="l", col=c("red","green","grey","blue")[gpsPlotDF$kmeanSoarTurn])
      # # spheres3d(x = gpsPlotDF$Longitude, y=gpsPlotDF$Latitude, z=gpsPlotDF$height.above.msl,
      # #           radius=0.5, #alpha=0.5,
      # #           col = c("red","green","grey","blue")[gpsPlotDF$kmeanSoarTurn])
      # snapshot3d(filename=paste0("Segmentation_plots/",s,"/",unique(gpsPlotDF$individual),"_",f,"_4classes.png"))
      # rgl.close()

      # open3d(windowRect=c(65,24,1920,1080))
      # plot3d(x = gpsPlotDF$Longitude, y=gpsPlotDF$Latitude, z=gpsPlotDF$height.above.msl,
      #        type="l", col=c("red","blue")[gpsPlotDF$kmeanSoar])
      # snapshot3d(filename=paste0("Segmentation_plots/",s,"/",unique(gpsPlotDF$individual),"_",f,"_soar.png"))
      # rgl.close()
      
      return(df_merge)
    }
    else{ # if there are no enough observation for any classification (< than the smoothing window) we add only empty columns
      df[,c("grSpeed_smooth15","height_smooth15","vertSpeed_smooth15","turnAngle_smooth15","kmeanSoar","kmeanSoarTurn","kmeanAct","kmeanSoarAct")] <- NA
      return(df)}
  }))
  # Add a plot per species with the flapping frequency
  # if(length(table(flight_df$acc_numberSamplesPerAxis > 30))==1){
  #   png(filename=paste0("Segmentation_plots/",s,"/FlappingFrequency_hist.png"),7,7, units = "in", res=300)
  #   hist(flight_df$PC1Freq[flight_df$kmeanAct=="act"], 
  #        main=paste0(s,"_",round(mean(flight_df$acc_numberSamplesPerAxis,na.rm=T)),"hz"), col="grey", xlab="Flapping frequency (flaps/sec)")
  #   dev.off()
  #   png(filename=paste0("Segmentation_plots/",s,"/Vedba_hist_active.png"),7,7, units = "in", res=300)
  #   hist(flight_df$vedbaAvg_smooth05s_flap[flight_df$kmeanAct=="act"], 
  #        main=paste0(s,"_",round(mean(flight_df$acc_numberSamplesPerAxis,na.rm=T)),"hz"), col="grey", xlab="Mean vedba active (0.5 s smooth)")
  #   dev.off()
  #   png(filename=paste0("Segmentation_plots/",s,"/Vedba_hist_passive.png"),7,7, units = "in", res=300)
  #   hist(flight_df$vedbaAvg_smooth05s_flap[flight_df$kmeanAct=="pass"], 
  #        main=paste0(s,"_",round(mean(flight_df$acc_numberSamplesPerAxis,na.rm=T)),"hz"), col="grey", xlab="Mean vedba passive (0.5 s smooth)")
  #   dev.off()
  # }else if(length(table(flight_df$acc_numberSamplesPerAxis > 30))==2){
  #   png(filename=paste0("Segmentation_plots/",s,"/FlappingFrequency_hist.png"),7,7, units = "in", res=300)
  #   hist(flight_df$PC1Freq[flight_df$kmeanAct=="act" & flight_df$acc_numberSamplesPerAxis < 30], 
  #        main=s, col=alpha("red",0.6), xlab="Flapping frequency (flaps/sec)")
  #   hist(flight_df$PC1Freq[flight_df$kmeanAct=="act" & flight_df$acc_numberSamplesPerAxis > 30], col=alpha("blue",0.6), add=T)
  #   legend("topright", c("25 Hz","50 Hz"), col=c("red","blue"), pch=19, bty="n")
  #   dev.off()
  #   png(filename=paste0("Segmentation_plots/",s,"/Vedba_hist_active.png"),7,7, units = "in", res=300)
  #   hist(flight_df$vedbaAvg_smooth05s_flap[flight_df$kmeanAct=="act" & flight_df$acc_numberSamplesPerAxis < 30], 
  #        main=s, col=alpha("red",0.6), xlab="Mean vedba active (0.5 s smooth)")
  #   hist(flight_df$vedbaAvg_smooth05s_flap[flight_df$kmeanAct=="act" & flight_df$acc_numberSamplesPerAxis > 30], col=alpha("blue",0.6), add=T)
  #   legend("topright", c("25 Hz","50 Hz"), col=c("red","blue"), pch=19, bty="n")
  #   dev.off()
  #   png(filename=paste0("Segmentation_plots/",s,"/Vedba_hist_passive.png"),7,7, units = "in", res=300)
  #   hist(flight_df$vedbaAvg_smooth05s_flap[flight_df$kmeanAct=="pass" & flight_df$acc_numberSamplesPerAxis < 30], 
  #        main=s, col=alpha("red",0.6), xlab="Mean vedba passive (0.5 s smooth)")
  #   hist(flight_df$vedbaAvg_smooth05s_flap[flight_df$kmeanAct=="pass" & flight_df$acc_numberSamplesPerAxis > 30], col=alpha("blue",0.6), add=T)
  #   legend("topright", c("25 Hz","50 Hz"), col=c("red","blue"), pch=19, bty="n")
  #   dev.off()
  # }
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
save(all_classif, file="Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&smoothACC25hz_behavClassif.rdata")
# only ACC data associated to each gps fix:
all_classif_summ <- all_classif[all_classif$sensor=="gps",]
save(all_classif_summ, file="Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata")

# Assign an ID to each continuous behaviour ####
#________________________________________________

setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp")

load("Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata") #object all_classif_summ

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

#So far we only removed a few columns and a few points with weird timelags so we simply overwrite the dataset save in the previous step
all_classif_summ <- do.call(rbind, flights_ls)
save(all_classif_summ, file="Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata")


### Let's give an ID to each individual behaviour
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
save(all_classif_summ_behavID, file="Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.rdata")
write.csv(all_classif_summ_behavID, file="Data_correct/ACC&GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.csv", row.names=F)
# Save it also in this other folder
save(all_classif_summ_behavID, file="/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment/Dataset/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.rdata")

#nrow(all_classif_summ_behavID)==sum(sapply(flights_ls, nrow))
# Check number of flight sessions and individuals at the end of the segmentation
nrow(all_classif_summ_behavID[all_classif_summ_behavID$species_id %in% c('VF','VR','VH','BK','RAV') &
                              all_classif_summ_behavID$harness_type %in% c("Backpack","LegLoop"),])

length(unique(all_classif_summ_behavID$tag_session_id[all_classif_summ_behavID$species_id %in% c('VF','VR','VH','BK','RAV') &
                                                    all_classif_summ_behavID$harness_type %in% c("Backpack","LegLoop")]))

length(unique(all_classif_summ_behavID$individual[all_classif_summ_behavID$species_id %in% c('VF','VR','VH','BK','RAV') &
                                                        all_classif_summ_behavID$harness_type %in% c("Backpack","LegLoop")]))

unique(all_classif_summ_behavID$TAG_id[all_classif_summ_behavID$species_id %in% c('VF','VR','VH','BK','RAV') &
                                             all_classif_summ_behavID$harness_type %in% c("Backpack","LegLoop")])
# This is to have a look at the tag IDs, in the final dataset we have:
# AxyTreck (G, J and Z); Axy1 (acc 1 to 4); AGM (acc 5 to 7); Gipsy1 (gps 3 to 6, 13 and 14); Gipsy5 (gps B)


#_________________________________________________________
# Prepare a dataset with the averages values per segments

#mypath <- "C:/Users/arian/Desktop/MPI/Rocamadour data/"
mypath <- "/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment"
(setwd(mypath))
load("Dataset/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.rdata")

length(unique(all_classif_summ_behavID$tag_session_id))
table(all_classif_summ_behavID$behavID_SoarAct)

all_classif_summ_behavID$trunc_timestamp <- as.POSIXct(all_classif_summ_behavID$trunc_timestamp, 
                                                       format="%Y-%m-%d %H:%M:%S", tz="UTC")

library(circular)
all_classif_summ_behavID$direction <- as.circular(all_classif_summ_behavID$direction, template="geographic", units="degrees", rotation="clock")

segments_ls <- split(all_classif_summ_behavID, all_classif_summ_behavID$behavID_SoarAct)
# Keep only segments longer than 5 fixes
segments_ls <- segments_ls[which(sapply(segments_ls, nrow)>5)]

# Double check that timelag is always 1 sec before averaging values in the next step (timelag is either 1 or 2)
table(unlist(sapply(segments_ls, function(s){unique(as.numeric(difftime(s$trunc_timestamp[-1], s$trunc_timestamp[-nrow(s)], units = "secs")))})))

# Calculate summary stats per segment for the models and save it for the models
summarySegment <- do.call(rbind, lapply(segments_ls, function(seg){
  uniqueCols <- t(unlist(apply(seg[,c(2,39:48,52:58,60:63)], 2, FUN=unique)))
  meanCols <- t(unlist(apply(seg[,c("Longitude","Latitude","height.above.msl","grSpeed","vertSpeed","odbaAvg_smooth3s","vedbaAvg_smooth3s_move","vedbaAvg_smooth05s_flap","vedbaAvg_smooth025s_noise","vedbaCum_smooth025s_noise")], 2, FUN=mean, na.rm=T)))
  colnames(meanCols) <- paste0(colnames(meanCols),".mean")
  segmDf <- data.frame(start.timestamp=min(seg$trunc_timestamp),
                       end.timestamp=max(seg$trunc_timestamp),
                       uniqueCols, 
                       meanCols,
                       direction=mean.circular(seg$direction, na.rm=T),
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
# Check again the summary
summary(summarySegment$glideRatio.segm[which(summarySegment$kmeanSoar=="glide")])
quantile(summarySegment$glideRatio.segm[which(summarySegment$kmeanSoar=="glide")], seq(0,1, 0.001), na.rm=T)
summarySegment$segmDuration[which(summarySegment$glideRatio.segm>100)]
summarySegment$vertDist.cum[which(summarySegment$glideRatio.segm>100)]
summary(summarySegment$vertSpeed.mean[which(summarySegment$glideRatio.segm<2)])
hist(summarySegment$glideRatio.segm, breaks="FD")
# Save the dataset for the models
write.csv(summarySegment, "Dataset/df_summaryValuesPerSegment.csv", row.names=F)

# Plot the glide ratio
ggplot(summarySegment[summarySegment$vertDist.cum > -0.1,], 
       aes(x = grSpeed.mean, y=vertSpeed.mean, col=harness_type)) +
  geom_point()
# These outliers with ground speed near to 0 are going to be removed in the next step before the models, 
# where we will devide high speed from low speed segments


#____________________________________________________________________
# Associate ENV variables from Movebank (we ended up not using this)
#
# # Format the dataset to be uploaded to movebank and annotated with env variables
# colnames(summarySegment)[c(2,26,27,28)] <- c("timestamp","location-long","location-lat","height-above-msl")
# summarySegment$timestamp<-as.POSIXct(strptime(summarySegment$timestamp,format="%Y-%m-%d %H:%M:%S", tz="UTC"))
# summarySegment$timestamp<- strftime(summarySegment$timestamp, format="%Y-%m-%d %H:%M:%OS3")
# write.csv(summarySegment,"dati_movebank.csv", row.names = F)
# #Import movebank data where each row corresponds to one behavioural segment, with no filters applied
# df_segments <- read.csv("Dataset/dati_movebank_bilinear.csv", as.is = T)
# N.B. after re-improting the columns will have a different name:
# start.timestamp = timestamp
# Longitude.mean and Latitude.mean will be location.long and location.lat






# #________________________________
# # Validate ACC segmentation ####
# 
# source("/home/mscacco/ownCloud/Martina/PHD/R_functions/load_rdata.R")
# dir.create("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Segmentation_plots/ACC_flappingPlots_kmeans07")
# setwd("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Segmentation_plots/ACC_flappingPlots_kmeans07")
# 
# # Load results
# load("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/GPS_format/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif.rdata") #object all_classif
# # Isolate only flapping events
# #flapEvents <- all_classif[which(all_classif$kmeanFlap=="act"),]
# flapEvents <- all_classif[which(all_classif$kmeanAct=="act"),]
# head(flapEvents)
# hist(flapEvents$vedbaAvg_smooth)
# hist(flapEvents$PC1Ampl)
# hist(flapEvents$PC1Freq)
# 
# # Split them in flight sessions
# flap_ls <- split(flapEvents, date(flapEvents$Timestamp))
# # List files containing raw ACC data
# ACC_dailyLists_correct <- list.files("/home/mscacco/ownCloud/Martina/PHD/Rocamadour_harnessExp/Data_correct/ACC_format", full.names = T, pattern="correctTimestamp_2018")
# 
# lapply(names(flap_ls), function(day){
#   #day=names(flap_ls)[4]
#   
#   flapDay <- flap_ls[[day]] #df with classified flapping events per day
#   flapDay_ls <- split(flapDay, flapDay$ACC_id)
#   
#   accRaw_ls <- load.rdata(grep(ACC_dailyLists_correct, pattern=day, value=T)) #list of raw data per tag
#   accRaw_tagID <- substr(names(accRaw_ls), nchar(names(accRaw_ls)), nchar(names(accRaw_ls))) #extract only tag number to match the acc id in the flap dataset
#   
#   lapply(names(flapDay_ls), function(tag){
#     #tag=names(flapDay_ls)[8]
#     
#     flaps <- flapDay_ls[[tag]]
#     raw <- accRaw_ls[[which(accRaw_tagID==tag)]]
#     
#     raw$timestampPerSec <- format(raw$NewTimestamp, format="%Y-%m-%d %H:%M:%S")
#     
#     Nplots <- if(nrow(flaps)>100){100}else{nrow(flaps)} #print max 100 plots per tag
#     
#     for(r in 1:Nplots){
#       t <- flaps[r,"acc_correctTimestamp"]
#       rawSub <- raw[which(raw$timestampPerSec==t),]
#       png(paste0("accTag_",tag,"_",t,".png"),
#           7,7, units = "in", res=300)
#       plot(rawSub$accX, col="green", type="l", ylim=c(-10,10),
#            main=paste0("vedbaBurst=",round(flaps[r,"vedbaAvg_burst"],2),"; vedbaSmooth=",round(flaps[r,"vedbaAvg_smooth"],2),"; flapAmpl=",round(flaps[r,"PC1Ampl"],2),
#                        "\n species=",flaps[r,"species_id"],"; height asl=",flaps[r,"height.above.msl"]))
#       lines(rawSub$accY, col="blue")
#       lines(rawSub$accZ, col="red")
#       dev.off()
#     }
#   })
# })
# 


#Embc segmentation
# mat1 <- as.matrix(df[,c("vertSpeed_smooth", "turnAngle_smooth")], ncol = 2)     
# class <- embc(mat1)                                       
# sctr(class, xlab="Mean climbing rate [m/s]", ylab="Cumulative turning angle [deg]")
# 
# df$embcClass <- factor(class@A, levels=c(1,2,3,4))

#Plot embc classes
# ggplot(df, aes(x=Longitude, y=Latitude, col=embcClass)) +
#   geom_path(size=1.2, col="black") + geom_point() +
#   scale_colour_manual(values=c("blue","green","red","grey"))
# ggplot(df, aes(x=Timestamp, y=height.above.msl, col=embcClass)) +
#   geom_path(size=1.2, col="black") + geom_point() +
#   scale_colour_manual(values=c("blue","green","red","grey"))
# open3d()
# plot3d(x = df$Longitude, y=df$Latitude, z=df$height.above.msl,
#           type="l", col=c("blue","green","red","grey")[df$embcClass]) #,col="black
# spheres3d(x = df$Longitude, y=df$Latitude, z=df$height.above.msl,
#           radius=0.5, #alpha=0.5,
#           col = c("blue","green","red","grey")[df$embcClass])

  

  


  
  





