#__________________________________
## Flight Session ####
#__________________________________

#mypath <- "C:/Users/arian/Desktop/MPI/Rocamadour data/"
mypath <- "/home/mscacco/ownCloud/Arianna" #on Martina's computer
(setwd(mypath))

#library("readxl")
datasum_id <- read.csv("Summary_DatasetsPlots/datasum_id.csv", as.is=T)

datasum_id<-datasum_id[which(datasum_id$harness_type %in% c("Backpack","LegLoop")),]
datasum_id<-datasum_id[which(datasum_id$harnessExp %in% c("treatment")),]
datasum_id <- datasum_id[which(!datasum_id$tag_session_id %in% c('2018-07-01_gps3_acc3_flight1','2018-06-29_gps14_acc2_flight2','2018-07-01_gps4_acc4_flight1','2018-06-26_gps4_acc4_flight2')),]

library(lubridate)
datasum_id$start.timestamp <- as.POSIXct(strptime(datasum_id$start.timestamp,
                                                  format="%Y-%m-%d %H:%M:%S", tz="UTC"))
datasum_id$hour<-as.numeric(hour(datasum_id$start.timestamp))

# ricodifico le classi per evitare scritte troppo lunghe nei grafici
label<-data.frame(species_id=unique(datasum_id$species_id),species_new=c("GV","HV","RV","TWE","COND","BDE","BK"),stringsAsFactors = F)
datasum_id<-merge(datasum_id, label, by="species_id", all.x=T)

datasum_id <- datasum_id[which(datasum_id$species_id %in% c('VF','VR','VH','BK','RAV')),]

harness_code <- rep("BP", nrow(datasum_id))
harness_code[datasum_id$harness_type=="LegLoop"] <- "LL"


hist(datasum_id$vedbaAvg_smooth3s_move.sum, breaks="FD")
png("vedbaAvg_smooth3s_move.sum~harness_code.png", 15, 5, unit='in', res=300)
boxplot(vedbaAvg_smooth3s_move.sum~harness_code+species_new,
        ylab="Cumulative VeDBA (g)",
        col=c("chocolate","lightblue"),
        data=datasum_id)
legend("topleft", pch=19, c("Backpack","Leg-loop"), col=c("chocolate","lightblue"), bty="n")
dev.off()

hist(sqrt(datasum_id$stepLength.sum), breaks="FD")
png("stepLength.sum~harness_type.png", 15, 5, unit='in', res=300)
boxplot(stepLength.sum~harness_code+species_new,
        ylab="Cumulative distance (m)",
        col=c("chocolate","lightblue"),
        data=datasum_id)
legend("topleft", pch=19, c("Backpack","Leg-loop"), col=c("chocolate","lightblue"), bty="n")
dev.off()

hist(datasum_id$flightDuration_sec)
png("flightDuration_min~harness_type.png", 15, 5, unit='in', res=300)
boxplot(flightDuration_sec/60~harness_code+species_new,
        ylab="Flight duration (min)",
        col=c("chocolate","lightblue"),
        data=datasum_id)
legend("topleft", pch=19, c("Backpack","Leg-loop"), col=c("chocolate","lightblue"), bty="n")
dev.off()

boxplot(flightDuration_sec~hour, data=datasum_id)
boxplot(flightDuration_sec~Date, data=datasum_id)

png("propSoaring~harness_type.png", 15, 5, unit='in', res=300)
boxplot(propSoaring~harness_code+species_new,
        ylab="Prop time spent in soaring flight",
        col=c("chocolate","lightblue"),
        data=datasum_id)
legend("topleft", pch=19, c("Backpack","Leg-loop"), col=c("chocolate","lightblue"), bty="n")
dev.off()

png("propActive~harness_type.png", 15, 5, unit='in', res=300)
boxplot(propActive~harness_code+species_new,
        ylab="Prop time spent in active flight",
        col=c("chocolate","lightblue"),
        data=datasum_id)
legend("topleft", pch=19, c("Backpack","Leg-loop"), col=c("chocolate","lightblue"), bty="n")
dev.off()

# Create lists by harness type and species
table(datasum_id$species_new, datasum_id$harness_type)
LL_list <- datasum_id[datasum_id$harness_type=="LegLoop",]
LL_list <- split(LL_list, LL_list$species_new)

BP_list <- datasum_id[datasum_id$harness_type=="Backpack",]
BP_list <- split(BP_list, BP_list$species_new)

# Differences will be calculated between the two types of harness within each species
# To avoid replicates, we randomly sample an equal number of observations for both harness types
# if LL > BP we randomly sample a subset of LL; otherwise the opposite
# After having an equal number of observations between LL and BP we calculate all possible combinations 
# and compute the difference for the variables of interest

# Difference is always calculated as BP - LL (so positive diff = BP is higher, negative diff = LL is higher)

diff_comb_fun <- function(x,y){  # x=LL_list[[2]]; y=BP_list[[2]]
  set.seed(1)
  if(nrow(x)>nrow(y)){
    xr <- sample(1:nrow(x), nrow(y))
    yr <- 1:nrow(y)
    comb <- expand.grid(xr, yr)
    xcomb <- x[comb[,1],]; ycomb <- y[comb[,2],]
    diff.df <- data.frame(diffDuration=ycomb$flightDuration_sec-xcomb$flightDuration_sec,
                          diffPropSoaring=ycomb$propSoaring-xcomb$propSoaring,
                          diffPropActive=ycomb$propActive-xcomb$propActive,
                          diffStepLength=ycomb$stepLength.sum-xcomb$stepLength.sum,
                          diffCumVedba=ycomb$vedbaAvg_smooth05s_flap.sum-xcomb$vedbaAvg_smooth05s_flap.sum)
    return(diff.df)
    
  }else{yr <- sample(1:nrow(y), nrow(x))
        xr <- 1:nrow(x)
        comb <- expand.grid(yr, xr)
        ycomb <- y[comb[,1],]; xcomb <- x[comb[,2],]
        diff.df <- data.frame(diffDuration=ycomb$flightDuration_sec-xcomb$flightDuration_sec,
                              diffPropSoaring=ycomb$propSoaring-xcomb$propSoaring,
                              diffPropActive=ycomb$propActive-xcomb$propActive,
                              diffStepLength=ycomb$stepLength.sum-xcomb$stepLength.sum,
                              diffCumVedba=ycomb$vedbaAvg_smooth05s_flap.sum-xcomb$vedbaAvg_smooth05s_flap.sum)
        return(diff.df)
}
}

# Here we apply this function to all variables at the same time to obtain a dataset of differences BP - LL
diff_df <- do.call(rbind, mapply(diff_comb_fun, x=LL_list, y=BP_list, SIMPLIFY = F))


# We want to measure the difference in flight performance between legloop and backpack. 
# To do this, we first measure a baseline within group (LL-LL and BP-BP) to measure these differences against.
# The baseline is calculated for each variable separately, calculating the difference between all possible combinations within the same species.
# Then averaged and taken the absolute value that is included as baseline in the t-test.
# Baselines are therefore calculated within each species and harness type,
# but then all difference values are concatenated and averaged all in one single mean baseline value.

baseline_fun <- function(x, var){
  mat <- outer(x[,var], x[,var], "-") #calculated the difference between all possible combinations
  return(mat[lower.tri(mat)]) #takes the lower triangle of the matrix to remove repeated values
}

baseFlightDur <- c(unlist(lapply(LL_list, baseline_fun, "flightDuration_sec")),
                   unlist(lapply(BP_list, baseline_fun, "flightDuration_sec")))
basePropSoar <- c(unlist(lapply(LL_list, baseline_fun, "propSoaring")),
                   unlist(lapply(BP_list, baseline_fun, "propSoaring")))
basePropAct <- c(unlist(lapply(LL_list, baseline_fun, "propActive")),
                   unlist(lapply(BP_list, baseline_fun, "propActive")))
baseSteplength <- c(unlist(lapply(LL_list, baseline_fun, "stepLength.sum")),
                   unlist(lapply(BP_list, baseline_fun, "stepLength.sum")))
baseCumVedba <- c(unlist(lapply(LL_list, baseline_fun, "vedbaAvg_smooth05s_flap.sum")),
                   unlist(lapply(BP_list, baseline_fun, "vedbaAvg_smooth05s_flap.sum")))


# Difference is always calculated as BP - LL (so positive diff = BP is higher, negative diff = LL is higher)
#library(wmwpow) #package for power analysis of wilcoxon test

summary(diff_df$diffDuration)
wilcox.test(abs(diff_df$diffDuration), mu = mean(abs(baseFlightDur)), alternative="greater")
wilcox.test(diff_df$diffDuration, baseFlightDur)

summary(diff_df$diffPropSoaring)
wilcox.test(abs(diff_df$diffPropSoaring), mu = mean(abs(basePropSoar)), alternative="greater")
wilcox.test(diff_df$diffPropSoaring, basePropSoar)

summary(diff_df$diffPropActive); summary(basePropAct)
wilcox.test(abs(diff_df$diffPropActive), mu = mean(abs(basePropAct)), alternative="greater")
wilcox.test(diff_df$diffPropActive, basePropAct)
# In the case of flapping the difference from the baseline is significant, 
# and the mean of the difference between groups is positive meaning that with backpack they have a higher prop of flapping flight??

summary(diff_df$diffStepLength)
wilcox.test(abs(diff_df$diffStepLength), mu = mean(abs(baseSteplength)), alternative="greater")
wilcox.test(diff_df$diffStepLength, baseSteplength)

summary(diff_df$diffCumVedba)
wilcox.test(abs(diff_df$diffCumVedba), mu = mean(abs(baseCumVedba)), alternative="greater")
wilcox.test(diff_df$diffCumVedba, baseCumVedba)


## No combination of rows
# if(nrow(x)>nrow(y)){
#   xsub <- x[sample(1:nrow(x), nrow(y)),]
#   diff.df <- data.frame(diffDuration=abs(x_sub$flightDuration_sec-y$flightDuration_sec),
#                         diffPropSoaring=abs(x_sub$propSoaring-y$propSoaring),
#                         diffPropActive=abs(x_sub$propActive-y$propActive),
#                         diffStepLength=abs(x_sub$stepLength.sum-y$stepLength.sum),
#                         diffCumVedba=abs(x_sub$vedbaAvg_smooth05s_flap.sum-y$vedbaAvg_smooth05s_flap.sum))
#   return(diff.df)
#   
# }