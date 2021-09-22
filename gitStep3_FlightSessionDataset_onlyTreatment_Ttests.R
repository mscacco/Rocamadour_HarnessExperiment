
## Flight Session Analysis ####
#_______________________________

library(lubridate)

# Set working directory to the repository folder
setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment/ScriptsMartina_final/Rocamadour_HarnessExperiment")

# Import summary information related to each flight session (dataset created in step 1.3.b)
df_FL <- read.csv("finalData/flightSession_summaryDataset.csv", as.is=T)

# Apply the same filters used in the treatment analysis (step 2):
# filter out tail attachment, control group, "perch to perch" flight sessions and species that were equipped with only one of the two harness types
df_FL <- df_FL[which(df_FL$harness_type %in% c("Backpack","LegLoop")),]
df_FL <- df_FL[which(df_FL$harnessExp %in% c("treatment")),]
df_FL <- df_FL[which(!df_FL$tag_session_id %in% c('2018-07-01_gps3_acc3_flight1','2018-06-29_gps14_acc2_flight2','2018-07-01_gps4_acc4_flight1','2018-06-26_gps4_acc4_flight2')),]
df_FL <- df_FL[which(df_FL$species_id %in% c('VF','VR','VH','BK','RAV')),]

# Format timestamp
df_FL$start.timestamp <- as.POSIXct(strptime(df_FL$start.timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
df_FL$hour <- as.numeric(hour(df_FL$start.timestamp))

# Create lists by harness type and species
table(df_FL$species_new, df_FL$harness_type)

LL_list <- df_FL[df_FL$harness_type=="LegLoop",]
LL_list <- split(LL_list, LL_list$species_new)

BP_list <- df_FL[df_FL$harness_type=="Backpack",]
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

