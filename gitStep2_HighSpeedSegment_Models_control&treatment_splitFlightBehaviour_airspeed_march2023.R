
# Set working directory to the repository folder
setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment/ScriptsMartina_final/Rocamadour_HarnessExperiment")
#setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment/Dataset") #on Martina's computer

# library(diffr) # compare the two last scripts
# diffr("ScriptsMartina_final/gitStep2_HighSpeedSegment_Models_control&treatment_splitFlightBehaviour_airspeed_acf.R",
#       "ScriptsMartina_final/Rocamadour_HarnessExperiment/gitStep2_HighSpeedSegment_Models_control&treatment_splitFlightBehaviour_airspeed_march2023.R")

# 2.1. FILTERING - Create final dataset ####
#____________________________________________

#Import dataset where each row corresponds to one behavioural segment (dataset created in step 1.3.b, wind variables calculated in step 1.4)
df_segments <- read.csv("finalData/df_summaryValuesPerSegment_wind.csv", as.is = T)

# Check which species have which tag attachment
table(df_segments$harness_type, df_segments$species_id)

# Filter out tail attachments 
df_segments <- df_segments[which(df_segments$harness_type %in% c("Backpack","LegLoop")),]

# Filter out species that were equipped with only one of the two harness types (CONDOR e PYG)
df_segments <- df_segments[which(df_segments$species_id %in% c('VF','VR','VH','BK','RAV')),]

length(unique(df_segments$tag_session_id))

#Filter out also flight sessions in which birds flew only "perch to perch" (no free flight)
df_segments <- df_segments[which(!df_segments$tag_session_id %in% c('2018-07-01_gps3_acc3_flight1','2018-06-29_gps14_acc2_flight2','2018-07-01_gps4_acc4_flight1','2018-06-26_gps4_acc4_flight2')),]

length(unique(df_segments$tag_session_id))
length(unique(df_segments$tag_session_id[df_segments$harness_type=="Backpack"]))
length(unique(df_segments$tag_session_id[df_segments$harness_type=="LegLoop"]))
table(df_segments$harnessExp)

#Have a look at distribution of response variables before separating high from low speed segments
hist(df_segments$grSpeed.mean, breaks = "FD")
hist(df_segments$grSpeed.mean, breaks = "FD", xlim=c(1,7))
summary(df_segments$grSpeed.mean[df_segments$grSpeed.mean > 4])
summary(df_segments$grSpeed.mean[df_segments$grSpeed.mean <= 4])

#Split low and high speed segments in two datasets
LowSp_segments <- df_segments[df_segments$grSpeed.mean <= 4,]
highSp_segments <- df_segments[df_segments$grSpeed.mean > 4,]

#Some checks
summary(LowSp_segments$grSpeed.mean)
summary(highSp_segments$grSpeed.mean)
nrow(LowSp_segments)==length(unique(LowSp_segments$behavID_SoarAct))

#Look at distribution of response variables in the two sub-datasets (high and low speed segments)
hist(LowSp_segments$airspeed, breaks = "FD")
hist(highSp_segments$airspeed, breaks = "FD")

hist(LowSp_segments$vedbaAvg_smooth3s_move.mean, breaks = "FD")
hist(highSp_segments$vedbaAvg_smooth3s_move.mean, breaks = "FD")

hist(LowSp_segments$height.above.msl.max, breaks = "FD")
hist(highSp_segments$height.above.msl.max, breaks = "FD")

hist(highSp_segments$glideRatio.segm, breaks="FD")
hist(sqrt(highSp_segments$glideRatio.segm), breaks="FD")
summary(highSp_segments$glideRatio.segm)

#Save only high speed segments data to be used in the models
save(highSp_segments, file="finalData/ModelDatasets_PerSegment_HIGHspeeds_wind.rdata")


# 2.2. MODELS treatment group ####
#_________________________________

library(lme4)
library(lmerTest)
library(lubridate)
library(ggResidpanel)
library(MuMIn)
library(sp)
library(geosphere)

setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment/ScriptsMartina_final/Rocamadour_HarnessExperiment")
source("function_standardError.R") #function stderr

# Import model dataset (we will use only high speed segments)
load("finalData/ModelDatasets_PerSegment_HIGHspeeds_wind.rdata") #object highSp_segments

# Exclude data related to Hercule (the control individual)
highSp_segments <- highSp_segments[which(highSp_segments$harnessExp %in% c("treatment")),]
nrow(highSp_segments)

# Count number and type of tags
tags <- highSp_segments[,c("GPS_id", "ACC_id", "TAG_id", "ACC_type")]
tags[!duplicated(tags),]

# Add a variable indicatinf the order of the segments from the beginning of the flight trajectory
ls <- split(highSp_segments, highSp_segments$tag_session_id)
highSp_segments <- do.call(rbind, lapply(ls, function(x) {
  x <- x[order(x$start.timestamp),] #ordina temporalmente
  x$segment.from.start<-1:nrow(x)
  return(x)
}))

# Range of distances (in metres) from the releasing point
# Calculate distance of all segments from release point (~ first segment of each track)
ls <- split(highSp_segments, highSp_segments$tag_session_id)
dists <- do.call(c, lapply(ls, function(x){
  return(distVincentyEllipsoid(p1=x[,c("Longitude.mean","Latitude.mean")], p2=x[1,c("Longitude.mean","Latitude.mean")]))
}))
summary(dists/1000) # in Km
mean(dists); stderr(dists) # in metres

# High speed segments
highSp_segments$start.timestamp <- as.POSIXct(strptime(highSp_segments$start.timestamp,
                                                 format="%Y-%m-%d %H:%M:%S", tz="UTC"))
highSp_segments$hour<-as.numeric(hour(highSp_segments$start.timestamp))
# Centre the variable hour and calculate the quadratic term
mean(highSp_segments$hour)
highSp_segments$hourScale <- highSp_segments$hour-12
# Hour centred at 12:00
highSp_segments$hourQ <- (highSp_segments$hourScale)^2

## Order by tag session and time
highSp_segments <- highSp_segments[order(highSp_segments$tag_session_id,highSp_segments$start.timestamp),]

## Split soaring and gliding segments in two datasets to apply models separately
highSp_segmentsSoar <- highSp_segments[which(highSp_segments$kmeanSoar=="soar"),]
highSp_segmentsGlide <- highSp_segments[which(highSp_segments$kmeanSoar=="glide"),]
table(highSp_segments$kmeanAct)
table(highSp_segments$kmeanSoar[highSp_segments$kmeanAct=="act"])

# a. VERTICAL SPEED MODEL ----
# _____________________________
vertSpeed_soar <- lmer(vertSpeed.mean~
                       harness_type*species_id+hourScale+
                        nFixes+
                        (1|individual)+(1|Date),
                        data=highSp_segmentsSoar)
summary(vertSpeed_soar, correlation=F)
anova(vertSpeed_soar)
hist(residuals(vertSpeed_soar))
r.squaredGLMM(vertSpeed_soar)
acf(residuals(vertSpeed_soar, retype="normalized"))
plot(vertSpeed_soar, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vertSpeed_soar)
# Check for the overall effect of the species*harness interaction
# vertSpeed_soar2 <- lmer(vertSpeed.mean~
#                           harness_type+species_id+hourScale+
#                          nFixes+
#                          (1|individual)+(1|Date),
#                        data=highSp_segmentsSoar)
# anova(vertSpeed_soar, vertSpeed_soar2)

# The interaction is significant, so both harness type and interaction species*harness are kept in the model
# Legloop birds have higher climbing rate
# Response is not transformed, estimates are in m/s, no backtransformation needed 

q=quantile(highSp_segmentsGlide$vertSpeed.mean, seq(0,0.1, 0.001))
plot(q)
# vertSpeed_glide <- lmer(vertSpeed.mean~
#                          harness_type*species_id+hourScale+
#                          nFixes+
#                          (1|individual)+(1|Date),
#                        data=highSp_segmentsGlide[-which.min(highSp_segmentsGlide$vertSpeed.mean),])
# summary(vertSpeed_glide, correlation=F)
# Check for the overall effect of the species*harness interaction
vertSpeed_glide2 <- lmer(vertSpeed.mean~
                           harness_type+species_id+hourScale+
                           nFixes+
                           (1|individual)+(1|Date),
                         data=highSp_segmentsGlide[-which.min(highSp_segmentsGlide$vertSpeed.mean),])
#anova(vertSpeed_glide,vertSpeed_glide2)
summary(vertSpeed_glide2, correlation=F)
hist(residuals(vertSpeed_glide2))
r.squaredGLMM(vertSpeed_glide2)
acf(residuals(vertSpeed_glide2, retype="normalized"))
plot(vertSpeed_glide2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vertSpeed_glide2)
# Since the interaction is not significant, check for overall effect of the harness type
# vertSpeed_glide3 <- lmer(vertSpeed.mean~
#                            species_id+hourScale+
#                            nFixes+
#                            (1|individual)+(1|Date),
#                          data=highSp_segmentsGlide[-which.min(highSp_segmentsGlide$vertSpeed.mean),])
# anova(vertSpeed_glide3,vertSpeed_glide2)

# The harness type is slightly significant and indicates lower sink speed when gliding!
# Response is not transformed, estimates are in m/s, no backtransformation needed 


# b. AIRSPEED MODEL ----
# _______________________

highSp_segmentsSoar_sub <- highSp_segmentsSoar[seq(1, nrow(highSp_segmentsSoar), by=2),]

airspeed_soar <- lmer(airspeed~
                       harness_type*species_id+hourScale+
                       nFixes+
                       (1|individual)+(1|Date),
                     data=highSp_segmentsSoar_sub)
# Check for the overall effect of the species*harness interaction
airspeed_soar2 <- lmer(airspeed~
                        harness_type+species_id+hourScale+
                        nFixes+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsSoar_sub)
anova(airspeed_soar, airspeed_soar2)
# Since the interaction is not significant, check for overall effect of the harness type
airspeed_soar3 <- lmer(airspeed~
                        species_id+hourScale+
                        nFixes+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsSoar_sub)
anova(airspeed_soar3, airspeed_soar2)
# Both interaction legloop*species and legloop effect non significant, we still report the second model with the non-significant effect
summary(airspeed_soar2, correlation=F)
hist(residuals(airspeed_soar2))
r.squaredGLMM(airspeed_soar2)
acf(residuals(airspeed_soar2, retype="normalized"))
plot(airspeed_soar2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(airspeed_soar2)
# Response is not transformed, estimates are in m/s, no backtransformation needed 

highSp_segmentsGlide_sub <- highSp_segmentsGlide[seq(1, nrow(highSp_segmentsGlide), by=2),]

airspeed_glide <- lmer(airspeed~
                        harness_type*species_id+hourScale+
                        nFixes+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsGlide_sub)
# Check for the overall effect of the species*harness interaction
airspeed_glide2 <- lmer(airspeed~
                         harness_type+species_id+hourScale+
                         nFixes+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsGlide_sub)
anova(airspeed_glide, airspeed_glide2)
# Since the interaction is not significant, check for overall effect of the harness type
airspeed_glide3 <- lmer(airspeed~
                         species_id+hourScale+
                         nFixes+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsGlide_sub)
anova(airspeed_glide3, airspeed_glide2)
# Both interaction legloop*species and legloop effect non significant, we still report the second model with the non-significant effect
summary(airspeed_glide2, correlation=F)
hist(residuals(airspeed_glide2))
r.squaredGLMM(airspeed_glide2)
acf(residuals(airspeed_glide2, retype="normalized"))
plot(airspeed_glide2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(airspeed_glide2)

# Response is not transformed, estimates are in m/s, no backtransformation needed 


# c. GLIDE RATIO MODEL ----
# __________________________
hist(sqrt(highSp_segments$glideRatio.segm), breaks="FD")

summary(highSp_segmentsGlide$vertSpeed.mean[which(highSp_segmentsGlide$glideRatio.segm > 100)])
summary(highSp_segmentsGlide$vertDist.cum[highSp_segmentsGlide$glideRatio.segm > 100])
summary(highSp_segmentsGlide$glideRatio.segm[highSp_segmentsGlide$vertSpeed.mean < -0.2])
summary(highSp_segmentsGlide$glideRatio.segm)
summary(highSp_segmentsGlide$vertDist.cum[highSp_segmentsGlide$vertSpeed.mean > -0.2 & highSp_segmentsGlide$vertSpeed.mean < -0.01])

# Run glide ratio model only on gliding segments with vertical speed < -0.2 m/s
highSp_segmentsGlide_sub <- highSp_segmentsGlide[highSp_segmentsGlide$vertSpeed.mean < -0.2,]
hist(highSp_segmentsGlide_sub$glideRatio.segm, breaks="FD", xlim=c(0,60))
hist(sqrt(highSp_segmentsGlide_sub$glideRatio.segm), breaks="FD")

# glRatio_glide <- lmer(sqrt(glideRatio.segm)~
#                         harness_type*species_id+hourScale+
#                         nFixes+#vedbaAvg_smooth05s_flap.mean+#height.above.msl.max+#segment.from.start+
#                         (1|individual)+(1|Date),
#                       data=highSp_segmentsGlide_sub)
# summary(glRatio_glide, correlation=F)
# Check for the overall effect of the species*harness interaction
glRatio_glide2 <- lmer(sqrt(glideRatio.segm)~
                         harness_type+species_id+hourScale+
                         nFixes+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsGlide_sub)
anova(glRatio_glide, glRatio_glide2)
# Since the interaction is not significant, check for overall effect of the harness type
# glRatio_glide3 <- lmer(sqrt(glideRatio.segm)~
#                         species_id+hourScale+
#                         nFixes+
#                         (1|individual)+(1|Date),
#                       data=highSp_segmentsGlide_sub)
# anova(glRatio_glide3, glRatio_glide2)
# The harness type is significant, legloop birds have slightly higher glide ratio
summary(glRatio_glide2, correlation=F)
hist(residuals(glRatio_glide2))
r.squaredGLMM(glRatio_glide2)
acf(residuals(glRatio_glide2, retype="normalized"))
plot(glRatio_glide2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(glRatio_glide2)
# Backtransformation of prediction for square root 
modcoefs <- coef(summary(glRatio_glide2))[,"Estimate"]
int <- (modcoefs["(Intercept)"])^2 #intercept glide ratio
ll <- (sum(modcoefs[c("(Intercept)","harness_typeLegLoop")]))^2 #glide ratio with effect of legloop
ll-int #increase (difference) in glide ratio with effect of legloop, which means about 1 m higher horizontal distance per unit of drop


# d. HEIGHT a.s.l. MODEL ----
# ____________________________
# Above sea level might be better than above ground level since we are using average/maximum values per segment
# Calculating height above ground would add a layer of inaccuracy due to the fact that the DEM value used to calculate it
# is the one associated to the centroid of the segment, which reliability also depend on the length and area that the segment occupies.

# Subsample to reduce temporal autocorrelation
highSp_segmentsSoar_sub <- highSp_segmentsSoar[seq(1, nrow(highSp_segmentsSoar), by=4),]

# height_soar <- lmer(log(height.above.msl.max)~
#                       harness_type*species_id+hourScale+
#                       nFixes+
#                       (1|individual)+(1|Date),
#                     data=highSp_segmentsSoar_sub)
# Check for the overall effect of the species*harness interaction
height_soar2 <- lmer(log(height.above.msl.max)~
                       harness_type+species_id+hourScale+
                         nFixes+#segment.from.start+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsSoar_sub)
anova(height_soar, height_soar2)
summary(height_soar2, correlation=F)
hist(residuals(height_soar2))
r.squaredGLMM(height_soar2)
acf(residuals(height_soar2, retype="normalized"))
plot(height_soar2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(height_soar2)
# Since the interaction term is slightly significant we also check for the overall effect of harness type
# height_soar3 <- lmer(log(height.above.msl.max)~
#                        species_id+hourScale+
#                        nFixes+
#                        (1|individual)+(1|Date),
#                      data=highSp_segmentsSoar_sub)
# anova(height_soar3, height_soar2)
# Which is highly significant, legloop birds reach higher altitudes during soaring


# Correct interpretation of log-linear models should be: %changeY = 100 * (e^beta - 1)
# But for very small beta, e^beta ~= 1 + beta, so %changeY ~= beta
# exp(x) == e^x
100*(exp(0.23)-1) #coefficient of harness.type=legloop


# e. VEDBA MODEL ----
# ___________________

#Only 62 active observations, so we only work on passive
table(highSp_segments$kmeanAct)

# Subsample to remove some temporal autocorrelation
highSp_segments_pass_sub <- highSp_segments[which(highSp_segments$kmeanAct=="pass"),]
highSp_segments_pass_sub <- highSp_segments_pass_sub[seq(1, nrow(highSp_segments_pass_sub), by=3),]

# vedba_pass <- lmer(log(vedbaAvg_smooth05s_flap.mean)~
#                      harness_type*species_id+hourScale+
#                      nFixes+
#                      (1|individual)+(1|Date), #+(1|ACC_type) the acc type effect is already absorbed by the combination of individual + date cause the type of ACC was consistent for the same individual during the same day
#                    data=highSp_segments_pass_sub) 
# Check for the overall effect of the species*harness interaction
vedba_pass2 <- lmer(log(vedbaAvg_smooth05s_flap.mean)~
                     harness_type+species_id+hourScale+
                     nFixes+
                     (1|individual)+(1|Date),
                   data=highSp_segments_pass_sub)
anova(vedba_pass, vedba_pass2)
summary(vedba_pass2, correlation=F)
hist(residuals(vedba_pass2))
r.squaredGLMM(vedba_pass2)
acf(residuals(vedba_pass2, retype="normalized"))
plot(vedba_pass2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vedba_pass2)
# The effect of the interaction is slightly significant, so we check the effect of harness type
# vedba_pass3 <- lmer(log(vedbaAvg_smooth05s_flap.mean)~
#                       species_id+hourScale+
#                       nFixes+
#                       (1|individual)+(1|Date),
#                     data=highSp_segments_pass_sub)
# anova(vedba_pass3, vedba_pass2)
# Legloop is highly significant, so we keep that without interaction

# With log transformation coefficient are interpreted as percentages of change in the response variable
# Correct interpretation of log-linear models should be: %changeY = 100 * (e^beta - 1)
# But for very small beta, e^beta ~= 1 + beta, so %changeY ~= beta
# exp(x) == e^x
100*(exp(0.094)-1)


# 2.3. T-TEST for CONTROL - Individual HERCULE ####
#___________________________________________________

# Import model dataset (only high speed segments)
load("finalData/ModelDatasets_PerSegment_HIGHspeeds_wind.rdata") #object highSp_segments

# Exclude treatment
control <- highSp_segments[which(highSp_segments$harnessExp %in% c("control")),]
nrow(control)

# Order by timestamp
head(control$timestamp)
control$timestamp <- as.POSIXct(control$start.timestamp,format="%Y-%m-%d %H:%M:%S", tz="UTC")
control <- control[order(control$harness_type,control$start.timestamp),]

table(control$harness_type, control$kmeanSoar)
table(control$harness_type, control$kmeanAct)

# Have a look at the time range of the two flight sessions
unique(control$tag_session_id)
range(control$timestamp[control$tag_session_id=="2018-07-01_gps4_acc4_flight2"])
range(control$timestamp[control$tag_session_id=="2018-07-01_gps4_acc4_flight3"])
# Flight session2 : "2018-07-01 14:20:19 UTC" "2018-07-01 14:28:33 UTC"
# Flight session 3: "2018-07-01 15:58:23 UTC" "2018-07-01 16:02:23 UTC"

# Both harness types had the same type of device to improve comparability
# Both had Gipsy gps and AXY acc and same number of observations
table(control$harness_type, control$ACC_type)
table(control$harness_type, control$GPS_id)
table(control$harness_type, control$TAG_id)

range(control$timestamp[control$harness_type=="LegLoop"])
range(control$timestamp[control$harness_type=="Backpack"])

summary(control$nFixes[control$harness_type=="LegLoop"])
summary(control$nFixes[control$harness_type=="Backpack"])

## Some boxplots and wilcoxon tests
#by default wilcox.test() calculates an exact p-value if the samples contain less than 50 finite values and there are no ties. Otherwise, a normal approximation is used.

# a. Vertical speed ----
#________________________
boxplot(vertSpeed.mean~harness_type*kmeanSoar,
        ylab="Mean vertical speed (m/s)",
        col=c("chocolate","lightblue"),
        data=control)
wilcox.test(control$vertSpeed.mean~control$harness_type,
            alternative = "two.sided")
control$vertSpeed.mean[control$harness_type!="LegLoop"]
control$vertSpeed.mean[control$harness_type=="LegLoop"]

# b. Airspeed ----
#_____________________
boxplot(airspeed~harness_type*kmeanSoar,
        ylab="Mean ground speed (m/s)",
        col=c("chocolate","lightblue"),
        data=control)
wilcox.test(control$airspeed~control$harness_type,
            alternative = "two.sided")

# c. Glide ratio ----
#_____________________
boxplot(glideRatio.segm~harness_type,
        ylab="Mean ground speed (m/s)",
        col=c("chocolate","lightblue"),
        ylim=c(0,60), data=control[control$kmeanSoar=="glide" & control$vertSpeed.mean < 0,])
wilcox.test(glideRatio.segm~harness_type,
            data=control[control$kmeanSoar=="glide" & control$vertSpeed.mean < 0,],
            alternative = "two.sided")

# d. Height a.s.l. ----
#_______________________
boxplot(height.above.msl.max~harness_type*kmeanSoar,
        ylab="Max height above sea level (m)",
        col=c("chocolate","lightblue"),
        data=control)

wilcox.test(control$height.above.msl.max~control$harness_type,
            alternative = "two.sided")
control$height.above.msl.max[control$harness_type!="LegLoop"]
control$height.above.msl.max[control$harness_type=="LegLoop"]

# e. VeDBA ----
#____________
boxplot(vedbaAvg_smooth05s_flap.mean~harness_type*kmeanAct,
        ylab="Mean VeDBA (g)",
        col=c("chocolate","lightblue"),
        data=control)
wilcox.test(control$vedbaAvg_smooth05s_flap.mean~control$harness_type,
            alternative = "two.sided")



## 2.4. FIGURES ####
#____________________

library(ggplot2)
library(ggpubr)
library(gridExtra)
dir.create("Figures")

# Import model dataset (only high speed segments)
load("finalData/ModelDatasets_PerSegment_HIGHspeeds_wind.rdata") #object highSp_segments

highSp_segments$harness_type <- factor(highSp_segments$harness_type, levels=c("Backpack","LegLoop"))

# a. BOXPLOTS with both Treatment and Control (Fig. 1) ----
#____________________________________________________________

vs <- ggplot(aes(y = vertSpeed.mean, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue"), name="Harness type") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Mean vertical speed [m/s]\n") +
  scale_x_discrete(labels=c("control" = "Validation group", "treatment" = "Experimental group")) +
  theme(legend.position = c(0.15,0.85), 
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key=element_blank(),
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

as <- ggplot(aes(y = airspeed, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Airspeed [m/s]\n") +
  scale_x_discrete(labels=c("control" = "Validation group", "treatment" = "Experimental group")) +
  theme(legend.position = "none",
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

vedba <- ggplot(aes(y = vedbaAvg_smooth05s_flap.mean, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Mean VeDBA [g]\n") +
  scale_x_discrete(labels=c("control" = "Validation group", "treatment" = "Experimental group")) +
  theme(legend.position = "none",
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

height <- ggplot(aes(y = height.above.msl.max, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Maximum height a.s.l. [m]\n") +
  scale_x_discrete(labels=c("control" = "Validation group", "treatment" = "Experimental group")) +
  theme(legend.position = "none",
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

pdf("Boxplots/multiboxplot_variables_harnesstype-treatment_newYlab_airspeed_newXlab.pdf", width=10, height=9)
#pdf("Figures/multiboxplot_variables_harnesstype-treatment_newYlab_airspeed_newXlab.pdf", width=10, height=9)
#png("Figures/multiboxplot_variables_harnesstype-treatment_newYlab.png", width=10, height=9, units="in", res=500)
ggarrange(vs,as,vedba,height, 
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2,
          widths = c(1,1,1,1))
dev.off()

# b. Glide Ratio Plot (Fig. 2) ----
#___________________________________

# Glide ratio was only analysed for gliding segments with vertical speed < -0.2 m/s
highSp_segmentsGlide <- highSp_segments[which(highSp_segments$kmeanSoar=="glide"),]
highSp_segmentsGlide_sub <- highSp_segmentsGlide[highSp_segmentsGlide$vertSpeed.mean < -0.2,]

pdf("Figures/VertDistVShorizDist_perHarnessType.pdf", 7, 6)
#png("Figures/VertDistVShorizDist_perHarnessType.png", width=7, height=6, units="in", res=500)
ggplot(aes(y = vertDist.cum, x = horizDist.cum), data = highSp_segmentsGlide_sub) + 
  geom_point(aes(color=harness_type), alpha=0.8) + 
  ylim(-1200,0) + xlim(0,7500) +#shape=harness_type, 
  scale_color_manual(values=c("darkorange","dodgerblue"), name="Harness type") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\nCumulative horizontal distance [m]") + ylab("Cumulative vertical distance [m]\n") +
  theme(legend.position = c(0.87,0.87), 
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key=element_blank(),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))
dev.off()

