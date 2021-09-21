
#mypath <- "C:/Users/arian/Desktop/MPI/Rocamadour data/"
mypath <- "/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment" #on Martina's computer
(setwd(mypath))

#_________________
# FILTERING ####
#________________

#Import movebank data where each row corresponds to one behavioural segment, with no filters applied
df_segments <- read.csv("Dataset/df_summaryValuesPerSegment.csv", as.is = T)

# Calculate max height above ground subtracting the DEM associated to the segment centroid from the max height asl of the segment
#df_segments$height.above.ground.max <- df_segments$height.above.msl.max - df_segments$ASTER.ASTGTM2.Elevation

# Check which species have which tag attachment
table(df_segments$harness_type, df_segments$species_id)
# Filter out tail attachments 
df_segments <- df_segments[which(df_segments$harness_type %in% c("Backpack","LegLoop")),]
#keep both treatment and control (hercule with double attachment)
#df_segments <- df_segments[which(df_segments$harnessExp %in% c("treatment", "control)),]

# Filter out species that were equipped with only one of the two harness types (CONDOR e PYG)
df_segments <- df_segments[which(df_segments$species_id %in% c('VF','VR','VH','BK','RAV')),]

length(unique(df_segments$tag_session_id))

#Filter out also flight sessions in which birds did "perch to perch"
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

#Split gliding and soaring segments in two datasets
LowSp_segments_gliding <- df_segments[which(LowSp_segments$kmeanSoarAct %in% c("glide_act","glide_pass")),]
LowSp_segments_soaring <- df_segments[which(LowSp_segments$kmeanSoarAct %in% c("soar_act","soar_pass")),]
highSp_segments_gliding <- df_segments[which(highSp_segments$kmeanSoarAct %in% c("glide_act","glide_pass")),]
highSp_segments_soaring <- df_segments[which(highSp_segments$kmeanSoarAct %in% c("soar_act","soar_pass")),]

#Some checks
summary(LowSp_segments$grSpeed.mean)
summary(highSp_segments$grSpeed.mean)
nrow(LowSp_segments)==length(unique(LowSp_segments$behavID_SoarAct))

#Look at distribution of response variables in the two sub-datasets (high and low speed segments)
hist(LowSp_segments$vedbaAvg_smooth3s_move.mean, breaks = "FD")
hist(highSp_segments$vedbaAvg_smooth3s_move.mean, breaks = "FD")

hist(LowSp_segments$height.above.msl.max, breaks = "FD")
hist(highSp_segments$height.above.msl.max, breaks = "FD")

hist(highSp_segments$glideRatio.segm, breaks="FD")
hist(sqrt(highSp_segments$glideRatio.segm), breaks="FD")
summary(highSp_segments$glideRatio.segm)

#Save separated datasets that we will use in the models
save(LowSp_segments,highSp_segments, file="Dataset/ModelDatasets_PerSegment_LOWvsHIGHspeeds.rdata")

#_______________________
# MODELS TREATMENT ####
#_______________________

library(lme4)
library(EnvStats) #for boxcox function
library(lattice)
library(lmerTest)
library(lubridate)
library(ggplot2)
library(ggResidpanel)
library(MuMIn)
library(stargazer)
library(sp)
library(geosphere)
source("/home/mscacco/ownCloud/Martina/PHD/R_functions/standard_error.R") #function stderr

# importa i dataset differenziati: uno per segmenti di volo con ground speed >4, l'altro per quelli <4 m/s
setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment") #on Martina's computer
load("Dataset/ModelDatasets_PerSegment_LOWvsHIGHspeeds.rdata") #objects LowSp_segments,highSp_segments

# Exclude data related to Hercule (the control individual)
# All other filters were applied earlier (see script "FilterSegmentDataset_PlotLowSpeedSegments_martina.R")
highSp_segments <- highSp_segments[which(highSp_segments$harnessExp %in% c("treatment")),]
nrow(highSp_segments)

# aggiunta variabile in cui viene indicato l'ordine della sequenza dei segmenti (utilizzato per ridurre l'autocorrelazione)
ls <- split(highSp_segments, highSp_segments$tag_session_id)
highSp_segments <- do.call(rbind, lapply(ls, function(x) {
  x <- x[order(x$start.timestamp),] #ordina temporalmente
  x$segment.from.start<-1:nrow(x)
  return(x)
}))

# Range of distances (in metres) from the releasing point
#park <- c(44.801962, 1.612855)
# Calculate distance of all segments from release point (~ first segment of each track)
ls <- split(highSp_segments, highSp_segments$tag_session_id)
dists <- do.call(c, lapply(ls, function(x){
  return(distVincentyEllipsoid(p1=x[,c("Longitude.mean","Latitude.mean")], p2=x[1,c("Longitude.mean","Latitude.mean")]))
}))
summary(dists/1000) # in Km
mean(dists); stderr(dists) # in metres

# nb. stargazer non funziona con il package lmertester

# High speed segments
highSp_segments$start.timestamp <- as.POSIXct(strptime(highSp_segments$start.timestamp,
                                                 format="%Y-%m-%d %H:%M:%S", tz="UTC"))
highSp_segments$hour<-as.numeric(hour(highSp_segments$start.timestamp))
# Centre the variable hour and calculate the quadratic term
mean(highSp_segments$hour)
highSp_segments$hourScale <- highSp_segments$hour-12
# Hour centred at 12:00
highSp_segments$hourQ <- (highSp_segments$hourScale)^2

table(highSp_segments$harnessExp)
table(highSp_segments$harness_type)
length(unique(highSp_segments$tag_session_id))
length(unique(highSp_segments$individual))


table(highSp_segments$kmeanSoar)
table(highSp_segments$kmeanAct)


## Order by tag session and time
highSp_segments <- highSp_segments[order(highSp_segments$tag_session_id,highSp_segments$start.timestamp),]

## Calculate airspeed (I should do that in the fix by fix dataset, before creating the segment dataset)
#highSp_segments$airSpeed <- 
  
  
## Split doaring and gliding
highSp_segmentsSoar <- highSp_segments[which(highSp_segments$kmeanSoar=="soar"),]
highSp_segmentsGlide <- highSp_segments[which(highSp_segments$kmeanSoar=="glide"),]

boxplot(highSp_segments$segmDuration~highSp_segments$kmeanSoar)
summary(highSp_segmentsSoar$segmDuration)
stderr(highSp_segmentsSoar$segmDuration)
summary(highSp_segmentsGlide$segmDuration)
stderr(highSp_segmentsGlide$segmDuration)

# VERTICAL SPEED MODEL
# _____________________________
boxplot(vertSpeed.mean~harness_type,
        ylab="Mean vertical speed (m/s)",
        col=c("chocolate","lightblue"),
        data=highSp_segments)
# outline=F rimuove gli outliers
hist(highSp_segments$vertSpeed.mean, breaks="FD")
boxplot(vertSpeed.mean~hour, data=highSp_segments)

# #power test
# dfNum <- 5 # n. predictors - 1
# dfDen <- 2134 # n. obs - n. pred
# f2 <- 
# quantile(highSp_segmentsSoar$vertSpeed.mean, seq(0,1,0.01))
# vertSpeed_var_soar <- highSp_segmentsSoar$vertSpeed.mean[-which.min(highSp_segmentsSoar$vertSpeed.mean)]
# bcS <- boxcox(vertSpeed_var_soar+abs(min(vertSpeed_var_soar))+0.001, optimize = TRUE)
# bcS$lambda

vertSpeed_soar <- lmer(vertSpeed.mean~
                      #(vertSpeed_var_soar+abs(min(vertSpeed_var_soar))+0.001)^bcS$lambda~
                       harness_type*species_id+hourScale+
                        nFixes+#segment.from.start+
                        (1|individual)+(1|Date),
                        data=highSp_segmentsSoar)#[-which.max(highSp_segmentsSoar$vertSpeed.mean),])
summary(vertSpeed_soar, correlation=F)
anova(vertSpeed_soar)
hist(residuals(vertSpeed_soar))
r.squaredGLMM(vertSpeed_soar)
acf(residuals(vertSpeed_soar, retype="normalized"))
plot(vertSpeed_soar, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vertSpeed_soar)
# Check for the overall effect of the species*harness interaction
vertSpeed_soar2 <- lmer(vertSpeed.mean~
                         #(vertSpeed_var_soar+abs(min(vertSpeed_var_soar))+0.001)^bcS$lambda~
                          harness_type+species_id+hourScale+
                         nFixes+#segment.from.start+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsSoar)#[-which.max(highSp_segmentsSoar$vertSpeed.mean),])
anova(vertSpeed_soar, vertSpeed_soar2)
# The interaction is significant, so both harness type and interaction species*harness are kept in the model
# Legloop birds have higher climbing rate


quantile(highSp_segmentsGlide$vertSpeed.mean, seq(0,1,0.01))
range(highSp_segmentsGlide$vertSpeed.mean[-which.min(highSp_segmentsGlide$vertSpeed.mean)]) # Remove lowest value
# vertSpeed_var_glide <- highSp_segmentsGlide$vertSpeed.mean[-which.min(highSp_segmentsGlide$vertSpeed.mean)]
# bcG <- boxcox(vertSpeed_var_glide+abs(min(vertSpeed_var_glide))+0.001, optimize = TRUE)
# bcG$lambda

vertSpeed_glide <- lmer(vertSpeed.mean~
                        #(vertSpeed_var_glide+abs(min(vertSpeed_var_glide))+0.001)^bcG$lambda~
                         harness_type*species_id+hourScale+
                         nFixes+#segment.from.start+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsGlide[-which.min(highSp_segmentsGlide$vertSpeed.mean),])
summary(vertSpeed_glide, correlation=F)
# Check for the overall effect of the species*harness interaction
vertSpeed_glide2 <- lmer(vertSpeed.mean~
                           #(vertSpeed_var_glide+abs(min(vertSpeed_var_glide))+0.001)^bcG$lambda~
                           harness_type+species_id+hourScale+
                           nFixes+#segment.from.start+
                           (1|individual)+(1|Date),
                         data=highSp_segmentsGlide[-which.min(highSp_segmentsGlide$vertSpeed.mean),])
anova(vertSpeed_glide,vertSpeed_glide2)
summary(vertSpeed_glide2, correlation=F)
hist(residuals(vertSpeed_glide2))
r.squaredGLMM(vertSpeed_glide2)
acf(residuals(vertSpeed_glide2, retype="normalized"))
plot(vertSpeed_glide2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vertSpeed_glide2)

# Since it's not significant, check for overall effect of the harness type
vertSpeed_glide3 <- lmer(vertSpeed.mean~
                           #(vertSpeed_var_glide+abs(min(vertSpeed_var_glide))+0.001)^bcG$lambda~
                           species_id+hourScale+
                           nFixes+#segment.from.start+
                           (1|individual)+(1|Date),
                         data=highSp_segmentsGlide[-which.min(highSp_segmentsGlide$vertSpeed.mean),])
anova(vertSpeed_glide3,vertSpeed_glide2)
# The harness type is slightly significant! Lower sink speed when gliding!

# Backtransformation
# DFpred_WS$segmSpeed <- (predict(mod1$gam,newdata=DFpred_WS, se.fit=T)[[1]])^(1/lambda) #if lambda is positive
# DFpred$Pred.y <- 1/((predict(mod3$gam,newdata=DFpred, se.fit=T)[[1]])^(1/abs(lambda_neg))) #if lambda is negative


# GROUND (HORIZONTAL) SPEED MODEL
# ________________________________
boxplot(grSpeed.mean~harness_type,
        ylab="Mean ground speed (m/s)",
        col=c("chocolate","lightblue"),
        data=highSp_segments)
boxplot(grSpeed.mean~hour, data=highSp_segments)
# hourPoly <- poly(highSp_segments$hour, 2)
# hourPoly.l <- hourPoly[,1]
# hourPoly.q <- hourPoly[,2]
hist(sqrt(highSp_segments$grSpeed.mean), breaks="FD")

# bcS <- boxcox(highSp_segmentsSoar$grSpeed.mean, optimize = TRUE)
# bcS$lambda
grSpeed_soar <- lmer(sqrt(grSpeed.mean)~#^bcS$lambda
                         harness_type*species_id+hourScale+
                         nFixes+#segment.from.start+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsSoar)#highSp_segmentsSoar)
summary(grSpeed_soar, correlation=F)
# Check for the overall effect of the species*harness interaction
grSpeed_soar2 <- lmer(sqrt(grSpeed.mean)~
                       harness_type+species_id+hourScale+
                       nFixes+#segment.from.start+
                       (1|individual)+(1|Date),
                     data=highSp_segmentsSoar)
anova(grSpeed_soar, grSpeed_soar2)
summary(grSpeed_soar2, correlation=F)
# Since it's not significant, check for overall effect of the harness type
grSpeed_soar3 <- lmer(sqrt(grSpeed.mean)~
                        species_id+hourScale+
                        nFixes+#segment.from.start+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsSoar)
anova(grSpeed_soar3, grSpeed_soar2)
# The harness type is significant! Legloop birds have faster forward speed when soaring
hist(residuals(grSpeed_soar2))
r.squaredGLMM(grSpeed_soar2)
acf(residuals(grSpeed_soar2, retype="normalized"))
plot(grSpeed_soar2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(grSpeed_soar2)
# Backtransformation of prediction for square root 
modcoefs <- coef(summary(grSpeed_soar2))[,"Estimate"]
int <- (modcoefs["(Intercept)"])^2 #intercept ground speed for black kite (reference species)
ll <- (sum(modcoefs[c("(Intercept)","harness_typeLegLoop")]))^2 #ground speed with effect of legloop
ll-int #increase (difference) in ground speed with effect of legloop for the reference species (since there is no itneraction this difference should be the same for all species)
int <- (sum(modcoefs[c("(Intercept)","species_idVF")]))^2 #intercept ground speed for black kite (reference species)
ll <- (sum(modcoefs[c("(Intercept)","species_idVF","harness_typeLegLoop")]))^2 #ground speed with effect of legloop
ll-int #Backtransformed prediction for griffon vulture

grSpeed_glide <- lmer(grSpeed.mean~
                          harness_type*species_id+hourScale+
                          nFixes+#segment.from.start+
                          (1|individual)+(1|Date),
                        data=highSp_segmentsGlide)
summary(grSpeed_glide, correlation=F)
hist(residuals(grSpeed_glide))
r.squaredGLMM(grSpeed_glide)
acf(residuals(grSpeed_glide, retype="normalized"))
plot(grSpeed_glide, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(grSpeed_glide)

# Check for the overall effect of the species*harness interaction
grSpeed_glide2 <- lmer(grSpeed.mean~
                        harness_type+species_id+hourScale+
                        nFixes+#segment.from.start+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsGlide)
anova(grSpeed_glide, grSpeed_glide2)
summary(grSpeed_glide2)
# The interaction is significant! Both harness type and species*harness are kept in the model.
# Legloop birds have lower forward speed while gliding (so backpack maybe higher forward speed to compensate faster sinking speed?)

# Difference between ll and bp forward speed ordered by wing loading
library(ggplot2)
orderSp <- factor(highSp_segmentsGlide$species_id, levels=c("BK","RAV","VR","VF","VH"))
ggplot(highSp_segmentsGlide, aes(x=orderSp, y=grSpeed.mean, fill=harness_type)) +
  geom_boxplot()

orderSp <- factor(highSp_segmentsSoar$species_id, levels=c("BK","RAV","VR","VF","VH"))
ggplot(highSp_segmentsSoar, aes(x=orderSp, y=grSpeed.mean, fill=harness_type)) +
  geom_boxplot()


# GLIDE RATIO MODEL
# _____________________________
boxplot(glideRatio.segm~harness_type,
        ylab="Glide ratio",
        col=c("chocolate","lightblue"),
        data=highSp_segments)
boxplot(glideRatio.segm~hour, data=highSp_segments)
# hourPoly <- poly(highSp_segments$hour, 2)
# hourPoly.l <- hourPoly[,1]
# hourPoly.q <- hourPoly[,2]
hist(sqrt(highSp_segments$glideRatio.segm), breaks="FD")

summary(highSp_segmentsGlide$vertSpeed.mean[which(highSp_segmentsGlide$glideRatio.segm > 100)])
summary(highSp_segmentsGlide$vertSpeed.mean[which(highSp_segmentsGlide$glideRatio.segm < 100)])
summary(highSp_segmentsGlide$segmDuration[highSp_segmentsGlide$glideRatio.segm > 100])
summary(highSp_segmentsGlide$vertDist.cum[highSp_segmentsGlide$glideRatio.segm > 100])
summary(highSp_segmentsGlide$horizDist.cum[highSp_segmentsGlide$glideRatio.segm > 100])

summary(highSp_segmentsGlide$glideRatio.segm[highSp_segmentsGlide$vertSpeed.mean < -0.2])
summary(highSp_segmentsGlide$glideRatio.segm[highSp_segmentsGlide$vertSpeed.mean > -0.1 & highSp_segmentsGlide$vertSpeed.mean < -0.01])
summary(highSp_segmentsGlide$vertDist.cum[highSp_segmentsGlide$vertSpeed.mean > -0.2 & highSp_segmentsGlide$vertSpeed.mean < -0.01])

highSp_segmentsGlide_sub <- highSp_segmentsGlide[highSp_segmentsGlide$vertSpeed.mean < -0.2,]
hist(highSp_segmentsGlide_sub$glideRatio.segm, breaks="FD", xlim=c(0,60))
hist(sqrt(highSp_segmentsGlide_sub$glideRatio.segm), breaks="FD")

png("Boxplots/VertDistVShorizDist_perHarnessType.png", width=7, height=6, units="in", res=500)
#pdf("Boxplots/VertDistVShorizDist_perHarnessType.pdf", 7, 6)
ggplot(aes(y = vertDist.cum, x = horizDist.cum), data = highSp_segmentsGlide_sub) + 
  geom_point(aes(color=harness_type), alpha=0.8) + 
  ylim(-1200,0) + xlim(0,7500) +#shape=harness_type, 
  scale_color_manual(values=c("darkorange","dodgerblue"), name="Harness type") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\nCumulative horizontal distance [m]") + ylab("Cumulative vertical distance [m]\n") +
  #scale_x_discrete(labels=c("control" = "Control group", "treatment" = "Treatment group")) +
  theme(legend.position = c(0.87,0.87), 
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key=element_blank(),
    legend.title = element_text(size=10),
    legend.text = element_text(size=8),
    axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
    axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))
dev.off()

glRatio_glide <- lmer(sqrt(glideRatio.segm)~
                        harness_type*species_id+hourScale+
                        nFixes+#vedbaAvg_smooth05s_flap.mean+#height.above.msl.max+#segment.from.start+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsGlide_sub)
summary(glRatio_glide, correlation=F)
hist(residuals(glRatio_glide))
r.squaredGLMM(glRatio_glide)
acf(residuals(glRatio_glide, retype="normalized"))
plot(glRatio_glide, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(glRatio_glide)

# Check for the overall effect of the species*harness interaction
glRatio_glide2 <- lmer(sqrt(glideRatio.segm)~
                         harness_type+species_id+hourScale+
                         nFixes+#segment.from.start+
                         (1|individual)+(1|Date),
                       data=highSp_segmentsGlide_sub)
anova(glRatio_glide, glRatio_glide2)
summary(glRatio_glide2)
# Since it's not significant, check for overall effect of the harness type
glRatio_glide3 <- lmer(sqrt(glideRatio.segm)~
                        species_id+hourScale+
                        nFixes+#segment.from.start+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsGlide_sub)
anova(glRatio_glide3, glRatio_glide2)
# The harness type is significant! Legloop birds have faster forward speed when soaring
hist(residuals(glRatio_glide2))
r.squaredGLMM(glRatio_glide2)
acf(residuals(glRatio_glide2, retype="normalized"))
plot(glRatio_glide2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(glRatio_glide2)
# Backtransformation of prediction for square root 
modcoefs <- coef(summary(glRatio_glide2))[,"Estimate"]
int <- (modcoefs["(Intercept)"])^2 #intercept glide ratio
ll <- (sum(modcoefs[c("(Intercept)","harness_typeLegLoop")]))^2 #glide ratio with effect of legloop
ll-int #increase (difference) in glide ratio with effect of legloop, which means 1 m higher horizontal distance per unit of drop



# HEIGHT ABOVE SEA MODEL
# _________________________
# Above sea level might be better than above ground level since we are using average/maximum values per segment
# Calculating height above ground would add a layer of inaccuracy due to the fact that the DEM value used to calculate it
# is the one associated to the centroid of the segment, which reliability also depend on the length and area that the segment occupies.
boxplot(height.above.msl.max~harness_type,
        ylab="Max height above ground level (m)",
        col=c("chocolate","lightblue"),
        data=highSp_segments)

boxplot(height.above.msl.max~harness_type, data=highSp_segments[highSp_segments$kmeanSoar=="glide",])
boxplot(height.above.msl.max~harness_type, data=highSp_segments[highSp_segments$kmeanSoar=="soar",])
hist(log(highSp_segments$height.above.msl.max), breaks="FD")
boxplot(height.above.msl.max~hour, data=highSp_segments[highSp_segments$kmeanSoar=="soar",])
hist(log(highSp_segments$height.above.msl.max[which(highSp_segments$kmeanSoar=="glide")]), breaks="FD")
hist(log(highSp_segments$height.above.msl.max[which(highSp_segments$kmeanSoar=="soar")]), breaks="FD")

# Failed attempt to account for autocorrelation by including this timeDiff variable, or time
# timeDiff <- as.numeric(highSp_segmentsSoar$timestamp[-1]-highSp_segmentsSoar$timestamp[-nrow(highSp_segmentsSoar)])/60
# Try to subsample
highSp_segmentsSoar_sub <- highSp_segmentsSoar[seq(1, nrow(highSp_segmentsSoar), by=2),]

height_soar <- lmer(log(height.above.msl.max)~
                      harness_type*species_id+hourScale+
                      nFixes+#segment.from.start+
                      (1|individual)+(1|Date),
                    data=highSp_segmentsSoar_sub)
summary(height_soar, correlation=F)
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
# Since it's slightly significant we also check for the overall effect of harness type
height_soar3 <- lmer(log(height.above.msl.max)~
                       species_id+hourScale+
                       nFixes+#segment.from.start+
                       (1|individual)+(1|Date),
                     data=highSp_segmentsSoar_sub)
anova(height_soar3, height_soar2)
# Which is highly significant, legloop birds reach higher altitudes during soaring

highSp_segmentsGlide_sub <- highSp_segmentsGlide[seq(1, nrow(highSp_segmentsGlide), by=2),]

height_glide <- lmer(log(height.above.msl.max)~
                       harness_type*species_id+hourScale+
                       nFixes+#segment.from.start+
                       (1|individual)+(1|Date),
                     data=highSp_segmentsGlide_sub)
summary(height_glide, correlation=F)
# Check for the overall effect of the species*harness interaction
height_glide2 <- lmer(log(height.above.msl.max)~
                       harness_type+species_id+hourScale+
                       nFixes+#segment.from.start+
                       (1|individual)+(1|Date),
                     data=highSp_segmentsGlide_sub)
anova(height_glide, height_glide2)
summary(height_glide2, correlation=F)
hist(residuals(height_glide2))
r.squaredGLMM(height_glide2)
acf(residuals(height_glide2, retype="normalized"))
plot(height_glide2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(height_glide2)
# Since the interaction is not significant we also check for the overall effect of harness type
height_glide3 <- lmer(log(height.above.msl.max)~
                        species_id+hourScale+
                        nFixes+#segment.from.start+
                        (1|individual)+(1|Date),
                      data=highSp_segmentsGlide_sub)
anova(height_glide3, height_glide2)
# The effect of harness type is significant and legloop birds reach higher heights during gliding


# VEDBA MODEL
# _____________________________
boxplot(vedbaAvg_smooth05s_flap.mean~harness_type,
        ylab="Mean VeDBA (g)",
        col=c("chocolate","lightblue"),
        data=highSp_segments)

hist(log(highSp_segments$vedbaAvg_smooth05s_flap.mean), breaks="FD")
boxplot(vedbaAvg_smooth05s_flap.mean~hour, data=highSp_segments)

table(highSp_segments$kmeanAct)
table(highSp_segments$Date[which(highSp_segments$kmeanAct=="act")],
      highSp_segments$harness_type[which(highSp_segments$kmeanAct=="act")],
      highSp_segments$species[which(highSp_segments$kmeanAct=="act")])
# Only 62 active observations, so we only work on passive

# Subsample to remove some temporal autocorrelation
highSp_segments_pass_sub <- highSp_segments[which(highSp_segments$kmeanAct=="pass"),]
highSp_segments_pass_sub <- highSp_segments_pass_sub[seq(1, nrow(highSp_segments_pass_sub), by=2),]
vedba_pass <- lmer(log(vedbaAvg_smooth05s_flap.mean)~
                     harness_type*species_id+hourScale+
                     nFixes+#segment.from.start+
                     (1|individual)+(1|Date), #+(1|ACC_type) the acc type effect should already be absorbed by the combination of individual + date cause the type of ACC was consistent for the same individual during the same day
                   data=highSp_segments_pass_sub) #highSp_segments[which(highSp_segments$kmeanAct=="pass"),])
summary(vedba_pass, correlation=F)
hist(residuals(vedba_pass))
r.squaredGLMM(vedba_pass)
acf(residuals(vedba_pass, retype="normalized"))
plot(vedba_pass, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vedba_pass)
# Check for the overall effect of the species*harness interaction
vedba_pass2 <- lmer(log(vedbaAvg_smooth05s_flap.mean)~
                     harness_type+species_id+hourScale+
                     nFixes+#segment.from.start+
                     (1|individual)+(1|Date),
                   data=highSp_segments_pass_sub)#highSp_segments[which(highSp_segments$kmeanAct=="pass"),])
anova(vedba_pass, vedba_pass2)
summary(vedba_pass2, correlation=F)
hist(residuals(vedba_pass2))
r.squaredGLMM(vedba_pass2)
acf(residuals(vedba_pass2, retype="normalized"))
plot(vedba_pass2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
qqmath(vedba_pass2)
# The effect of the interaction is slightly significant, let's check the effect of harness type
# not significant effect on vultures, but tawny eagles (RAV) with legloop spend more energy (0.3% more?)
vedba_pass3 <- lmer(log(vedbaAvg_smooth05s_flap.mean)~
                      species_id+hourScale+
                      nFixes+#segment.from.start+
                      (1|individual)+(1|Date),
                    data=highSp_segments_pass_sub)#highSp_segments[which(highSp_segments$kmeanAct=="pass"),])
anova(vedba_pass3, vedba_pass2)
# Legloop is highly significant, so we keep that without interaction
# Legloop individuals spend 0.08% less energy than backpack individuals

# With log transformation coefficient are interpreted as percentages of change in the response variable








#___________________________________
# T-TEST for CONTROL - HERCULE ####
#___________________________________

#mypath <- "C:/Users/arian/Desktop/MPI/Rocamadour data/"
mypath <- "/home/mscacco/ownCloud/Martina/ProgettiVari/Rocamadour/Arianna_HarnessExperiment" #on Martina's computer
(setwd(mypath))

## Segment dataset

load("Dataset/ModelDatasets_PerSegment_LOWvsHIGHspeeds.rdata") #objects LowSp_segments,highSp_segments

# Exclude treatment
control <- highSp_segments[which(highSp_segments$harnessExp %in% c("control")),]
nrow(control)

head(control$timestamp)
control$timestamp <- as.POSIXct(control$start.timestamp,format="%Y-%m-%d %H:%M:%S", tz="UTC")

control <- control[order(control$harness_type,control$start.timestamp),]

table(control$harness_type, control$kmeanSoar)
table(control$harness_type, control$kmeanAct)

table(control$harnessExp)
table(control$harness_type)
length(unique(control$tag_session_id))
length(unique(control$individual))

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
library(wmwpow) #package for power analysis of wilcoxon test
library(pwr) #package for power analysis of t-test

# Vertical speed
#________________
#power analysis, find out the power of the test, and the sample size that would be needed to find significance
ef_vs <- abs(mean(control$vertSpeed.mean[control$harness_type=="LegLoop"]) -  #effect size of the variable (Cohen)
            mean(control$vertSpeed.mean[control$harness_type=="Backpack"]))/sd(control$vertSpeed.mean)
pwr.t.test(n=18, d=ef_vs, sig.level=0.05, alternative="two.sided") #find power
pwr.t.test(d=ef_vs, sig.level=0.05, alternative="two.sided", power=0.8) #find sample size for power=0.8
shiehpow(n=18, m=19, p=ef_vs, alpha=0.05, dist="norm", sides="two.sided")

boxplot(vertSpeed.mean~harness_type*kmeanSoar,
        ylab="Mean vertical speed (m/s)",
        col=c("chocolate","lightblue"),
        data=control)
wilcox.test(control$vertSpeed.mean~control$harness_type,
            alternative = "two.sided")
control$vertSpeed.mean[control$harness_type!="LegLoop"]
control$vertSpeed.mean[control$harness_type=="LegLoop"]

# Ground speed
#________________
ef_gs <- abs(mean(control$grSpeed.mean[control$harness_type=="LegLoop"]) -  #effect size of the variable (Cohen)
            mean(control$grSpeed.mean[control$harness_type=="Backpack"]))/sd(control$grSpeed.mean)
pwr.t.test(n=18, d=ef_gs, sig.level=0.05, alternative="two.sided") #find power
pwr.t.test(d=ef_gs, sig.level=0.05, alternative="two.sided", power=0.8) #find sample size for power=0.8
shiehpow(n=18, m=19, p=ef_gs, alpha=0.05, dist="norm", sides="two.sided")

boxplot(grSpeed.mean~harness_type*kmeanSoar,
        ylab="Mean ground speed (m/s)",
        col=c("chocolate","lightblue"),
        data=control)
wilcox.test(control$grSpeed.mean~control$harness_type,
            alternative = "two.sided")

# Glide ratio
#________________
boxplot(glideRatio.segm~harness_type,
        ylab="Mean ground speed (m/s)",
        col=c("chocolate","lightblue"),
        ylim=c(0,60), data=control[control$kmeanSoar=="glide" & control$vertSpeed.mean < 0,])
wilcox.test(glideRatio.segm~harness_type,
            data=control[control$kmeanSoar=="glide" & control$vertSpeed.mean < 0,],
            alternative = "two.sided")

# Height a.s.l.
#________________
ef_height <- abs(mean(control$height.above.msl.max[control$harness_type=="LegLoop"]) -  #effect size of the variable (Cohen)
                   mean(control$height.above.msl.max[control$harness_type=="Backpack"]))/sd(control$height.above.msl.max)
pwr.t.test(n=18, d=ef_height, sig.level=0.05, alternative="two.sided") #find power
pwr.t.test(d=ef_height, sig.level=0.05, alternative="two.sided", power=0.8) #find sample size for power=0.8
shiehpow(n=18, m=19, p=ef_height, alpha=0.05, dist="norm", sides="two.sided")

boxplot(height.above.msl.max~harness_type*kmeanSoar,
        ylab="Max height above sea level (m)",
        col=c("chocolate","lightblue"),
        data=control)

wilcox.test(control$height.above.msl.max~control$harness_type,
            alternative = "two.sided")
control$height.above.msl.max[control$harness_type!="LegLoop"]
control$height.above.msl.max[control$harness_type=="LegLoop"]

boxplot(height.above.ground.max~harness_type, data=control[control$kmeanSoar=="glide",])
boxplot(height.above.ground.max~harness_type, data=control[control$kmeanSoar=="soar",])


# VeDBA
#_______
ef_vedba <- abs(mean(control$vedbaAvg_smooth05s_flap.mean[control$harness_type=="LegLoop"]) -  #effect size of the variable (Cohen)
            mean(control$vedbaAvg_smooth05s_flap.mean[control$harness_type=="Backpack"]))/sd(control$vedbaAvg_smooth05s_flap.mean)
pwr.t.test(n=18, d=ef_vedba, sig.level=0.05, alternative="two.sided") #find power
pwr.t.test(d=ef_vedba, sig.level=0.05, alternative="two.sided", power=0.8) #find sample size for power=0.8
shiehpow(n=18, m=19, p=ef_vedba, alpha=0.05, dist="norm", sides="two.sided")

boxplot(vedbaAvg_smooth05s_flap.mean~harness_type*kmeanAct,
        ylab="Mean VeDBA (g)",
        col=c("chocolate","lightblue"),
        data=control)
wilcox.test(control$vedbaAvg_smooth05s_flap.mean~control$harness_type,
            alternative = "two.sided")




# ### dataset di fixes
# load("Datasets/completeDF_allFlightSessions_allDays_GPS&burstACC_behavClassif_ID.RData")
# #dataset principale completo, da utilizzare per i plot:
# dati<-all_classif_summ_behavID
# rm(all_classif_summ_behavID)
# 
# dati$trunc_timestamp <- as.POSIXct(strptime(dati$trunc_timestamp,format="%Y-%m-%d %H:%M:%S", tz="UTC"))
# 
# control <- dati[which(dati$harnessExp %in% c("control")),]
# control <- control[which(!control$tag_session_id %in% c('2018-07-01_gps3_acc3_flight1','2018-06-29_gps14_acc2_flight2','2018-07-01_gps4_acc4_flight1','2018-06-26_gps4_acc4_flight2')),]
# control <- control[order(control$tag_session_id, control$harness_type, control$Timestamp),]
# 
# flight_ls <- lapply(split(control, control$flightSession), function(x)split(x, x$harness_type))
# 
# uno <- flight_ls[[1]]
# lapply(uno, function(x)range(x$Timestamp))
# plot(Latitude~Longitude, data=uno[[1]], type='l')
# plot(Latitude~Longitude, data=uno[[2]], type='l')
# table(uno[[1]]$kmeanSoarAct)
# 
# library(rgl)
# i=2
# open3d()
# plot3d(x=uno[[i]]$Longitude, y=uno[[i]]$Latitude, z=uno[[i]]$height.above.msl,
#        xlab="", ylab="", zlab="",
#        type="l",
#        col=c("red","black","lightblue","blue")[uno[[i]]$kmeanSoarAct])
# 
# due <- flight_ls[[2]]
# lapply(due, function(x)range(x$Timestamp))
# plot(Latitude~Longitude, data=due[[1]], type='l')
# plot(Latitude~Longitude, data=due[[2]], type='l')
# table(due[[2]]$kmeanSoarAct)
# 
# segments_ls <- split(control, control$behavID_SoarAct)
# #Filter segments longer than 5 fixes
# segmentLength <- which(sapply(segments_ls, nrow)>5)
# segments_ls <- segments_ls[segmentLength]
# 
# 
# #seg <- segments_ls[[1]] #applica a un solo segmento per test
# summaryFunction <- function(seg){
#   uniqueCols <- t(unlist(apply(seg[,c(2,39:48,52:58,60:63)], 2, FUN=unique)))
#   meanCols <- t(unlist(apply(seg[,c("Longitude","Latitude","height.above.msl","grSpeed","vertSpeed","odbaAvg_smooth3s","vedbaAvg_smooth3s_move","vedbaAvg_smooth05s_flap","vedbaAvg_smooth025s_noise","vedbaCum_smooth025s_noise")], 2, FUN=mean, na.rm=T)))
#   colnames(meanCols) <- paste0(colnames(meanCols),".mean")
#   segmDf <- data.frame(start.timestamp=min(seg$trunc_timestamp),
#                        end.timestamp=max(seg$trunc_timestamp),
#                        uniqueCols, 
#                        meanCols,
#                        height.above.msl.max=max(seg$height.above.msl, na.rm=T),
#                        nFixes=nrow(seg))
#   return(segmDf)
# }

## BOXPLOTS with both Treatment and Control ####
#_______________________________________________

load("Dataset/ModelDatasets_PerSegment_LOWvsHIGHspeeds_env.rdata") #objects LowSp_segments,highSp_segments

highSp_segments$harness_type <- factor(highSp_segments$harness_type, levels=c("Backpack","LegLoop"))

library(ggplot2)
vs <- ggplot(aes(y = vertSpeed.mean, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue"), name="Harness type") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Mean vertical speed [m/s]\n") +
  scale_x_discrete(labels=c("control" = "Control group", "treatment" = "Treatment group")) +
  theme(legend.position = c(0.15,0.85), 
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key=element_blank(),
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))
        #panel.background = element_rect(fill = "transparent", colour = NA), 
        #plot.background = element_rect(fill = "transparent", colour = NA),
        #legend.background = element_rect(fill = "transparent", colour = NA),
        #legend.key=element_blank(),
        #panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        # legend.title = element_text(colour="ghostwhite"), legend.text = element_text(colour="ghostwhite"),
        # axis.line = element_line(size = 1, linetype = "solid",colour = "ghostwhite"),
        # axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        # axis.ticks = element_line(colour="ghostwhite"),
        # axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

gs <- ggplot(aes(y = grSpeed.mean, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Mean horizontal speed [m/s]\n") +
  scale_x_discrete(labels=c("control" = "Control group", "treatment" = "Treatment group")) +
  theme(legend.position = "none",
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

vedba <- ggplot(aes(y = vedbaAvg_smooth05s_flap.mean, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Mean VeDBA [g]\n") +
  scale_x_discrete(labels=c("control" = "Control group", "treatment" = "Treatment group")) +
  theme(legend.position = "none",
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

height <- ggplot(aes(y = height.above.msl.max, x = harnessExp, fill = harness_type), data = highSp_segments) + 
  geom_boxplot() +
  scale_fill_manual(values=c("darkorange","dodgerblue")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + ylab("Maximum height a.s.l. [m]\n") +
  scale_x_discrete(labels=c("control" = "Control group", "treatment" = "Treatment group")) +
  theme(legend.position = "none",
        axis.text.x= element_text(colour="black", size=10), axis.text.y= element_text(colour="black", size=10),
        axis.title.x = element_text(colour = "black", size=11.5), axis.title.y = element_text(colour = "black", size=11.5))

library(ggpubr)

pdf("Boxplots/multiboxplot_variables_harnesstype-treatment_newYlab.pdf", width=10, height=9)
png("Boxplots/multiboxplot_variables_harnesstype-treatment_newYlab.png", width=10, height=9, units="in", res=500)
ggarrange(vs,gs,vedba,height, 
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2,
          widths = c(1,1,1,1))
dev.off()



