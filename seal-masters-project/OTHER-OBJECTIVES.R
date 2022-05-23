#########################################
##########Other objectives

##############################################################################
rm(list=ls())

objects()
search()

#packageS

library(lme4)   # need to run GLMM
library(MASS) 	# general package
library(car)  	# genearl package
library(ggplot2)
library(GGally) # for ggpairs
library(lubridate)
#library(pracma)  # for Mode fnc
library(dplyr)

# modelling packages
library(ordinal)
#library(mgcv)
#library(itsadug)
#library(gamm4)
#library(arm)
#library(AICcmodavg)
library(bbmle) #for AICtab
library(MuMIn)# need for AIC comparisons
library(blmeco) # overdispersion function e.g. dispersion_glmer(m.full)  # not for glmmTMB
library(rcompanion)  # does the plotnormal histogram
library(effects)
#library(glmmTMB)
#library(lmtest)
library(rptR)
library(r2glmm)
library(sjPlot)

# SET WORKING DIR
setwd("C:\\Users\\smann\\OneDrive\\Documents\\R\\Seal project\\Final Analysis")


# READ DATA FILEs ====================================================

DF<-read.csv(file = "MERGED-DATASET.csv", header = TRUE, 
             na.strings=c("","NA"))
 
# ====================================================================
# FIRST: # DATA QUALITY CHECKING ------------------------------------------------------
# check dataframe
names(DF)
summary(DF)
str(DF)
head(DF)


# check variable types - set any factors -
# and any ordered factors
## Order levels of the factor; otherwise R will alphabetize them

table(DF$ZOOM.NUM)

DF$PUP.SEX = factor(DF$PUP.SEX,levels=unique(DF$PUP.SEX))
DF$CS = factor(DF$CS,levels=unique(DF$CS))
DF$MUM.LOCN = factor(DF$MUM.LOCN,levels=unique(DF$MUM.LOCN))
DF$SUBSTRATE = factor(DF$SUBSTRATE,levels=unique(DF$SUBSTRATE))
DF$IN.POOL = factor(DF$IN.POOL,levels=unique(DF$IN.POOL))
DF$INPOOL.YN = factor(DF$INPOOL.YN, levels = unique (DF$INPOOL.YN))
DF$MPR.orient = factor(DF$MPR.orient,levels=unique(DF$MPR.orient))
DF$MP.orient = factor(DF$MP.orient,levels=unique(DF$MP.orient))
DF$MPR.orient = factor(DF$MPR.orient,levels=unique(DF$MPR.orient))
DF$RAIN.DRY = factor(DF$RAIN.DRY,levels=unique(DF$RAIN.DRY))
DF$SUN.SHADE = factor(DF$SUN.SHADE,levels=unique(DF$SUN.SHADE))
DF$FOCNUM = factor(DF$FOCNUM,levels=unique(DF$FOCNUM))
DF$FOCAL = factor(DF$FOCAL,levels=unique(DF$FOCAL))
DF$ID = factor(DF$ID,levels=unique(DF$ID))


##ordered factors

DF$TOPO = factor(DF$TOPO,ordered = TRUE)
#DF$YEAR = factor(DF$YEAR,ordered = TRUE)


############################################################################
#DROP ZOOMED OUT RECORDS

DF <- subset(DF, DF$ZOOM.NUM != 99)
################################################################################
## MISSING DATA / NAs ETC

table(DF$MP.PROX)  # LOTS OF POOS WHICH BASICALLY = NA
table(DF$MP.PROX.NUM)
summary(DF$MP.PROX.NUM) # LOTS OF NAS
summary(DF)

# check for missing data
# POOLDIST highest
# then MP.ORIENT, MPR.orient, MP.PROX.NUM, LINE.OF.SIGHT
as.data.frame(sapply(DF, function(x) sum(is.na(x))))

## NOW trim the dataset to only those we need in models

names(DF)

#INCLUDES POOLDIST & MP.PROX.NUM & LINE.OF.SIGHT
DF2 <- select(DF, INDEX, ID, YEAR, DOY, FOCNUM,FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,MP.PROX.NUM,LINE.OF.SIGHT,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY, POOLDIST, IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS)  

# NB: "MP.orient"  &  "MPR.orient"  & MP.PROX.NUM & corraling - do sepaprate analyses 
# with each as a RV and createtheir own DF to test if P/R influences any of these

#check for NAs                                  
as.data.frame(sapply(DF2, function(x) sum(is.na(x)))) 

str(DF2)

#############################################################

names(DF)

# we can reorder the levels here if you want (models use one level of a factor as a baseline
# e.g. for M vs F it will be F (alphabetical) - thats fine - 
# but interprtingr esults can be easier sometimes if we re-order levels)


###########################################################################################
# Z correcting adjusts for  scaling problems: i.e. where different NUMERIC parameters 
# have v different scales
# z correction? (z = (x - mean(x))/sd(x))
DF2$Z_DOY   <- (DF2$DOY - mean(DF2$DOY))/sd(DF2$DOY)

#DF3$Z_MP.PROX.NUM  <- (DF3$MP.PROX.NUM - mean(DF3$MP.PROX.NUM))/sd(DF3$MP.PROX.NUM)

summary(DF2)




#############

# glmm on continous RV e.g mp-prox
# set this so models know how to deal with any NA data
options(na.action = "na.fail")
names(DF2)
as.data.frame(sapply(DF3, function(x) sum(is.na(x)))) 
DF3 <- select(DF2, INDEX, ID, YEAR, DOY, FOCNUM,FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,MP.PROX.NUM,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY,  IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS) 

##MP-PROX WITHOUT POOL
dfmod <- subset(DF3, !is.na(MP.PROX.NUM))
str(dfmod$MP.PROX.NUM)
summary(dfmod$MP.PROX.NUM)  # max is only 4 so may not model well this way - lets see
names(dfmod)
as.data.frame(sapply(dfmod, function(x) sum(is.na(x))))
hist(dfmod$MP.PROX.NUM) # may need to transform
names(dfmod)
  
#FULL MODEL use lmer and reml = false for cf by AIC
MF.PROX <- lmer(MP.PROX.NUM ~ PUP.SEX + PUPAGE + YEAR + 
                  SUN.SHADE + RAIN.DRY +
                    TOPO + SUBSTRATE + NFEM.5M +
                    NFEM.2M + Z_med.rMSSD.adj.RPCALL + 
                    (1|ID) + (1|ID:FOCAL), 
                  data = dfmod, REML = FALSE) 
MF.PROX <- lmer(MP.PROX.NUM ~ PUP.SEX + PUPAGE + YEAR + 
                  #INPOOL.YN +
                  TOPO + SUBSTRATE + NFEM.5M +
                  NFEM.2M + Z_med.rMSSD.adj.RPCALL + 
                  (1|ID) + (1|ID:FOCAL), 
                data = dfmod, REML = FALSE) 
  
summary(MF.PROX)  
dredge(MF.PROX) 

  
#for best model use glmer - and defualt (reml) - best for cf effects
MF.PROX.BEST <- lmer(MP.PROX.NUM ~ PUP.SEX + SUBSTRATE  + (1|ID) + (1|ID:FOCAL), 
                        data = dfmod, 
                        REML = FALSE)
  
summary(MF.PROX.BEST)
Anova(MF.PROX.BEST)

##LINE OF SIGHT
as.data.frame(sapply(DF3, function(x) sum(is.na(x))))
DF3 <- select(DF2, INDEX, ID, YEAR, DOY, FOCNUM,FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,LINE.OF.SIGHT,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY,  IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS) 

dfmod <- subset(DF3, !is.na(LINE.OF.SIGHT))
str(dfmod$LINE.OF.SIGHT)
summary(dfmod$LINE.OF.SIGHT)  # max is only 4 so may not model well this way - lets see
names(dfmod)
as.data.frame(sapply(dfmod, function(x) sum(is.na(x))))
hist(dfmod$LINE.OF.SIGHT) # may need to transform
names(dfmod)

dfmod$LINE.OF.SIGHT <- factor(dfmod$LINE.OF.SIGHT)

MF.LINE.OF.SIGHT <- glmer(LINE.OF.SIGHT ~  NFEM.2M + NFEM.5M + YEAR 
                          #+ INPOOL.YN
                 + PUPAGE + PUP.SEX + TOPO + SUBSTRATE +            
                 + Z_med.rMSSD.adj.RPCALL
                 + (1|ID)+ (1|ID:FOCAL), data = dfmod, 
                 family = binomial (link = "logit"))

summary(MF.LINE.OF.SIGHT)
dredge(MF.LINE.OF.SIGHT)

MF.LINE.OF.SIGHT.BEST <- glmer(LINE.OF.SIGHT ~  SUBSTRATE
                          + (1|ID)+ (1|ID:FOCAL), data = dfmod, 
                          family = binomial (link = "logit"), control = glmerControl(tolPwrss=1e-3))

summary(MF.LINE.OF.SIGHT.BEST)
Anova(MF.LINE.OF.SIGHT.BEST)

##POOL DIST
as.data.frame(sapply(DF3, function(x) sum(is.na(x))))
DF3 <- select(DF2, INDEX, ID, YEAR, DOY, FOCNUM,FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,POOLDIST,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY,  IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS) 

dfmod <- subset(DF3, !is.na(POOLDIST))
str(dfmod$POOLDIST)
summary(dfmod$POOLDIST)  # max is only 4 so may not model well this way - lets see
names(dfmod)
as.data.frame(sapply(dfmod, function(x) sum(is.na(x))))
hist(dfmod$POOLDIST) # may need to transform
names(dfmod)



MF.POOLDIST <- lmer(POOLDIST ~ PUP.SEX + PUPAGE + YEAR +
                  TOPO + SUBSTRATE + RAIN.DRY + SUN.SHADE + NFEM.5M +
                  NFEM.2M + Z_med.rMSSD.adj.RPCALL + 
                  (1|ID) + (1|ID:FOCAL), 
                data = dfmod, REML = FALSE) 

summary(MF.POOLDIST) 
dredge(MF.POOLDIST)

MF.POOLDIST.BEST <- lmer(POOLDIST ~ PUP.SEX + YEAR +
                      TOPO + SUBSTRATE + SUN.SHADE + NFEM.5M +
                      NFEM.2M + 
                      (1|ID)+ (1|ID:FOCAL), 
                    data = dfmod, REML = FALSE) 

summary(MF.POOLDIST.BEST)
Anova(MF.POOLDIST.BEST)


#IN POOL OR NOT
#WHOLE DATASET - BINARY
as.data.frame(sapply(DF3, function(x) sum(is.na(x))))
DF3 <- select(DF2, INDEX, ID, YEAR, DOY, FOCNUM,FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY,  IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS)  
dfmod <- na.omit(DF3)
str(dfmod$INPOOL.YN)
summary(dfmod$POOLDIST)  # max is only 4 so may not model well this way - lets see
names(dfmod)
as.data.frame(sapply(dfmod, function(x) sum(is.na(x))))
hist(dfmod$POOLDIST) # may need to transform
names(dfmod)
dfmod$INPOOL.YN <- factor(dfmod$INPOOL.YN)
MF.IN.POOL <- glmer(INPOOL.YN ~  YEAR
                          + PUPAGE + PUP.SEX + SUN.SHADE + RAIN.DRY             
                            + Z_med.rMSSD.adj.RPCALL + (1|ID)+ (1|ID:FOCAL),
                           data = dfmod, 
                          family = binomial (link = "logit"))
MF.IN.POOL1 <- glmer(INPOOL.YN ~  YEAR
                    + PUPAGE + PUP.SEX + SUN.SHADE + RAIN.DRY           
                      + Z_med.rMSSD.adj.RPCALL
                   , data = dfmod, 
                    family = binomial (link = "logit"), start=list(fixef=coef(MF.IN.POOL)), control=glmerControl(nAGQ0initStep=FALSE))

summary(MF.IN.POOL)
dredge(MF.IN.POOL)

MF.IN.POOL.BEST <- glmer(INPOOL.YN ~  PUP.SEX + PUPAGE + SUN.SHADE            
                       + Z_med.rMSSD.adj.RPCALL
                     + (1|ID), data = dfmod, 
                     family = binomial (link = "logit"))

summary(MF.IN.POOL.BEST)


##CORRAL
DF3 <- select(DF, INDEX, ID, YEAR, DOY, FOCNUM, FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY, IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              MDML.NEW, PDGR.NEW, MTE.NEW,   MPPM.NEW,  MS.MDML.NEW, 
              Z_BDAY.DOY, Z_MDML.NEW, Z_PDGR.NEW,         
              Z_MTE.NEW, Z_MPPM.NEW, Z_MS.MDML.NEW, EST.MAT.MASS,  ### NEW NEW NEW
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS)

#check for NAs                                  
as.data.frame(sapply(dfcorral, function(x) sum(is.na(x)))) 
#Load dataset
CORALLED.DF <-read.csv(file = "Coralled-Data.csv", header = TRUE, 
             na.strings=c("","NA"))

#create FOCAL column
CORALLED.DF$FOCAL <- paste(CORALLED.DF$YY, CORALLED.DF$FOCNUM, sep="_")

#check for wrong focal numbers
names(CORALLED.DF)
CORALLED.DF[duplicated(CORALLED.DF$FOCAL), ]

CORALLED.DF <- select(CORALLED.DF, FOCAL, CORRAL)


#merge datasets by FOCAL
dfcorral <- merge(DF3, CORALLED.DF, by = "FOCAL")
#REMOVE NAS
dfcorral <- na.omit(dfcorral)
#create binomial coralled.yn
dfcorral$CORRAL.YN <- as.factor(ifelse(dfcorral$CORRAL == 0, 0, 1))

#do binomial glmm
MF.CORRAL <- glmer(CORRAL.YN ~  NFEM.2M + NFEM.5M + YEAR
                   + PUPAGE + PUP.SEX + TOPO + SUBSTRATE             
                   + Z_med.rMSSD.adj.RPCALL+ (1|ID)+ (1|ID:FOCAL), data = dfcorral, 
                   family = binomial (link = "logit"), control = glmerControl(tolPwrss=1e-3))

summary(MF.CORRAL)
dredge(MF.CORRAL)


