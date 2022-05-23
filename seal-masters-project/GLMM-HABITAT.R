##################################################
##GLMM for MCA values
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

DF.MCA<-read.csv(file = "MCA-ROT-DATASET.csv", header = TRUE, 
                 na.strings=c("","NA"))

DF <- cbind(DF, ROUGHNESS = DF.MCA$ROUGHNESS, GRASSINESS = DF.MCA$GRASSINESS) 
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
DF$LINE.OF.SIGHT = factor(DF$LINE.OF.SIGHT,levels=unique(DF$LINE.OF.SIGHT))
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

DF2 <- select(DF, INDEX, ID, YEAR, DOY, FOCNUM, FOCAL, Iteration,
              PUPAGE, PUP.SEX,
              ZOOM.NUM, MUM.LOCN,
              NFEM.2M, TOT.SEALS.2M,TOT.SEALS.5M, NFEM.5M,
              SUN.SHADE, RAIN.DRY, IN.POOL, INPOOL.YN, TOPO, SUBSTRATE,
              GRASSINESS, ROUGHNESS,
              MDML.NEW, PDGR.NEW, MTE.NEW,   MPPM.NEW,  MS.MDML.NEW, 
              Z_BDAY.DOY, Z_MDML.NEW, Z_PDGR.NEW,         
              Z_MTE.NEW, Z_MPPM.NEW, Z_MS.MDML.NEW, EST.MAT.MASS,  ### NEW NEW NEW
              med.rMSSD.adj.RPCALL,Z_med.rMSSD.adj.RPCALL, CS)  
# omitted POOLDIST & MP.PROX.NUM,



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

summary(DF2)

# set this so models know how to deal with any NA data
options(na.action = "na.fail")
names(DF2)
#NEED TO REMOVE MISSING DATA ROWS
dfmod <- na.omit(DF2)


#histograms
par(mfrow = c(1,1))
hist(dfmod$GRASSINESS)
hist(dfmod$ROUGHNESS)


MF.GRASSINESS <- lmer(GRASSINESS ~  NFEM.2M + NFEM.5M + YEAR 
                      #+ INPOOL.YN
                      + PUPAGE + PUP.SEX + SUN.SHADE + RAIN.DRY             
                      + Z_med.rMSSD.adj.RPCALL
                      + (1|ID)+ (1|ID:FOCAL),
                      data = dfmod, REML = FALSE)
summary(MF.GRASSINESS)
DREDGEGRASSINESS <- dredge(MF.GRASSINESS)

MF.GRASSINESS.BEST <- lmer(GRASSINESS ~ NFEM.2M + NFEM.5M + PUPAGE + RAIN.DRY + (1|ID)+ (1|ID:FOCAL), 
                           data = dfmod, 
                           #family =  gaussian(link = "identity) - unused argument so removed
                           REML = FALSE)

summary(MF.GRASSINESS.BEST)
Anova(MF.GRASSINESS.BEST)

NULL.GRASSINESS <- lmer(GRASSINESS ~ (1|ID)+ (1|ID:FOCAL), 
                           data = dfmod, 
                           #family =  gaussian(link = "identity) - unused argument so removed
                           REML = FALSE)

summary(NULL.GRASSINESS)
AICctab(MF.GRASSINESS.BEST, NULL.GRASSINESS)

MF.ROUGHNESS <- lmer(ROUGHNESS ~  NFEM.2M + NFEM.5M + YEAR 
                      #+ INPOOL.YN
                      + PUPAGE + PUP.SEX + SUN.SHADE + RAIN.DRY           
                      + Z_med.rMSSD.adj.RPCALL
                      + (1|ID)+ (1|ID:FOCAL),
                      data = dfmod, REML = FALSE)

summary(MF.ROUGHNESS)
DREDGEROUGHNESS <- dredge(MF.ROUGHNESS)
DREDGEROUGHNESS <- as.data.frame(DREDGEROUGHNESS)
MF.ROUGHNESS.BEST <- lmer(ROUGHNESS ~ NFEM.2M + NFEM.5M + PUPAGE + SUN.SHADE + (1|ID)+ (1|ID:FOCAL), 
                          data = dfmod, 
                          #family =  gaussian(link = "identity) - unused argument so removed
                          REML = FALSE)

summary(MF.ROUGHNESS.BEST)
Anova(MF.ROUGHNESS.BEST)

NULL.ROUGHNESS <- lmer(ROUGHNESS ~ (1|ID)+ (1|ID:FOCAL), 
                          data = dfmod, 
                          #family =  gaussian(link = "identity) - unused argument so removed
                          REML = FALSE)

summary(NULL.ROUGHNESS)
AICctab(MF.ROUGHNESS.BEST, NULL.ROUGHNESS)

DFX <- DF2

# IMPORTANT - NEED TO ORDER/SORT THE DATAFRAME BY THE NEW FOCAL NUMBER
DFX <- DFX[order(DFX$FOCAL,DFX$Iteration),]

# 1st calcualte mean/median, max etc values for all parameters that will
# need summarising by FOCNUM 
# (or can do the same by DOY later if need to - simply replace FOCNUMwith DOY in commands below)
# FIRST LETS CREATE A COUNT OF THE NUMBER OF RECORDS IN EACH NEW.FOC 
# THIS WILL BE USEFUL FOR WORKING OUT PROPORTIONS LATER ON
NRECS.FOC <- tapply(DFX$FOCAL , DFX$FOCAL , length)
DFX <- data.frame(DFX, NRECS.FOC=rep(NRECS.FOC, table(DFX$FOCAL)))
# NOW WE CAN COMPUTE THE COUNTS OF OCCURENCES OF E.G. HABITAT TYPES BY NEW.FOC 
DFX$FLAT.YN <- as.integer(ifelse(DFX$TOPO == "FLAT", 1, 0))
CNT.TOPO.FLAT <- tapply(DFX$FLAT.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.TOPO.FLAT=rep(CNT.TOPO.FLAT, table(DFX$FOCAL )))
DFX$CNT.TOPO.NOT.FLAT <- DFX$NRECS.FOC - DFX$CNT.TOPO.FLAT

DFX$MOD.YN <- as.integer(ifelse(DFX$TOPO == "MOD", 1, 0))
CNT.TOPO.MOD <- tapply(DFX$MOD.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.TOPO.MOD=rep(CNT.TOPO.MOD, table(DFX$FOCAL )))
DFX$CNT.TOPO.NOT.MOD <- DFX$NRECS.FOC - DFX$CNT.TOPO.MOD

DFX$ROUGH.YN <- as.integer(ifelse(DFX$TOPO == "ROUGH", 1, 0))
CNT.TOPO.ROUGH <- tapply(DFX$ROUGH.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.TOPO.ROUGH=rep(CNT.TOPO.ROUGH, table(DFX$FOCAL )))
DFX$CNT.TOPO.NOT.ROUGH <- DFX$NRECS.FOC - DFX$CNT.TOPO.ROUGH

DFX$GRASS.YN <- as.integer(ifelse(DFX$SUBSTRATE == "GRASS", 1, 0))
CNT.SUB.GRASS <- tapply(DFX$GRASS.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.SUB.GRASS=rep(CNT.SUB.GRASS, table(DFX$FOCAL )))
DFX$CNT.SUB.NOT.GRASS <- DFX$NRECS.FOC - DFX$CNT.SUB.GRASS

DFX$MUD.YN <- as.integer(ifelse(DFX$SUBSTRATE == "MUD", 1, 0))
CNT.SUB.MUD <- tapply(DFX$MUD.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.SUB.MUD=rep(CNT.SUB.MUD, table(DFX$FOCAL )))
DFX$CNT.SUB.NOT.MUD <- DFX$NRECS.FOC - DFX$CNT.SUB.MUD

DFX$ROCK.YN <- as.integer(ifelse(DFX$SUBSTRATE == "ROCK", 1, 0))
CNT.SUB.ROCK <- tapply(DFX$ROCK.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.SUB.ROCK=rep(CNT.SUB.ROCK, table(DFX$FOCAL )))
DFX$CNT.SUB.NOT.ROCK <- DFX$NRECS.FOC - DFX$CNT.SUB.ROCK

DFX$SUN.YN <- as.integer(ifelse(DFX$SUN.SHADE == "SUN", 1, 0))
CNT.SUN <- tapply(DFX$SUN.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.SUN=rep(CNT.SUN, table(DFX$FOCAL )))
DFX$SUN.PROP <-DFX$CNT.SUN/DFX$NRECS.FOC

DFX$RAIN.YN <- as.integer(ifelse(DFX$RAIN.DRY == "RAIN", 1, 0))
CNT.RAIN <- tapply(DFX$RAIN.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.RAIN=rep(CNT.RAIN, table(DFX$FOCAL )))
DFX$RAIN.PROP <- DFX$CNT.RAIN/DFX$NRECS.FOC

DFX$INPOOL.YN <- as.integer(ifelse(DFX$IN.POOL == "NO", 0, 1))
CNT.INPOOL <- tapply(DFX$INPOOL.YN, DFX$FOCAL , sum)
DFX <- data.frame(DFX, CNT.INPOOL=rep(CNT.INPOOL, table(DFX$FOCAL )))
DFX$INPOOL.PROP <- DFX$CNT.INPOOL/DFX$NRECS.FOC

# example of comuting median value by NEW.FOC 
MED.DENSF2M <- tapply(DFX$NFEM.2M, DFX$FOCAL , median)
DFX <- data.frame(DFX, MED.DENSF2M=rep(MED.DENSF2M, table(DFX$FOCAL )))
MED.DENSF5M <- tapply(DFX$NFEM.5M, DFX$FOCAL , median)
DFX <- data.frame(DFX, MED.DENSF5M=rep(MED.DENSF5M, table(DFX$FOCAL )))

MEAN.DENSF2M <- tapply(DFX$NFEM.2M, DFX$FOCAL , mean)
DFX <- data.frame(DFX, MEAN.DENSF2M=rep(MEAN.DENSF2M, table(DFX$FOCAL )))
MEAN.DENSF5M <- tapply(DFX$NFEM.5M, DFX$FOCAL , mean)
DFX <- data.frame(DFX, MEAN.DENSF5M=rep(MEAN.DENSF5M, table(DFX$FOCAL )))

# example of comuting max value by NEW.FOC 
MAX.DENSF2M <- tapply(DFX$NFEM.2M, DFX$FOCAL , max)
DFX <- data.frame(DFX, MAX.DENSF2M=rep(MAX.DENSF2M, table(DFX$FOCAL )))
MAX.DENSF5M <- tapply(DFX$NFEM.5M, DFX$FOCAL , max)
DFX <- data.frame(DFX, MAX.DENSF5M=rep(MAX.DENSF5M, table(DFX$FOCAL )))

# 1st calc min value for iteration per focal
# then select records where iteration value == that min value

MIN.IT <- tapply(DFX$Iteration, DFX$FOCAL , min)
DFX <- data.frame(DFX, MIN.IT=rep(MIN.IT, table(DFX$FOCAL )))

DF.HABITAT <- subset(DFX, DFX$Iteration == DFX$MIN.IT)


# each focal should now have only 1 record - but worth checking
zz <- as.data.frame(table(DF.HABITAT$FOCAL))
table(zz$Freq)
which(zz$Freq > 1)

CI.loess <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

##------------------------------------------------------------------

par(mfrow = c(1,1))

ggpairs(DF.HABITAT,
        columns =, c("MEAN.DENSF2M","MED.DENSF2M", "MAX.DENSF2M",
                     "MEAN.DENSF5M", "MEAN.DENSF5M", "MEAN.DENSF5M"),
        lower = list(continuous = CI.loess),
        upper = list(continuous = "cor"), 
        diag = list(continuous = "bar"),
        axisLabels = 'show')


# set this so models know how to deal with any NA data
options(na.action = "na.fail")
names(DF2)
#NEED TO REMOVE MISSING DATA ROWS
dfmod <- na.omit(DF.HABITAT)

### MODEL SUMMARISED BY FOCAL
#1:  FULL MODEL use lmer and reml = false for cf by AIC
names(DF.HABITAT)
MF.GRASSINESS <- lmer(GRASSINESS ~ MED.DENSF2M + MED.DENSF5M + YEAR
                      + PUPAGE + PUP.SEX + RAIN.PROP + SUN.PROP 
                      # + INPOOL.PROP        
                      + Z_med.rMSSD.adj.RPCALL 
                      + (1|ID)+ (1|ID:FOCAL), 
                      data = dfmod, 
                      REML = FALSE)
summary(MF.GRASSINESS)  
dredge(MF.GRASSINESS)
#for best model use glmer - and defualt (reml) - best for cf effects
#ERROR USING GLMER = calling glmer() with family=gaussian (identity link) as a shortcut to lmer() is deprecated; please call lmer() directly
MF.GRASSINESS.BEST <- lmer(GRASSINESS ~ PUPAGE + YEAR + (1|ID)+ (1|ID:FOCAL), 
                           data = dfmod, 
                           #family =  gaussian(link = "identity) - unused argument so removed
                           REML = FALSE)

summary(MF.GRASSINESS.BEST)

##ROUGHNESS
MF.ROUGHNESS <- lmer(ROUGHNESS ~ MED.DENSF2M + MED.DENSF5M + YEAR
                     + PUPAGE + PUP.SEX + RAIN.PROP + SUN.PROP 
                     # + INPOOL.PROP               
                     + Z_med.rMSSD.adj.RPCALL 
                     + (1|ID)+ (1|ID:FOCAL), 
                     data = dfmod, 
                     REML = FALSE)


summary(MF.ROUGHNESS)  
dredge(MF.ROUGHNESS) 

MF.ROUGHNESS.BEST <- lmer(ROUGHNESS ~ PUP.SEX + YEAR + (1|ID), 
                          data = dfmod+ (1|ID:FOCAL), 
                          #family =  gaussian(link = "identity) - unused argument so removed
                          REML = FALSE)

summary(MF.ROUGHNESS.BEST)



####RPTR
#https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html 

#https://rdrr.io/cran/rptR/f/vignettes/rptR.Rmd

library(rptR)

#library(Hmisc)

#####ROUGHNESS

dfmod$YYID <- paste(dfmod$YEAR, dfmod$ID, sep="_")
AVROUGHNESS <- as.data.frame(tapply(dfmod$ROUGHNESS, dfmod$YYID, mean))
AVROUGHNESS$YYID <- row.names(AVROUGHNESS)
library(tidyr)
AVROUGHNESS <- AVROUGHNESS %>%
  separate(YYID, c("YEAR", "ID"), "_")
names(AVROUGHNESS)[names(AVROUGHNESS)=="tapply(dfmod$ROUGHNESS, dfmod$YYID, mean)"] <- "ROUGHNESS"
#AVGRASSINESS$ID <- factor(AVGRASSINESS$ID) ## REMOVE THESE TO LEAVE AS CHARACTER
#AVGRASSINESS$YEAR <- factor(AVGRASSINESS$YEAR)
str(AVROUGHNESS)


#my data frame was called dfHRV
# NOW TO SUBSET ONLY SEALS WITH > 1 HRV MEASURE 
x <- as.data.frame(table(AVROUGHNESS$ID)) # gives no. of samples (years) per seal 
colnames(x) <- c("ID", "Freq")
x <- subset(x, Freq > 1)

rep.df <- merge(AVROUGHNESS, x, by="ID")

as.data.frame(table(x$Freq))

# heres' the repeatability model - I was looking at repeatability of the variable rMSSD.adj.RPCALL from the dataframe rep.df.HRV
rpt(ROUGHNESS ~ (1 | ID), grname = "ID", data = rep.df, datatype = "Gaussian", 
    nboot = 0, npermut = 0)
# this 1st one does a quick run to check it works - the next does the same but 1000 permutations so it can calc signifcance values and confidence intervals

repROUGHNESS <- rpt(ROUGHNESS ~ (1 | ID), grname = "ID", data = rep.df, datatype = "Gaussian", 
                    nboot = 1000, npermut = 0)
# alternative with freq as fixed effect as in Monestrier et al 2016??) - to account for differing number of repeats per individual
repROUGHNESS2 <- rpt(ROUGHNESS ~ Freq + (1 | ID), grname = "ID", data = rep.df, datatype = "Gaussian", 
                     nboot = 1000, npermut = 0)

#this gives overall repeatability across the whole sample/all individuals
print(repROUGHNESS)
summary(repROUGHNESS)
plot(repROUGHNESS)
str(repROUGHNESS)

print(repROUGHNESS2)
summary(repROUGHNESS2)
plot(repROUGHNESS2)
str(repROUGHNESS2)

# and to get values of repeatability for each individual
## Ri individual repeatabilities

# subset ID and HRv - then use aggregate to get INDIVIDUAL (within individual) variance
df.var <- select(rep.df, ID, ROUGHNESS)
### THIS IS WHERE GETS STUCK WHEN A FACTOR #####
ROUGHNESS.var = aggregate(df.var, by = list(df.var$ID), FUN = var)  # calc variance by ID
colnames(ROUGHNESS.var) <- c("ID", "X", "ROUGHNESS.VI") # rename variables to something sensible


# now to get VG i.e. between indiv var - use simple LMM
# with HRV as the dependent/repsonse variable and ID as random. 

Mvar <- lmer(ROUGHNESS ~ Freq + (1|ID),
             data = rep.df, 
             #family =  gaussian(link = "identity")
) 

summary(Mvar)
class(Mvar)
# extract variance component from the model
V1 <- as.data.frame(VarCorr(Mvar)) ####THIS IS WHERE GETS STUCK WHEN CHARACTER
VG <- V1$vcov[1]  # extract variance measure for 'ID' == VG i.e. between group (or individual) variance


# Then compute Ri for each individual using HRV.var df
ROUGHNESS.var$Ri <- VG / (VG + ROUGHNESS.var$ROUGHNESS.VI)

# now get summarry stats for Ri and plot a nice graph
describe(ROUGHNESS.var$Ri)
summary(ROUGHNESS.var$Ri)

#and plot it

p1 <- ggplot(data=HRV.var, aes(HRV.var$Ri)) + 
  geom_histogram(binwidth=0.1)  +
  labs(x="Individual-level repeatability", y = "Count") +
  theme_classic()

p1 + geom_vline(aes(xintercept=mean(Ri)),
                color="blue", linetype="dashed", size=1)

####################################################
#####GRASSINESS

dfmod$YYID <- paste(dfmod$YEAR, dfmod$ID, sep="_")
AVGRASSINESS <- as.data.frame(tapply(dfmod$GRASSINESS, dfmod$YYID, mean))
AVGRASSINESS$YYID <- row.names(AVGRASSINESS)
library(tidyr)
AVGRASSINESS <- AVGRASSINESS %>%
  separate(YYID, c("YEAR", "ID"), "_")
names(AVGRASSINESS)[names(AVGRASSINESS)=="tapply(dfmod$GRASSINESS, dfmod$YYID, mean)"] <- "GRASSINESS"
#AVGRASSINESS$ID <- factor(AVGRASSINESS$ID)
#AVGRASSINESS$YEAR <- factor(AVGRASSINESS$YEAR)
str(AVGRASSINESS)
#my data frame was called dfHRV
# NOW TO SUBSET ONLY SEALS WITH > 1 HRV MEASURE 
x <- as.data.frame(table(AVGRASSINESS$ID)) # gives no. of samples (years) per seal 
colnames(x) <- c("ID", "Freq")
x <- subset(x, Freq > 1)

rep.df <- merge(AVGRASSINESS, x, by="ID")

as.data.frame(table(x$Freq))

# heres' the repeatability model - I was looking at repeatability of the variable rMSSD.adj.RPCALL from the dataframe rep.df.HRV
rpt(GRASSINESS ~ (1 | ID), grname = "ID", data = rep.df, datatype = "Gaussian", 
    nboot = 0, npermut = 0)
# this 1st one does a quick run to check it works - the next does the same but 1000 permutations so it can calc signifcance values and confidence intervals

repGRASSINESS<- rpt(GRASSINESS ~ (1 | ID), grname = "ID", data = rep.df, datatype = "Gaussian", 
                    nboot = 1000, npermut = 0)
# alternative with freq as fixed effect as in Monestrier et al 2016??) - to account for differing number of repeats per individual
repGRASSINESS2 <- rpt(GRASSINESS ~ Freq + (1 | ID), grname = "ID", data = rep.df, datatype = "Gaussian", 
                      nboot = 1000, npermut = 0)

#this gives overall repeatability across the whole sample/all individuals
print(repGRASSINESS)
summary(repGRASSINESS)
plot(repGRASSINESS)
str(repGRASSINESS)

print(repGRASSINESS2)
summary(repGRASSINESS2)
plot(repGRASSINESS2)
str(repGRASSINESS2)

# and to get values of repeatability for each individual
## Ri individual repeatabilities

# subset ID and HRv - then use aggregate to get INDIVIDUAL (within individual) variance
df.var <- select(rep.df, ID, GRASSINESS)
GRASSINESS.var = aggregate(df.var, by = list(df.var$ID), FUN = var)  # calc variance by ID
colnames(GRASSINESS.var) <- c("ID", "X", "GRASSINESS.VI") # rename variables to something sensible


# now to get VG i.e. between indiv var - use simple LMM
# with HRV as the dependent/repsonse variable and ID as random. 

Mvar <- lmer(GRASSINESS ~ Freq + (1|ID),
             data = rep.df, 
             #family =  gaussian(link = "identity")
) 

summary(Mvar)
# extract variance component from the model
V1 <- as.data.frame(VarCorr(Mvar))
VG <- V1$vcov[1]  # extract variance measure for 'ID' == VG i.e. between group (or individual) variance


# Then compute Ri for each individual using HRV.var df
HRV.var$Ri <- VG / (VG + HRV.var$RHRV.VI)

# now get summarry stats for Ri and plot a nice graph
describe(HRV.var$Ri)
summary(HRV.var$Ri)

#and plot it

p1 <- ggplot(data=HRV.var, aes(HRV.var$Ri)) + 
  geom_histogram(binwidth=0.1)  +
  labs(x="Individual-level repeatability", y = "Count") +
  theme_classic()

p1 + geom_vline(aes(xintercept=mean(Ri)),
                color="blue", linetype="dashed", size=1)

####################################################

dev.off()
p2 <- ggplot(dfmod,aes(ROUGHNESS ,MDML.DEVB)) + 
  geom_smooth(method="lm",size=1) +
  labs(x="Median resting HRV within a breeding season", y = "Absolute deviance from mean maternal daily mass loss rate (kg)")+
  theme_classic() +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic()

p2 + scale_y_continuous(breaks = seq(0.0, 1.7, by=0.5), limits=c(0.0,1.7))
p2 + scale_x_continuous(breaks = seq(20, 90, by=10), limits=c(20,95))

p2 +theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10,face="bold"))




#for best model use glmer - and defualt (reml) - best for cf effects
MF.PROX.BEST <- glmer(MP.PROX.NUM ~ SUBSTRATE + Z_med.rMSSD.adj.RPCALL + (1|ID) + (1|ID:FOCNUM), 
                      data = dfmod, 
                      family =  gaussian(link = "identity"))

summary(MF.PROX.BEST)

