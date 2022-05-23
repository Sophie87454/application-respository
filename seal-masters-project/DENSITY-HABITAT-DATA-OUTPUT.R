## Script for checking data, combining years into one dataframe and producing output files

rm(list=ls())

objects()
search()


library(MASS) 	# general package
library(car)  	# genearl package
library(ggplot2)
library(dplyr)
library(GGally) # for ggpairs
library(lubridate)


#set working directory
setwd("C:\\Users\\smann\\OneDrive\\Documents\\R\\Seal project\\Final analysis")

#read in files
DF2013 <- read.csv(file = "2013-Seal-Data.csv", header = TRUE, 
                      na.strings=c("","NA"))

DF2014 <- read.csv(file = "2014-Seal-Data.csv", header = TRUE, 
                      na.strings=c("","NA"))

DF2015.05 <- read.csv(file = "2015-Seal-Data.csv", header = TRUE, 
                      na.strings=c("","NA"))

DF2015.10 <- read.csv(file = "2015-SealData-10minutes.csv", header = TRUE, 
                      na.strings=c("","NA"))

DF2016<-read.csv(file = "2016-Seal-Data.csv", header = TRUE, 
                 na.strings=c("","NA"))

DF2017 <- read.csv(file = "2017-Seal-Data.csv", header = TRUE, 
                   na.strings=c("","NA"))

## DATA QUALITY CHECKING
#check dataframe
names(DF2013)
summary(DF2013)
str(DF2013)
head(DF2013)

names(DF2014)
summary(DF2014)
str(DF2014)
head(DF2014)

names(DF2015.05)
summary(DF2015.05)
str(DF2015.05)
head(DF2015.05)

names(DF2015.10)
summary(DF2015.10)
str(DF2015.10)
head(DF2015.10)

names(DF2016)
summary(DF2016)
str(DF2016)
head(DF2016)

names(DF2017)
summary(DF2017)
str(DF2017)
head(DF2017)

#check seal IDs
sort(unique(DF2013$ID))#checked
sort(unique(DF2014$ID))#checked
sort(unique(DF2015.05$ID))#checked
sort(unique(DF2015.10$ID))
sort(unique(DF2016$ID))
sort(unique(DF2017$ID))

# CONVERT TO 10MIN SAMPLING
# DO THIS BY SELECTING ONLY ROWS WITH AN ODD NUMBER FOR ITERATION
DF2015.05 <- DF2015.05[DF2015.05$Iteration %% 2 == 1, ]

#Combine dataframes into one 
dfmain <- rbind(DF2013, DF2014, DF2015.05, DF2015.10, DF2016, DF2017)
dfmain$INDEX <- 1:nrow(dfmain)

#convert to factor
dfmain$ID <- factor(dfmain$ID)

#create time object and create time point for each sample
dfmain$DDATE <- as.Date( with(dfmain, paste(YY,MM,DD, sep = "." )) , format = "%Y.%m.%d" )
dfmain$TIME.FVSTART <- paste(dfmain$DDATE,as.character(dfmain$TSTARTRT))
dfmain$TIME.FVSTART <- as.POSIXct((dfmain$TIME.FVSTART),format=  "%Y-%m-%d %H:%M:%S")
dfmain$TIME.FVEND <- dfmain$TIME.FVSTART + dfmain$Iteration*dfmain$SAMPINT*60
dfmain$SAMP.TIME <- strftime(dfmain$TIME.FVEND, format="%H:%M:%S") 

#change date/pupstage to day of year
dfmain$DOY <- yday(dfmain$DDATE)


#create numeric version of MP.PROX
dfmain$MP.PROX.NUM <- NA
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "0", 0, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "<0", 0, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "1", 1, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "2", 2, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "3", 3, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "4", 4, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "5", 5, dfmain$MP.PROX.NUM)
dfmain$MP.PROX.NUM <- ifelse(dfmain$MP.PROX == "5PLUS", 6, dfmain$MP.PROX.NUM)

#create numeric version of zoom - wide = 1
dfmain$ZOOM.NUM <- NA
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "2", 2, dfmain$ZOOM.NUM)
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "3", 3, dfmain$ZOOM.NUM)
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "4", 4, dfmain$ZOOM.NUM)
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "5", 5, dfmain$ZOOM.NUM)
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "5PLUS", 99, dfmain$ZOOM.NUM)
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "WIDE", 1, dfmain$ZOOM.NUM

#calculate seal density
dfmain$TOT.SEALS.2M <- dfmain$NFEM.2M + dfmain$NPUP.2M + dfmain$NMALE.2M 
dfmain$TOT.SEALS.5M <- dfmain$NFEM.5M + dfmain$NPUP.5M + dfmain$NMALE.5M 

#reduce orientation categories to line of sight y/n
dfmain$LINE.OF.SIGHT <- NA
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "MawayP", "0", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "RIGHT", "1", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "MfacingP", "1", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "LEFT", "1", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- factor(dfmain$LINE.OF.SIGHT)

#COMBINE YEAR AND FOCNUM
dfmain$FOCAL <- paste(dfmain$YY, dfmain$FOCNUM, sep="_")

#REMOVE CERTAIN RECORDS
#e.g. where MOOS, in pool, moving
DFWITHOUT <- dfmain[which(dfmain$MP.PROX!="MOOS" & dfmain$MUM.LOCN=="SAME"), ]
#decided to keep POOS as sometimes because pup was behind rock or just out of sight on camera
names(DFWITHOUT)

#create new data frame with chosen variables
DF2 <- dfmain 

DF3 <- DFWITHOUT 

summary(DF3)

names(DF2)

#ADDING PROACTIVE/REACTIVE DATA
PRDF <- read.csv(file = "HRV-2013-2017-PRIMARY-DATA-Nov20.csv", header = TRUE, 
                 na.strings=c("","NA"))
av.HR <- mean(PRDF$med.rMSSD.adj.RPCALL)
PRDF <- PRDF[,1:17]  

sort(unique(DF2$ID))
sort(unique(PRDF$ID))

#create merged dataframe - contains only seals with HR data
DF2$YEAR <- DF2$YY
dfmerged <- merge(DF2, PRDF, by=c("ID", "YEAR"))

#partially merged data
dfpartialmerge <- merge(DF2, PRDF, by= c("ID", "YEAR"), all = TRUE)
dfpartialmerge <- dfpartialmerge[which(dfpartialmerge$FOCAL!="NA"), ]

names(dfmerged)
#CREATE CS VALUES
#merged
dfmerged$CS <- NA
for (i in seq_len(nrow(dfmerged))) {
  dfmerged$CS = ifelse(dfmerged$med.rMSSD.adj.RPCALL > av.HR, "Reactive", "Proactive")
}  #classifies as pro or reactive based on mean rmssd
dfmerged$CS <- factor(dfmerged$CS)

#partially merged
dfpartialmerge$CS <- NA
for (i in seq_len(nrow(dfpartialmerge))) {
  dfpartialmerge$CS = ifelse(dfpartialmerge$med.rMSSD.adj.RPCALL > av.HR, "Reactive", "Proactive")
}  #classifies as pro or reactive based on mean rmssd
dfpartialmerge$CS <- factor(dfpartialmerge$CS)


dfmerged <- arrange(dfmerged, INDEX)
dfpartialmerge <- arrange(dfpartialmerge, INDEX)

#CALC PUP AGE ASSUMING PUP DOB ADDED IN FOR ALL RECORDS FIRST
dfmerged$PUPAGE <- dfmerged$DOY - dfmerged$BDAY.DOY + 1
dfpartialmerge$PUPAGE <- dfpartialmerge$DOY - dfpartialmerge$BDAY.DOY + 1

# CALCULATE ESTIMATED MATERNAL MASS EACH DAY
dfmerged$EST.MAT.MASS <- (dfmerged$MPPM.NEW - ((dfmerged$DOY - dfmerged$BDAY.DOY) *   dfmerged$MS.MDML.NEW))
dfpartialmerge$EST.MAT.MASS <- (dfpartialmerge$MPPM.NEW - ((dfpartialmerge$DOY - dfpartialmerge$BDAY.DOY) *   dfpartialmerge$MS.MDML.NEW))

#create binary values
#merged
dfmerged$SUN.YN <- ifelse(dfmerged$SUN.SHADE == "SUN", 1,0)
dfmerged$DRY.YN <- ifelse(dfmerged$RAIN.DRY == "DRY", 1,0)
dfmerged$INPOOL.YN <- ifelse(dfmerged$IN.POOL == "NO", 0,1) # EITHER HEAD OR BODY IN POOL = 1

dfmerged$TOPO.FLAT.YN <- ifelse(dfmerged$TOPO == "FLAT", 1,0)
dfmerged$TOPO.MOD.YN <- ifelse(dfmerged$TOPO == "MOD", 1,0)
dfmerged$TOPO.ROUGH.YN <- ifelse(dfmerged$TOPO == "ROUGH", 1,0)

dfmerged$SUBST.GRASS.YN <- ifelse(dfmerged$SUBSTRATE == "GRASS", 1,0)
dfmerged$SUBST.MUD.YN <- ifelse(dfmerged$SUBSTRATE == "MUD", 1,0)
dfmerged$SUBST.ROCK.YN <- ifelse(dfmerged$SUBSTRATE == "ROCK", 1,0)

#partially merged
dfpartialmerge$SUN.YN <- ifelse(dfpartialmerge$SUN.SHADE == "SUN", 1,0)
dfpartialmerge$DRY.YN <- ifelse(dfpartialmerge$RAIN.DRY == "DRY", 1,0)
dfpartialmerge$INPOOL.YN <- ifelse(dfpartialmerge$IN.POOL == "NO", 0,1) # EITHER HEAD OR BODY IN POOL = 1

dfpartialmerge$TOPO.FLAT.YN <- ifelse(dfpartialmerge$TOPO == "FLAT", 1,0)
dfpartialmerge$TOPO.MOD.YN <- ifelse(dfpartialmerge$TOPO == "MOD", 1,0)
dfpartialmerge$TOPO.ROUGH.YN <- ifelse(dfpartialmerge$TOPO == "ROUGH", 1,0)

dfpartialmerge$SUBST.GRASS.YN <- ifelse(dfpartialmerge$SUBSTRATE == "GRASS", 1,0)
dfpartialmerge$SUBST.MUD.YN <- ifelse(dfpartialmerge$SUBSTRATE == "MUD", 1,0)
dfpartialmerge$SUBST.ROCK.YN <- ifelse(dfpartialmerge$SUBSTRATE == "ROCK", 1,0)

#change variables
#merged
str(dfmerged)
dfmerged$FOCNUM <- factor(dfmerged$FOCNUM)
dfmerged$SUN.SHADE <- factor(dfmerged$SUN.SHADE)
dfmerged$RAIN.DRY <- factor(dfmerged$RAIN.DRY)
dfmerged$MP.orient <- factor(dfmerged$MP.orient)
dfmerged$MPR.orient <- factor(dfmerged$MPR.orient)
dfmerged$IN.POOL <- factor(dfmerged$IN.POOL)
dfmerged$TOPO <- factor(dfmerged$TOPO)
dfmerged$SUBSTRATE <- factor(dfmerged$SUBSTRATE)
dfmerged$MUM.LOCN <- factor(dfmerged$MUM.LOCN)
dfmerged$FOCAL <- factor(dfmerged$FOCAL)
dfmerged$PUP.SEX <- factor(dfmerged$PUP.SEX)
dfmerged$SUN.YN <- factor(dfmerged$SUN.YN)
dfmerged$DRY.YN <- factor(dfmerged$DRY.YN)
dfmerged$INPOOL.YN <- factor(dfmerged$INPOOL.YN)
dfmerged$TOPO.FLAT.YN <- factor(dfmerged$TOPO.FLAT.YN)
dfmerged$TOPO.MOD.YN <- factor(dfmerged$TOPO.MOD.YN)
dfmerged$TOPO.ROUGH.YN <- factor(dfmerged$TOPO.ROUGH.YN)
dfmerged$SUBST.GRASS.YN <- factor(dfmerged$SUBST.GRASS.YN)
dfmerged$SUBST.MUD.YN <- factor(dfmerged$SUBST.MUD.YN)
dfmerged$SUBST.ROCK.YN <- factor(dfmerged$SUBST.ROCK.YN)

#PARTIAL MERGE
str(dfmerged)
dfpartialmerge$FOCNUM <-factor(dfpartialmerge$FOCNUM)
dfpartialmerge$SUN.SHADE <- factor(dfpartialmerge$SUN.SHADE)
dfpartialmerge$RAIN.DRY <- factor(dfpartialmerge$RAIN.DRY)
dfpartialmerge$MP.orient <- factor(dfpartialmerge$MP.orient)
dfpartialmerge$MPR.orient <- factor(dfpartialmerge$MPR.orient)
dfpartialmerge$IN.POOL <- factor(dfpartialmerge$IN.POOL)
dfpartialmerge$TOPO <- factor(dfpartialmerge$TOPO)
dfpartialmerge$SUBSTRATE <- factor(dfpartialmerge$SUBSTRATE)
dfpartialmerge$MUM.LOCN <- factor(dfpartialmerge$MUM.LOCN)
dfpartialmerge$FOCAL <- factor(dfpartialmerge$FOCAL)
dfpartialmerge$PUP.SEX <- factor(dfpartialmerge$PUP.SEX)
dfpartialmerge$SUN.YN <- factor(dfpartialmerge$SUN.YN)
dfpartialmerge$DRY.YN <- factor(dfpartialmerge$DRY.YN)
dfpartialmerge$INPOOL.YN <- factor(dfpartialmerge$INPOOL.YN)
dfpartialmerge$TOPO.FLAT.YN <- factor(dfpartialmerge$TOPO.FLAT.YN)
dfpartialmerge$TOPO.MOD.YN <- factor(dfpartialmerge$TOPO.MOD.YN)
dfpartialmerge$TOPO.ROUGH.YN <- factor(dfpartialmerge$TOPO.ROUGH.YN)
dfpartialmerge$SUBST.GRASS.YN <- factor(dfpartialmerge$SUBST.GRASS.YN)
dfpartialmerge$SUBST.MUD.YN <- factor(dfpartialmerge$SUBST.MUD.YN)
dfpartialmerge$SUBST.ROCK.YN <- factor(dfpartialmerge$SUBST.ROCK.YN)

#Output summary file 
#merged
outfile.name <- paste0("MERGED-DATASET.csv")
write.table(dfmerged, file = outfile.name, col.names=T, row.names=F, sep = ",", append = FALSE)

#partial merged
outfile.name <- paste0("PARTIAL-MERGED-DATASET.csv")
write.table(dfpartialmerge, file = outfile.name, col.names=T, row.names=F, sep = ",", append = FALSE)

