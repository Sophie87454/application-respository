## Script for checking data, combining years into one dataframe and 

rm(list=ls())

objects()
search()

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\Documents\\R\\Seal project\\Final analysis")



#read in files
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
sort(unique(DF2015.05$ID))
sort(unique(DF2015.10$ID))
sort(unique(DF2016$ID))
sort(unique(DF2017$ID))

#Combine dataframes into one 
dfmain <- rbind(DF2015.05, DF2015.10, DF2016, DF2017)
dfmain$INDEX <- 1:nrow(dfmain)

#convert to factor
dfmain$ID <- as.factor(dfmain$ID)

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
dfmain$ZOOM.NUM <- ifelse(dfmain$ZOOM == "WIDE", 1, dfmain$ZOOM.NUM)#I treated this as 1 on the few times it occured

#calculate seal density
dfmain$TOT.SEALS.2M <- dfmain$NFEM.2M + dfmain$NPUP.2M + dfmain$NMALE.2M 
dfmain$TOT.SEALS.5M <- dfmain$NFEM.5M + dfmain$NPUP.5M + dfmain$NMALE.5M 

#reduce orientation categories to line of sight y/n
dfmain$LINE.OF.SIGHT <- NA
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "MawayP", "N", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "RIGHT", "Y", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "MfacingP", "Y", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- ifelse(dfmain$MP.orient == "LEFT", "Y", dfmain$LINE.OF.SIGHT)
dfmain$LINE.OF.SIGHT <- as.factor(dfmain$LINE.OF.SIGHT)

#Preliminary analysis
#check for variables which might not be useful
summary(dfmain)
par(mfrow = c(3,4))
for (i in 11:22) {
  plot(as.factor(dfmain[,i]), main=colnames(dfmain)[i],
       ylab = "Count", col="steelblue", las = 2)
}
par(mfrow = c(3,3))
for (i in 23:31) {  
  plot(as.factor(dfmain[,i]), main=colnames(dfmain)[i],
       ylab = "Count", col="steelblue", las = 2)
}
par(mfrow = c(3,3))
for (i in 32:38) {  
  plot(as.factor(dfmain[,i]), main=colnames(dfmain)[i],
       ylab = "Count", col="steelblue", las = 2)
}

#COMBINE YEAR AND FOCNUM
dfmain$FOCAL <- paste(dfmain$YY, dfmain$FOCNUM, sep="_")

#REMOVE CERTAIN RECORDS
#e.g. where MOOS, in pool, moving
DFWITHOUT <- dfmain[which(dfmain$MP.PROX!="MOOS" & dfmain$MUM.LOCN=="SAME"), ]
#decided to keep POOS as sometimes because pup was behind rock or just out of sight on camera
names(DFWITHOUT)

#create new data frame with chosen variables
DF2 <- dfmain %>% select("INDEX", "FOCAL", "ID", "YY", "FOCNUM", "ZOOM","ZOOM.NUM", "NFEM.2M", "NPUP.2M", "NMALE.2M", "NFEM.5M", "NPUP.5M","NMALE.5M",
                     "SUN.SHADE", "RAIN.DRY",  "MP.orient", "MPR.orient", "POOLDIST", "TOPO",
                     "SUBSTRATE", "IN.POOL", "Iteration",  "DDATE", "SAMP.TIME","DOY" ,"MP.PROX", "MP.PROX.NUM", "TIME.FVSTART", "TIME.FVEND",
                     "LINE.OF.SIGHT")

DF3 <- DFWITHOUT %>% select("INDEX", "FOCAL", "ID", "YY", "FOCNUM", "ZOOM.NUM", "NFEM.2M", "NPUP.2M", "NMALE.2M", "NFEM.5M", "NPUP.5M","NMALE.5M", "TOT.SEALS.2M", "TOT.SEALS.5M",
                            "SUN.SHADE", "RAIN.DRY",  "MP.orient", "MPR.orient" , "TOPO", "SUBSTRATE", "IN.POOL" , "Iteration",  "DDATE", "SAMP.TIME",
                            "DOY", "MP.PROX","MP.PROX.NUM", "TIME.FVSTART", "TIME.FVEND","LINE.OF.SIGHT")

summary(DF3)

# FIRST WE NEED TO RUN THIS TO CREATE A FUNCTION to do loess with CI for ggpairs 
CI.loess <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

names(DF3)
# PICK WHICH VARS YOU WANT TO PLAT AGAINST EACH OTHER - AND DO IN SUBSETS E.G. 
# USEFUL TO KEEP COPIES OF THE RESULTING PLOTS (E.G HERE - SEEMS N PUPS AND MUMS CORREALTES AT 5M, NOT 2M
# BUT PLENTY OF SCATTER SO WORTH KEEPING ALL)
ggpairs(DF3,
        columns =, c("DOY", "NFEM.2M", "NPUP.2M", "NFEM.5M", "NPUP.5M"),
        lower = list(continuous = CI.loess),
        upper = list(continuous = "cor"), 
        diag = list(continuous = "bar"),
        axisLabels = 'show')

# E.G HAVE A LOOK AT THE MORE WEATHER RELATED VARIABLES (TAKES A BIT MORE TO GET YOUR HEAD AROUND THESE ONES!)
ggpairs(DF3,
        columns =, c("DOY", "SUN.SHADE",  "RAIN.DRY" , "IN.POOL" , "POOLDIST", "MP.PROX.NUM" ),
        lower = list(continuous = CI.loess),
        upper = list(continuous = "cor"), 
        diag = list(continuous = "bar"),
        axisLabels = 'show')



#Loop to summarise paramters by focal number

# Set a couple of printing options
options(digits = 12)
options(digits.secs = 4)

#Empty data frame for output
dfout <- data.frame(FOCAL = factor(),
                    FOCNUM = factor(),
                    ID = factor(),
                    YEAR = numeric(),
                    DOY = integer(),
                    TSTART =  numeric(),
                    TEND =  numeric(),
                    FOCDUR  = numeric(),
                    MAX.ZOOM = integer(),
                    NFEM.5M.MAX.ZOOM = integer(),
                    NPUP.5M.MAX.ZOOM = integer(),
                    NMALE.5M.MAX.ZOOM = integer(),
                    
                    NFEM.2M.MEAN = integer(),
                    NFEM.2M.MIN = integer(),
                    NFEM.2M.MAX = integer(),
                    SHADE.PROP =  numeric(),
                    RAIN.PROP = numeric(),
                    IN.POOL.PROP = numeric(),
                    TOPO.MODE = factor(),
                    SUBSTRATE.MODE = factor(),
                    MP.PROX.MODE = numeric(),
                    LINE.OF.SIGHT = factor(),
                    stringsAsFactors=FALSE)
#Temporary version
tmpout <- dfout
tmpout[nrow(tmpout)+1,] <- rbind(tmpout, (rep(NA, length(tmpout))))

flist <- sort(unique(DF3$FOCAL )) 

for (q in 1:length(flist)) {  
  
  fnum <- flist[q]
  
  DX <- subset(DF3, DF3$FOCAL == fnum) 
  nrecs <- dim(DX)[1]
  
  tmpout$FOCAL <- fnum
  tmpout$FOCNUM <- DX$FOCNUM[1]
  tmpout$ID <- DX$ID[1]
  tmpout$YEAR <- DX$YY[1]
  tmpout$DOY <- DX$DOY[1]
  tmpout$TSTART <- strftime(DX$TIME.FVSTART[1], format="%H:%M:%S") 
  tmpout$TEND <- DX$SAMP.TIME[dim(DX)[1]]
  tmpout$FOCDUR <- difftime(DX$TIME.FVEND[dim(DX)[1]],DX$TIME.FVSTART[1], units= "mins")
  
  
  ZOOM.OUT <- max(DX$ZOOM.NUM, na.rm=T) 
  tmpout$MAX.ZOOM <- ZOOM.OUT
  
  if (ZOOM.OUT == 99) {   
    max.zoom.DX <- subset(DX, DX$ZOOM.NUM == 99)
    
    tmpout$NFEM.5M.MAX.ZOOM <- max.zoom.DX$NFEM.5M[1]
    tmpout$NPUP.5M.MAX.ZOOM <- max.zoom.DX$NPUP.5M[1]
    tmpout$NMALE.5M.MAX.ZOOM <- max.zoom.DX$NMALE.5M[1]
    
  } 
  
  tmpout$NFEM.2M.MEAN <- mean(DX$NFEM.2M, na.rm=T)
  tmpout$NFEM.2M.MIN <- min(DX$NFEM.2M, na.rm=T)
  tmpout$NFEM.2M.MAX <- max(DX$NFEM.2M, na.rm=T)
  
  tmpout$SHADE.PROP <- length(which(DX$SUN.SHADE == "SHADE"))  / length(DX$SUN.SHADE[!is.na(DX$SUN.SHADE)])
  tmpout$RAIN.PROP <- length(which(DX$RAIN.DRY == "RAIN"))  / length(DX$RAIN.DRY[!is.na(DX$RAIN.DRY)])
  tmpout$IN.POOL.PROP <- length(which(DX$IN.POOL == "YES"))  / length(DX$IN.POOL[!is.na(DX$IN.POOL)])
  tmpout$TOPO.MODE <- Mode(DX$TOPO)
  tmpout$SUBSTRATE.MODE <- Mode(DX$SUBSTRATE)
  tmpout$MP.PROX.MODE <- Mode(DX$MP.PROX) 
  tmpout$LINE.OF.SIGHT <- Mode(DX$LINE.OF.SIGHT)
  
  dfout <-  rbind(dfout,tmpout)
}  

#ADDING PROACTIVE/REACTIVE DATA
PRDF <- read.csv(file = "HRV-2013-2017-PRIMARY-DATA-Nov20.csv", header = TRUE, 
                 na.strings=c("","NA"))

PRDF <- PRDF[,1:17]  

sort(unique(dfout$ID))
sort(unique(PRDF$ID))

#create merged dataframe - contains only seals with HR data
dfoutmerged <- merge(dfout, PRDF, by=c("ID", "YEAR"))

#partially merged data - seals have 
dfpartialmerge <- merge(dfout, PRDF, by= c("ID", "YEAR"), all = TRUE)
dfpartialmerge <- dfpartialmerge[which(dfpartialmerge$FOCAL!="NA"), ]


dfpartialmerge$CS <- NA
for (i in seq_len(nrow(dfout))) {
  dfpartialmerge$CS = ifelse(dfpartialmerge$med.rMSSD.adj.RPCALL > av.HR, "Reactive", "Proactive")
}

dfpartialmerge$CS <- as.factor(dfpartialmerge$CS)
dfpartialmerge$TOPO.MODE <- as.factor(dfpartialmerge$TOPO.MODE)
dfpartialmerge$SUBSTRATE.MODE <- as.factor(dfpartialmerge$SUBSTRATE.MODE)
dfpartialmerge$LINE.OF.SIGHT <- as.factor(dfpartialmerge$LINE.OF.SIGHT)
dfpartialmerge$MP.PROX.MODE <- as.numeric(dfpartialmerge$MP.PROX.MODE)
summary(dfpartialmerge)
 
 
#Output summary file 
outfile.name <- paste0("MERGED-DATASET.txt")
 write.table(dfout, file = outfile.name, col.names=T, row.names=F, sep = ",", append = FALSE)
 
names(dfpartialmerge)
 library(tidyverse)
 df.pca <- dfpartialmerge[c("NFEM.5M.MAX.ZOOM", "NPUP.5M.MAX.ZOOM",
                             "NMALE.5M.MAX.ZOOM", "NFEM.2M.MEAN",
                             "SHADE.PROP","RAIN.PROP")]
 res.pca <- prcomp(df.pca, scale = TRUE)
 summary(res.pca)
 