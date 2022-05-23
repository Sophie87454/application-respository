#ROAD SAFETY
#CLUSTERING


#library(esquisse)
#library(ggplot2)
#library(reshape2)
library(factoextra)
#library(cluster)


##Data Preparation
library(tidyverse)

rm(list=ls())

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Road safety")


#import accident dataset
df<- read.csv("dft-road-casualty-statistics-accident-last-5-years.csv")

names(df)
head(df)
summary(df)
str(df)


#convert any missing data to NA
df[df==-1]<- NA
df[is.null(df)]<- NA


#count the number of distinct locations
n_distinct(df$local_authority_ons_district)


#create dataframe with location and accident severity frequencies
location_df <- df %>%
  select(accident_severity, local_authority_ons_district) %>% 
  group_by(local_authority_ons_district) %>% count(accident_severity) %>% 
  pivot_wider(names_from=accident_severity, values_from=n) %>% rename(fatal = "1") %>%
  rename(serious = "2") %>% rename(slight = "3") %>% 
  mutate(fatal = ifelse(is.na(fatal), 0, fatal),
         serious = ifelse(is.na(serious), 0, serious),
         slight = ifelse(is.na(slight), 0, slight)) %>%
  as.data.frame()

#save prepared dataset
write.csv(location_df, "location_df.csv", row.names = FALSE)


##K-means clustering

str(location_df)
names(location_df)
summary(location_df)


colSums(is.na(location_df))

head(location_df)
length(location_df[,1])

rownames(location_df) <- location_df[,1]
location_df[,1] <- NULL 

normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}


lsoa<-rownames(location_df)
norm<-as.data.frame(lapply(location_df,normalise))
rownames(norm)<-lsoa

rm(list=setdiff(ls(), "norm"))

tendency <- get_clust_tendency(norm, n = nrow(norm)-1, graph = FALSE)
tendency$hopkins_stat


gc()
memory.limit(size=40000)
fviz_nbclust(norm, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(norm,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

set.seed(123)
km.fit <- kmeans(norm, 4)
km.fit$cluster
km.fit$size

fviz_cluster(km.fit,norm)

aggregate(norm, by=list(cluster=km.fit$cluster), mean)


clusters <- as.data.frame(km.fit$cluster)
cl1 <- merge(norm, clusters, by=0 )
rownames(cl1) <- cl1[,1]
cl1[,1] <- NULL
cl1 <- cl1 %>% filter(`km.fit$cluster`!=4) %>%
  select(-`km.fit$cluster`)

summary(cl1)
count(cl1$fatal)

fviz_nbclust(cl1, kmeans, method = "wss")

set.seed(123)
km.fit <- kmeans(cl1, 5)
km.fit$cluster
km.fit$size

fviz_cluster(km.fit,cl1)

aggregate(cl1, by=list(cluster=km.fit$cluster), mean)



clusters <- as.data.frame(km.fit$cluster)
clusters$local_authority_ons_district <- row.names(clusters)
full <- merge(df, clusters, by="local_authority_ons_district")
names(full)[ncol(full)] <- "cluster"
colSums(is.na(df))



str(full)
full$latitude <- as.numeric(full$latitude)
full$longitude <- as.numeric(full$longitude)
full$cluster <- as.factor(full$cluster)



library(ggmap)
# compute the bounding box
bc_bbox <- make_bbox(lat = latitude, lon = longitude, data = full)
bc_bbox

# grab the maps from google
bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain")

# plot the points and color them by sector
ggmap(bc_big) + 
  geom_point(data = full, mapping = aes(x = longitude, y = latitude, color = cluster),
             shape=".")



####compare SAS and R results

SAS_df <- read.csv("sas-clusters_TRAIN.csv")
SAS_df <- SAS_df %>% select(local_authority_ons_district, X_SEGMENT1_)


compare <-  merge(SAS_df, clusters, by='local_authority_ons_district')
names(compare)[ncol(compare)] <- "cluster"

compare %>% filter(cluster==1) %>% count(X_SEGMENT1_) 
compare %>% filter(cluster==2) %>% count(X_SEGMENT1_)
compare %>% filter(cluster==3) %>% count(X_SEGMENT1_)
compare %>% filter(cluster==4) %>% count(X_SEGMENT1_)
compare %>% filter(cluster==5) %>% count(X_SEGMENT1_)
