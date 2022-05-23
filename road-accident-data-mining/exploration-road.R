#ROAD SAFETY 
#DATA EXPLORATION & PREP

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(visdat)

rm(list=ls())

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Road safety")

#ACCIDENT DATASET
df <- read.csv("dft-road-casualty-statistics-accident-last-5-years.csv")

#exploration
head(accident_df)
tail(accident_df)
summary(accident_df)
str(accident_df)
names(accident_df)

dim(accident_df)

#dataset large so selecting specific years to reduce size - selected 2016-2018
accident_df <- df %>% filter(accident_year<2018)

#change ï..accident_index to accident_index
accident_df <- accident_df %>% rename(accident_index=ï..accident_index)
#check for duplicated data - no duplicates
table(duplicated(accident_df$accident_index))




#remove initally selected unnecessary variables
#remove accident reference, police force, local_authority_district, 
#local_authority_ons_district, local_authority_highway, first_road_number,
#second_road_number, lsoa_of_accident_location, did_police_officer_attend_scene_of_accident
accident_df <- accident_df %>% select(-c(accident_reference, police_force, local_authority_district, 
                                         local_authority_ons_district, local_authority_highway, first_road_number,
                                         second_road_number, lsoa_of_accident_location, did_police_officer_attend_scene_of_accident))
names(accident_df)


#NA variables are indicated as -1 or NULL for some variables- making NA
accident_df[accident_df==-1]<- NA
accident_df[is.null(accident_df)]<- NA




#visualising missing data 
vis_miss(accident_df, warn_large_data = FALSE)
#second_road_class and junction control have 
#similar missing values (41.79.01% and 41.7%)
#trunk road has regular missing values (9.14%)
#doesnt appear random
#only 3.5% missing altogether
colSums(is.na(accident_df))


#decided to remove second_road_class and junction_contol due to high level of NA
accident_df <- accident_df %>% select(-c(second_road_class, junction_control))
names(accident_df)

#CONVERT TIME TO JUST HOUR - less levels
accident_df$hour<- gsub("\\:.*","",accident_df$time)
#make integer
accident_df$hour<- as.integer(accident_df$hour)

#CONVERT DATE TO MONTH - less levels
#convert to date
accident_df$date <- as.Date(accident_df$date, format = "%d/%m/%Y") 
#extract month and put in new variable month
accident_df$month <- format(accident_df$date, format = "%m") 
#check
accident_df %>% count(month)
#make integer
accident_df$month<- as.integer(accident_df$month)

#make speed limit integer
accident_df$speed_limit<- as.integer(accident_df$speed_limit)
accident_df$longitude <- as.numeric(accident_df$longitude)
accident_df$latitude <- as.numeric(accident_df$latitude)
accident_df$location_northing_osgr <- as.numeric(accident_df$location_northing_osgr)
accident_df$location_easting_osgr <- as.numeric(accident_df$location_easting_osgr)
accident_df$number_of_vehicles <- as.numeric(accident_df$number_of_vehicles)
accident_df$number_of_casualties <- as.numeric(accident_df$number_of_casualties)

str(accident_df)

###CORRELATION PLOT
library(corrplot)

accident_df.corr <- accident_df %>% 
  select(-c(accident_index, date, time)) %>% na.omit() %>%  cor()
corrplot(accident_df.corr)
heatmap(x = accident_df.corr, symm = TRUE)
# speed limit and urban/rural area seem correlated




###VISUALISE NUMERIC DATA

accident_df$accident_severity <- as.factor(accident_df$accident_severity)

histogram <- function(attribute) {
  accident_df %>% ggplot(aes(x= attribute,fill = accident_severity)) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal()
}

nos <- histogram(accident_df$location_northing_osgr) +  labs(x = "locaton_northing_osgr")
eos <- histogram(accident_df$location_easting_osgr) +  labs(x = "locaton_easting_osgr")
long<- histogram(accident_df$longitude) +  labs(x = "longitude")
lat <- histogram(accident_df$latitude) +  labs(x = "latitude")
hour <- histogram(accident_df$hour) +  labs(x = "hour")
mon <- histogram(accident_df$month) +  labs(x = "month")
cas <- histogram(accident_df$number_of_casualties) +  labs(x = "number_of_casualties")
veh <- histogram(accident_df$number_of_vehicles) +  labs(x = "number_of_vehicles")

library(ggpubr)
ggarrange(nos, eos, lat, long,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
ggarrange(hour, mon, cas, veh,
          labels = c("E", "F","G", "H"),
          ncol = 2, nrow = 2)


#remove date and time as created new variables
#remove location_easting and location_northing as latitude and longitude similar so can be used in place
accident_df <- accident_df %>% select(-c(date,time,location_easting_osgr, location_northing_osgr))


###CHANGE CATEGORICAL FORMAT

#accident_severity
#check levels
accident_df %>% count(accident_severity)
#check missing data (-1)
sum(is.na(accident_df$accident_severity))
#change value
accident_df$accident_severity <- as.integer(accident_df$accident_severity)
accident_df$accident_severity[accident_df$accident_severity==1] <- "FATAL"
accident_df$accident_severity[accident_df$accident_severity==2] <- "SERIOUS"
accident_df$accident_severity[accident_df$accident_severity==3] <- "SLIGHT"
#convert to factor
accident_df$accident_severity <- as.factor(accident_df$accident_severity)


#first_road_class
#check NA
sum(is.na(accident_df$first_road_class))
#make unkown NA
accident_df$first_road_class[accident_df$first_road_class==6]<-NA
#check factor levels
accident_df %>% count(first_road_class)
#change level name
accident_df$first_road_class[accident_df$first_road_class==1]<-"MOT"
accident_df$first_road_class[accident_df$first_road_class==2]<-"MOT"
accident_df$first_road_class[accident_df$first_road_class==3]<-"A"
accident_df$first_road_class[accident_df$first_road_class==4]<-"B"
accident_df$first_road_class[accident_df$first_road_class==5]<-"C"
#make factor
accident_df$first_road_class <- as.factor(accident_df$first_road_class)

#road_type
#check NA
sum(is.na(accident_df$road_type))
#create NA VARIABLES
accident_df$road_type[accident_df$road_type==9]<- NA
accident_df$road_type[accident_df$road_type==-1]<- NA
accident_df$road_type[accident_df$road_type==12]<- NA
#check factor levels
accident_df %>% count(road_type)
#change variable names to make interpretation easier
accident_df$road_type[accident_df$road_type==1]<-"RB"
accident_df$road_type[accident_df$road_type==2]<-"OWS"
accident_df$road_type[accident_df$road_type==3]<-"DC"
accident_df$road_type[accident_df$road_type==6]<-"SC"
accident_df$road_type[accident_df$road_type==7]<-"SR"
#make factor
accident_df$road_type<-as.factor(accident_df$road_type)

#speed limit
accident_df %>% count(speed_limit)
#NULL didn't work earlier so make NA using string "NULL"
accident_df$speed_limit[accident_df$speed_limit=="NULL"] <- NA
#check NA
sum(is.na(accident_df$speed_limit))
#convert to factor
accident_df$speed_limit<- as.factor(accident_df$speed_limit)

#junction_detail
#check NA
sum(is.na(accident_df$junction_detail))
#change to NA
accident_df$junction_detail[accident_df$junction_detail==99]<-NA
accident_df$junction_detail[accident_df$junction_detail==9]<-NA
#check factor levels
accident_df %>% count(junction_detail)
#change level names to make interpretation easier
accident_df$junction_detail[accident_df$junction_detail==0]<-"NOT"
accident_df$junction_detail[accident_df$junction_detail==1]<-"RB"
accident_df$junction_detail[accident_df$junction_detail==2]<-"MRB"
accident_df$junction_detail[accident_df$junction_detail==3]<-"TJUN"
accident_df$junction_detail[accident_df$junction_detail==5]<-"SR"
accident_df$junction_detail[accident_df$junction_detail==6]<-"CR"
accident_df$junction_detail[accident_df$junction_detail==7]<-"4ARM"
accident_df$junction_detail[accident_df$junction_detail==8]<-"PD"
#make factor
accident_df$junction_detail<-as.factor(accident_df$junction_detail)

#pedestrian_crossing_human_control
#check NA
sum(is.na(accident_df$pedestrian_crossing_control))
#change to NA
accident_df$pedestrian_crossing_human_control[accident_df$pedestrian_crossing_human_control==9]<-NA
#change level name - make yes or no due to low levels of not none answers
accident_df$pedestrian_crossing_human_control[accident_df$pedestrian_crossing_human_control==0]<- "NO" 
accident_df$pedestrian_crossing_human_control[accident_df$pedestrian_crossing_human_control==1]<- "YES"
accident_df$pedestrian_crossing_human_control[accident_df$pedestrian_crossing_human_control==2]<- "YES" 
#make factor
accident_df$pedestrian_crossing_human_control <- as.factor(accident_df$pedestrian_crossing_human_control)
#check levels
accident_df %>% count(pedestrian_crossing_human_control)

#pedestrian physical control
#check NA
sum(is.na(accident_df$pedestrian_crossing_physical_facilities))
#make unknown NA
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==9]<-NA
#change level name - change to yes or no for same reason as other pedestrian
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==0] <- "NO"
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==1] <- "YES"
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==4] <- "YES"
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==5] <- "YES"
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==7] <- "YES"
accident_df$pedestrian_crossing_physical_facilities[accident_df$pedestrian_crossing_physical_facilities==8] <- "YES"
#make factor
accident_df$pedestrian_crossing_physical_facilities <- as.factor(accident_df$pedestrian_crossing_physical_facilities)
#check levels
accident_df %>% count(pedestrian_crossing_physical_facilities)

#light conditions
#check Na
sum(is.na(accident_df$light_conditions))
#convert to NA
accident_df$light_conditions[accident_df$light_conditions==7]<-NA
#check factor levels
accident_df %>% count(light_conditions)
#change level name
accident_df$light_conditions[accident_df$light_conditions==1]<-"DAY"
accident_df$light_conditions[accident_df$light_conditions==4]<-"LIT"
accident_df$light_conditions[accident_df$light_conditions==5]<-"UNLIT"
accident_df$light_conditions[accident_df$light_conditions==6]<-"NL"
#convert to factor
accident_df$light_conditions<-as.factor(accident_df$light_conditions)

#weather_conditions
#check NA
sum(is.na(accident_df$weather_conditions))
#convert to NA
accident_df$weather_conditions[accident_df$weather_conditions==9]<-NA
accident_df$weather_conditions[accident_df$weather_conditions==8]<-NA
#check levels
accident_df %>% count(weather_conditions)

#high_winds
accident_df$high_winds<-NA

for(i in 1:nrow(accident_df)) {
  if (is.na(accident_df$weather_conditions[i])) {
    accident_df$high_winds[i] <- NA
  }
  else if ((accident_df$weather_conditions[i]<4) || (accident_df$weather_conditions[i]==7)){
    accident_df$high_winds[i] <- "NO"
  }
  
  else if ((accident_df$weather_conditions[i]>3) & (accident_df$weather_conditions[i]<7)){
    accident_df$high_winds[i] <- "YES"
  }
}

accident_df %>% count(high_winds)

#combine high winds and non high-winds
accident_df$weather_conditions[accident_df$weather_conditions==4]<-1
accident_df$weather_conditions[accident_df$weather_conditions==5]<-2
accident_df$weather_conditions[accident_df$weather_conditions==6]<-3

#change level names
accident_df$weather_conditions[accident_df$weather_conditions==1]<-"FINE"
accident_df$weather_conditions[accident_df$weather_conditions==2]<-"RAIN"
accident_df$weather_conditions[accident_df$weather_conditions==3]<-"SNOW"
accident_df$weather_conditions[accident_df$weather_conditions==7]<-"FOG"
#check levels
accident_df %>% count(weather_conditions)
#make factor
accident_df$weather_conditions<-as.factor(accident_df$weather_conditions)

#road_surface_conditions
#CHECK na
sum(is.na(accident_df$road_surface_conditions))
#convert to NA
accident_df$road_surface_conditions[accident_df$road_surface_conditions==9]<-NA
#check levels
accident_df %>% count(accident_df$road_surface_conditions)
#change level names
accident_df$road_surface_conditions[accident_df$road_surface_conditions==1]<-"DRY"
accident_df$road_surface_conditions[accident_df$road_surface_conditions==2]<-"WET"
accident_df$road_surface_conditions[accident_df$road_surface_conditions==3]<-"SNOW"
accident_df$road_surface_conditions[accident_df$road_surface_conditions==4]<-"ICE"
accident_df$road_surface_conditions[accident_df$road_surface_conditions==5]<-"FLOOD"
#make factor
accident_df$road_surface_conditions<-as.factor(accident_df$road_surface_conditions)

#special conditions
#check NA
sum(is.na(accident_df$special_conditions_at_site))
#convert NA
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==9]<-NA
#check levels
accident_df %>% count(special_conditions_at_site)
#change levels name
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==0]<-"NONE"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==1]<-"AFSO"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==2]<-"ASPD"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==3]<-"RSO"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==4]<-"RW"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==5]<-"RSD"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==6]<-"OIL"
accident_df$special_conditions_at_site[accident_df$special_conditions_at_site==7]<-"MUD"
#make factor
accident_df$special_conditions_at_site<-as.factor(accident_df$special_conditions_at_site)

#carriageway hazards
#check NA
sum(is.na(accident_df$carriageway_hazards))
#convert NA
accident_df$carriageway_hazards[accident_df$carriageway_hazards==9]<-NA
#check levels
accident_df %>% count(carriageway_hazards)
#change level names
accident_df$carriageway_hazards[accident_df$carriageway_hazards==0]<-"NONE"
accident_df$carriageway_hazards[accident_df$carriageway_hazards==1]<-"LOAD"
accident_df$carriageway_hazards[accident_df$carriageway_hazards==2]<-"OBJ"
accident_df$carriageway_hazards[accident_df$carriageway_hazards==3]<-"ACC"
accident_df$carriageway_hazards[accident_df$carriageway_hazards==6]<-"PED"
accident_df$carriageway_hazards[accident_df$carriageway_hazards==7]<-"ANI"
#convert to factor
accident_df$carriageway_hazards<-as.factor(accident_df$carriageway_hazards)

#urban or rural
#check NA
sum(is.na(accident_df$urban_or_rural_area))
#check levels
accident_df %>% count(urban_or_rural_area)
#make unallocated NA
accident_df$urban_or_rural_area[accident_df$urban_or_rural_area==3]<-NA
#change level name
accident_df$urban_or_rural_area[accident_df$urban_or_rural_area==1]<-"URB"
accident_df$urban_or_rural_area[accident_df$urban_or_rural_area==2]<-"RUR"
#make factor
accident_df$urban_or_rural_area<-as.factor(accident_df$urban_or_rural_area)

#trunk_road_flag
#check NA
sum(is.na(accident_df$trunk_road_flag))
#change level name
accident_df$trunk_road_flag[accident_df$trunk_road_flag==1] <- "TRUNK"
accident_df$trunk_road_flag[accident_df$trunk_road_flag==2] <- "NOT"
#make factor 
accident_df$trunk_road_flag <- as.factor(accident_df$trunk_road_flag)
#check levels
accident_df %>% count(trunk_road_flag)


##VISUALISE ACCIDENT SEVERITY DIFFERENCES IN NUMERICAL DATA
#histograms
accident_df$longitude <- as.numeric(accident_df$longitude)

#VISUALISE ACCIDENT SEVERITY DIFFERENCES IN CATEGORICAL DATA
#(modified from julia silge's blog)
str(accident_df)

accident_df <- accident_df %>% mutate_if(is.integer,as.factor)
accident_df <- accident_df %>% mutate_if(is.numeric,as.factor)

accident_df %>%
  select(accident_severity, accident_year,day_of_week,first_road_class,
         road_type,speed_limit,junction_detail,
  ) %>% pivot_longer(accident_year:junction_detail) %>%
  ggplot(aes(y = value, fill = accident_severity)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)

accident_df %>%
  select(accident_severity, pedestrian_crossing_human_control, pedestrian_crossing_physical_facilities, 
         light_conditions,weather_conditions
  ) %>% pivot_longer(pedestrian_crossing_human_control:weather_conditions) %>%
  ggplot(aes(y = value, fill = accident_severity)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)

accident_df %>%
  select(accident_severity, high_winds, road_surface_conditions,
         urban_or_rural_area,trunk_road_flag
  ) %>% pivot_longer(high_winds:trunk_road_flag) %>%
  ggplot(aes(y = value, fill = accident_severity)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)

accident_df %>%
  select(accident_severity,carriageway_hazards, special_conditions_at_site
  ) %>% pivot_longer(carriageway_hazards:special_conditions_at_site) %>%
  ggplot(aes(y = value, fill = accident_severity)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)

#change day of week to whether week day or weekend
#check NA
sum(is.na(accident_df$day_of_week))
#change to weekday or not
accident_df$day_of_week <- as.integer(accident_df$day_of_week)
accident_df$day_of_week[accident_df$day_of_week==1] <- "NO"
accident_df$day_of_week[accident_df$day_of_week==7] <- "NO"
accident_df$day_of_week[accident_df$day_of_week==2] <- "YES"
accident_df$day_of_week[accident_df$day_of_week==3] <- "YES"
accident_df$day_of_week[accident_df$day_of_week==4] <- "YES"
accident_df$day_of_week[accident_df$day_of_week==5] <- "YES"
accident_df$day_of_week[accident_df$day_of_week==6] <- "YES"
#make factor
accident_df$day_of_week <- as.factor(accident_df$day_of_week)
#check factor levels
accident_df %>% count(day_of_week)

#CHECK MISSING VALUES AGAIN
vis_miss(accident_df, warn_large_data = FALSE)
colSums(is.na(accident_df))
#missing 2.7%
#first road class 33.59%

str(accident_df)

#remove road_class_type for one dataset
prep_data <- accident_df %>% select(-c(first_road_class))
prep_data <- na.omit(prep_data)
write.csv(prep_data, "prepped_data.csv",row.names = FALSE)

#keep dataset with road class
roadclass_df <- na.omit(accident_df)
write.csv(roadclass_df, "roadclass_df.csv",row.names = FALSE)


