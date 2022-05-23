#ROAD SAFETY 
#CRISP DM --- DATA PROCESSING

library(tidyverse)
library(esquisse)
library(ggplot2)

rm(list=ls())

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\Documents\\University\\Data Science\\ASDM\\Project\\Road safety")

#accident dataset
accident_df <- read.csv("prepped_data.csv")

head(accident_df)
str(accident_df)
summary(accident_df)
names(accident_df)
colSums(is.na(accident_df))


accident_df <- accident_df %>% mutate_if(is.character,as.factor)
accident_df$speed_limit <- as.factor(accident_df$speed_limit)
accident_df$hour <- as.integer(accident_df$hour)
accident_df$month <- as.factor(accident_df$month)
accident_df$high_winds <- as.factor(accident_df$high_winds)
accident_df$accident_year <- as.factor(accident_df$accident_year)


#boruta
library(Boruta)
dev.off()
importance <- Boruta(accident_severity ~ ., data=accident_df)
plot(importance, las=2, cex.axis=0.8)




#visualise time relationship


#visualise date relationship

#CORRELATION
names(accident_df)
COR <- accident_df %>% select(accident_severity, accident_year, location_easting_osgr, location_easting_osgr,
                              longitude, latitude, number_of_casualties, number_of_vehicles, date, day_of_week,
                              time, first_road_class, road_type, speed_limit, junction_detail, junction_control,
                              second_road_class, pedestrian_crossing_human_control, pedestrian_crossing_physical_facilities,
                              light_conditions, weather_conditions, road_surface_conditions, special_conditions_at_site,
                              carriageway_hazards, urban_or_rural_area, trunk_road_flag, month, hour)
library(vcd)

empty_m <- matrix(ncol = length(COR),
                  nrow = length(COR),
                  dimnames = list(names(COR), 
                                  names(COR)))
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m ,cor)

corrplot(cor_matrix)





#visualise potential relationship of location and accident severity


ggplot(accident_df) +
  aes(
    x = as.numeric(location_easting_osgr),
    y = as.numeric(location_northing_osgr),
    colour = accident_severity
  ) +
  geom_point(shape = "circle", size = 0.75) +
  scale_color_hue(direction = 1) +
  theme_minimal()



