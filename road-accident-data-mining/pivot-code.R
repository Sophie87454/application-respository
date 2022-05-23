library(tidyverse)

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Road safety")

rm(list=ls())

#import accident dataset
df<- read.csv("prepped_data.csv")


accident_df <- df %>% select(-c(accident_year, longitude, latitude)) 



#box hour numbers
accident_df %>% count(hour)
accident_df$hour <- as.numeric(accident_df$hour)
accident_df$hour_group<-NA

for(i in 1:nrow(accident_df)) {
  if (is.na(accident_df$hour[i])) {
    accident_df$hour_group[i] <- NA
  }
  else if ((accident_df$hour[i]<4)){
    accident_df$hour_group[i] <- "0-3"
  }
  else if ((accident_df$hour[i]>3) & (accident_df$hour[i]<8)){
    accident_df$hour_group[i] <- "4-7"
  }
  else if ((accident_df$hour[i]>7) & (accident_df$hour[i]<12)){
    accident_df$hour_group[i] <- "8-11"
  }
  else if ((accident_df$hour[i]>11) & (accident_df$hour[i]<16)){
    accident_df$hour_group[i] <- "12-15"
  }
  else if ((accident_df$hour[i]>15) & (accident_df$hour[i]<20)){
    accident_df$hour_group[i] <- "16-19"
  }
  else if ((accident_df$hour[i]>19) & (accident_df$hour[i]<24)){
    accident_df$hour_group[i] <- "20-23"
  }
}
accident_df %>% count(hour_group)


#box hour numbers
accident_df %>% count(month)
accident_df$month <- as.numeric(accident_df$month)
accident_df$season<-NA

for(i in 1:nrow(accident_df)) {
  if (is.na(accident_df$month[i])) {
    accident_df$season[i] <- NA
  }
  else if ((accident_df$month[i]<3)){
    accident_df$season[i] <- "WINTER"
  }
  else if ((accident_df$month[i]>2) & (accident_df$month[i]<6)){
    accident_df$season[i] <- "SPRING"
  }
  else if ((accident_df$month[i]>5) & (accident_df$month[i]<9)){
    accident_df$season[i] <- "SUMMER"
  }
  else if ((accident_df$month[i]>8) & (accident_df$month[i]<12)){
    accident_df$season[i] <- "AUTUMN"
  }
  else if (accident_df$month[i]==12){
    accident_df$season[i] <- "WINTER"
  }
  
}
accident_df %>% count(season)

#number of vehicles and number of casualties change to factor
accident_df$number_of_casualties[accident_df$number_of_casualties>5]<-"5+"
accident_df$number_of_casualties[accident_df$number_of_vehicles>5]<- "5+"


accident_df <- accident_df %>% mutate_if(is.character,as.factor) 
accident_df <- accident_df %>% mutate_if(is.integer,as.factor) 

names(accident_df)

accident_df <- accident_df %>% select(-c(hour, month))

pivot_df <- accident_df %>% pivot_longer(-c(accident_index), 
                                         names_to = "attribute", values_to = "character") %>%
  unite(attributes,c(attribute,character), sep=' - ')

write.csv(pivot_df, "pivot_df.csv",row.names = FALSE)

fatal_pivot <- accident_df %>% filter(accident_severity=="FATAL") %>% select(-accident_severity) %>%
  pivot_longer(-c(accident_index), names_to = "attribute", values_to = "character") %>%
  unite(attributes,c(attribute,character), sep=' - ')
write.csv(fatal_pivot, "fatal_pivot.csv",row.names = FALSE)

serious_pivot <- accident_df %>% filter(accident_severity=="SERIOUS") %>% select(-accident_severity) %>%
  pivot_longer(-c(accident_index), names_to = "attribute", values_to = "character") %>%
  unite(attributes,c(attribute,character), sep=' - ')
write.csv(serious_pivot, "serious_pivot.csv",row.names = FALSE)

slight_pivot <- accident_df %>% filter(accident_severity=="SLIGHT") %>% select(-accident_severity) %>%
  pivot_longer(-c(accident_index), names_to = "attribute", values_to = "character") %>%
  unite(attributes,c(attribute,character), sep=' - ')
write.csv(slight_pivot, "slight_pivot.csv",row.names = FALSE)
