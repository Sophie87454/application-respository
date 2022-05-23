#ROAD SAFETY
#ASSOCIATION RULES

library(tidyverse)
library(arules)
library(arulesViz) 
library(psych)

rm(list=ls())

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Road safety")

#import datasets
accident_df <- read.csv("prepped_data.csv")

###REMOVE accident_index, accident_year, longitude, latitude
accident_df <- accident_df %>% select(-c(accident_index, accident_year,
                                          longitude, latitude))


#change variable type
str(accident_df)
accident_df <- accident_df %>% mutate_if(is.character,as.factor) 
accident_df <- accident_df %>% mutate_if(is.integer,as.factor) 
accident_df$number_of_vehicles <- as.integer(accident_df$number_of_vehicles)
accident_df$number_of_casualties <- as.integer(accident_df$number_of_casualties)
accident_df$month <- as.integer(accident_df$month)
accident_df$hour <- as.integer(accident_df$hour)




for (i in 1:ncol(accident_df)){
 print(prop.table(table(accident_df[,i])))
}
  
fatal_df <- accident_df %>% filter(accident_severity=="FATAL") %>% select(-accident_severity)
for (i in 1:ncol(fatal_df)){
  print(prop.table(table(fatal_df[,i])))
}

serious_df <- accident_df %>% filter(accident_severity=="SERIOUS") %>% select(-accident_severity)
for (i in 1:ncol(serious_df)){
  print(prop.table(table(serious_df[,i])))
}

slight_df <- accident_df %>% filter(accident_severity=="SLIGHT") %>% select(-accident_severity)
for (i in 1:ncol(slight_df)){
  print(prop.table(table(slight_df[,i])))
}


describe(accident_df)
describe(fatal_df)
describe(serious_df)
describe(slight_df)


#ASSOCIATION RULE MINING

#fatal as consequent
rules <- apriori(accident_df, parameter=list(minlen=2, conf=0.1, supp=0.1),
                 appearance=list(rhs=c("accident_severity=FATAL")))


#serious as consequent
rules <- apriori(accident_df, parameter=list(),
                 appearance=list(rhs=c("accident_severity=SERIOUS")))


#slight as consequent
rules <- apriori(accident_df, parameter=list(minlen=2, maxlen=4, conf=0.6, supp=0.4),
                 appearance=list(rhs=c("accident_severity=SLIGHT")))
rules.sorted<-sort(rules, by="lift")

redundant <- is.redundant(rules.sorted)
which(redundant)

rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

inspect(head(rules.pruned, 20))  





#FATAL ACCIDENTS ONLY
rules <- apriori(fatal_df, parameter= list(minlen=2, maxlen=3, supp=0.4, conf=0.5))

redundant <- is.redundant(rules)
which(redundant)
rules.pruned <- rules[!redundant]

plot(rules.pruned)
plot(rules.pruned, method="grouped") 

rules.support<-sort(rules.pruned, by="support")

rules.lift<-sort(rules.pruned, by="lift")


inspect(rules.support)
inspect(head(rules.support, 20))  
plot(rules.support, method = "graph", limit = 20)

inspect(rules.lift)
table(inspect(head(rules.lift, 20))) 
plot(rules.lift, method = "graph", limit = 20)




#SERIOUS ACCIDENTS ONLY 
rules <- apriori(serious_df, parameter= list(minlen=2, maxlen=4, supp=0.4, conf=0.7))

redundant <- is.redundant(rules)
which(redundant)
rules.pruned <- rules[!redundant]

plot(rules.pruned)
plot(rules.pruned, method="grouped") 

rules.support<-sort(rules, by="support")

rules.lift<-sort(rules, by="lift")


inspect(rules.support)
inspect(head(rules.support, 20))  
plot(rules.support, method = "graph", limit = 20)

inspect(rules.lift)
inspect(head(rules.lift, 20))  
plot(rules.support, method = "graph", limit = 20)




#SLIGHT ACCIDENTS ONLY
rules <- apriori(slight_df, parameter= list(minlen=2, maxlen=4, supp=0.4, conf=0.7))

redundant <- is.redundant(rules)
which(redundant)
rules.pruned <- rules[!redundant]

plot(rules.pruned)
plot(rules.pruned, method="grouped") 

rules.support<-sort(rules, by="support")

rules.lift<-sort(rules, by="lift")


inspect(rules.support)
inspect(head(rules.support, 20))  
plot(rules.support, method = "graph", limit = 20)

inspect(rules.lift)
inspect(head(rules.lift, 20))  
plot(rules.support, method = "graph", limit = 20)

