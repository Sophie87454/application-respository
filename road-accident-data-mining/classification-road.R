#CLASSIFICATION

library(tidyverse)
library(rsample)
library(caret)
library(party)
library(randomForest)

rm(list=ls())

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Road safety")

#import datasets
accident_df <- read.csv("prepped_data.csv")


str(accident_df)


###REMOVE accident_index, accident_year, longitude, latitude
accident_df <- accident_df %>% select(-c(accident_index, accident_year,
                                          longitude, latitude))


#make all factors
str(accident_df)
accident_df <- accident_df %>% mutate_if(is.character,as.factor) 
accident_df <- accident_df %>% mutate_if(is.integer,as.factor) 
accident_df$number_of_vehicles <- as.integer(accident_df$number_of_vehicles)
accident_df$number_of_casualties <- as.integer(accident_df$number_of_casualties)
accident_df$month <- as.integer(accident_df$month)
accident_df$hour <- as.integer(accident_df$hour)




#split dataset
set.seed(123)

accident_split <- accident_df %>% initial_split(prop=0.7, strata=accident_severity)
accident_train <- training(accident_split)
accident_train <- downSample(accident_train, accident_train$accident_severity)
accident_train <- accident_train %>% select(-Class)
accident_test <- testing(accident_split)

accident_test %>% count(accident_severity)
accident_train %>% count(accident_severity)




##DECISION TREE
control <- trainControl(method = "cv", number = 10)

###use cross validation to find optimal tree

accident_dt <- train(accident_severity~., accident_train, method="ctree",
                     trControl= control)
accident_dt

###employ decision tree using suggested mincriterion
accidentTree <- ctree(accident_severity~.,data=accident_train,
                      control = ctree_control(mincriterion = 0.5, 
                                              maxdepth = 5))

print(accidentTree)
plot(accidentTree)

#get model accuracy
accidentPred <- predict(accidentTree, newdata= accident_test)
confusionMatrix(accidentPred, accident_test$accident_severity, mode = "everything")




##RANDOM FOREST
##random forest with cross validation


accident_rf <- train(accident_severity~., accident_train, method="rf",
                      trControl= control)
accident_rf

#random forest optimal model

accidentRF <- randomForest(accident_severity~.,data=accident_train,
                           mtry=2)
accidentRF

plot(accidentRF)
varImpPlot(accidentRF) 
importance(accidentRF)

#accuracy
accidentPred <- predict(accidentRF, newdata= accident_test)
confusionMatrix(accidentPred, accident_test$accident_severity, mode = "everything")
