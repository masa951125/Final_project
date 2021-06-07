rm(list=ls())

library(tidyverse)
library(caret)

url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/00597/garments_worker_productivity.csv"
garment <-read.csv(url,stringsAsFactors=T)

str(garment)
summary(garment)

#data cleansing#

#1 department "finishing"? "finishing ", and "sweing"?

levels(garment$department)
#[1] "finishing"  "finishing " "sweing" 

garment$department <- garment$department%>% 
  fct_recode(finishing ="finishing", finishing= "finishing ", sewing= "sweing")
str(garment)

#2 wip

#impute mean
mean(garment$wip, na.rm=TRUE)
#[1] 1190.466

index_NA_wip <- which(is.na(garment$wip))
garment$wip[index_NA_wip]<-mean(garment$wip, na.rm=TRUE)
summary(garment)

#data exploration#

#response value actual_productivity
ggplot(data= garment, aes(actual_productivity))+geom_histogram(color="black")
mu <-mean(garment$actual_productivity)

RMSE(garment$actual_productivity,mu)
#[1] 0.174415

#test_set, train_set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(garment$actual_productivity, p=0.2, list=F, times=1)
test_set <- garment[test_index,]
train_set <- garment[-test_index,]


#knn
train_knn <- train(actual_productivity ~ .,
                   method = "knn",
                   data = train_set)

train_knn$bestTune
knn_preds <- predict(train_knn, test_set)
RMSE(knn_preds,test_set$actual_productivity)

#glm
train_glm  <- train(actual_productivity ~ .,
                    method = "glm",
                    data = train_set)

glm_pred <- predict(train_glm, test_set)
RMSE(glm_pred, test_set$actual_productivity)

#lm
train_lm  <- train(actual_productivity ~ .,
                    method = "lm",
                    data = train_set)

lm_pred <- predict(train_lm, test_set)
RMSE(glm_pred, test_set$actual_productivity)

install.packages("psych")
library(psych)
pairs.panels(garment[,5:15])
