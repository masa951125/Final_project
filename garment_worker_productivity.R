rm(list=ls())
library(tidyverse)
library(caret)

url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/00597/garments_worker_productivity.csv"
garment <-read.csv(url,stringsAsFactors=T)

str(garment)
summary(garment)

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(garment$actual_productivity, p=0.2, list=F, times=1)
test_set <- garment[test_index,]
train_set <- garment[-test_index,]
dummy_train_set <- train_set %>% select(-wip)

#knn
train_knn <- train(actual_productivity ~ .,
                   method = "knn",
                   data = dummy_train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
knn_preds <- predict(train_knn, test_set)
RMSE(knn_preds,test_set$actual_productivity)
install.packages("pROC")
library(pROC)

roc(knn_preds,test_set$actual_productivity)

#glm
train_glm  <- train(actual_productivity ~ .,
                    method = "glm",
                    data = dummy_train_set)

glm_pred <- predict(train_glm, test_set)
RMSE(glm_pred, test_set$actual_productivity)

#lm
train_lm  <- train(actual_productivity ~ .,
                    method = "lm",
                    data = dummy_train_set)

lm_pred <- predict(train_lm, test_set)
RMSE(glm_pred, test_set$actual_productivity)

install.packages("psych")
library(psych)
pairs.panels(garment[,5:15])
