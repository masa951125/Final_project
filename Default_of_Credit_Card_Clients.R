library(tidyverse)
library(caret)
install.packages("DataExplorer")
library(DataExplorer)

url <-"https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"
download.file(url, "original_default.csv")
original_default <- read_csv("original_default.csv")

#split, validation, default
set.seed(2021, sample.kind = "Rounding")
validation_index <- createDataPartition(original_default$default.payment.next.month, p=0.1, list=F, times=1)
validation <- original_default[validation_index,]
default <- original_default[-validation_index,]
#we will use validataion to evaluate the model

str(default)
summary(default)
head(default)



plot_histogram(default)

names(default)
#[1] "ID"                         "LIMIT_BAL"                 
#[3] "SEX"                        "EDUCATION"                 
#[5] "MARRIAGE"                   "AGE"                       
#[7] "PAY_0"                      "PAY_2"                     
#[9] "PAY_3"                      "PAY_4"                     
#[11] "PAY_5"                      "PAY_6"                     
#[13] "BILL_AMT1"                  "BILL_AMT2"                 
#[15] "BILL_AMT3"                  "BILL_AMT4"                 
#[17] "BILL_AMT5"                  "BILL_AMT6"                 
#[19] "PAY_AMT1"                   "PAY_AMT2"                  
#[21] "PAY_AMT3"                   "PAY_AMT4"                  
#[23] "PAY_AMT5"                   "PAY_AMT6"                  
#[25] "default.payment.next.month"

#LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit
ggplot(data=default, aes(LIMIT_BAL))+geom_histogram()

#SEX: Gender (1=male, 2=female)
ggplot(data=default, aes(SEX-1))+geom_bar()


#test_set, train_set
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(default$default.payment.next.month, p=0.1, list=F, times=1)
test_set <- default[test_index,]
train_set <- default[-test_index,]


#knn
train_knn <- train(default.payment.next.month ~ .,
                   method = "knn",
                   data = train_set)

train_knn$bestTune
knn_preds <- predict(train_knn, test_set)
