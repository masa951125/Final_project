



install.packages("naniar")

library(tidyverse)
library(caret)
library(randomForest)
library(naniar) # looking for missing values
library(corrplot)

#kaggle data
#https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset/download

#my GitHub data https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv
url <- "https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"

#download the data
download.file(url,"client_data.csv")
client_data <-read.csv("client_data.csv")

head(client_data)

summary(client_data)

str(client_data)
#'data.frame':	30000 obs. of  25 variables:
#$ ID                        : int  1 2 3 4 5 6 7 8 9 10 ...
#$ LIMIT_BAL                 : num  20000 120000 90000 50000 50000 50000 500000 100000 140000 20000 ...
#$ SEX                       : int  2 2 2 2 1 1 1 2 2 1 ...
#$ EDUCATION                 : int  2 2 2 2 2 1 1 2 3 3 ...
#$ MARRIAGE                  : int  1 2 2 1 1 2 2 2 1 2 ...
#$ AGE                       : int  24 26 34 37 57 37 29 23 28 35 ...
#$ PAY_0                     : int  2 -1 0 0 -1 0 0 0 0 -2 ...
#$ PAY_2                     : int  2 2 0 0 0 0 0 -1 0 -2 ...
#$ PAY_3                     : int  -1 0 0 0 -1 0 0 -1 2 -2 ...
#$ PAY_4                     : int  -1 0 0 0 0 0 0 0 0 -2 ...
#$ PAY_5                     : int  -2 0 0 0 0 0 0 0 0 -1 ...
#$ PAY_6                     : int  -2 2 0 0 0 0 0 -1 0 -1 ...
#$ BILL_AMT1                 : num  3913 2682 29239 46990 8617 ...
#$ BILL_AMT2                 : num  3102 1725 14027 48233 5670 ...
#$ BILL_AMT3                 : num  689 2682 13559 49291 35835 ...
#$ BILL_AMT4                 : num  0 3272 14331 28314 20940 ...
#$ BILL_AMT5                 : num  0 3455 14948 28959 19146 ...
#$ BILL_AMT6                 : num  0 3261 15549 29547 19131 ...
#$ PAY_AMT1                  : num  0 0 1518 2000 2000 ...
#$ PAY_AMT2                  : num  689 1000 1500 2019 36681 ...
#$ PAY_AMT3                  : num  0 1000 1000 1200 10000 657 38000 0 432 0 ...
#$ PAY_AMT4                  : num  0 1000 1000 1100 9000 ...
#$ PAY_AMT5                  : num  0 0 1000 1069 689 ...
#$ PAY_AMT6                  : num  0 2000 5000 1000 679 ...
#$ default.payment.next.month: int  1 1 0 0 0 0 0 0 0 0 ...

#check NAs
vis_miss(client_data)

#correlation plot 
names(client_data)
sapply(client_data, class)

client_data$default.payment.next.month <- as.numeric(client_data$default.payment.next.month)
r = cor(client_data[-c(3, 4, 5)])
corrplot(r)
plot_histo

#plot client_data$default.payment.next.month

client_data%>% ggplot(aes(default.payment.next.month)) + geom_bar()

sum(client_data$default.payment.next.month==1)
mean(client_data$default.payment.next.month==1)
#[1] 6636 [1] 0.2212

sum(client_data$default.payment.next.month==0)
p <-mean(client_data$default.payment.next.month==0)

set.seed(NULL)
B <-1000
default_simple <- replicate(B,{
  d <- sample(c(0,1),30000,prob=c(p, 1-p),replace=T)
 }) 

mean(default_simple[,145]==client_data$default.payment.next.month)

sample <-sample(c(0,1),30000,prob=c(p, 1-p),replace=T)
mean(sample==client_data$default.payment.next.month)


mean(client_data$default.payment.next.month==0)
#[1] 23364 [1] 0.7788

#high default rate!

rowMeans(default_simple)
mean(default_simple[,1:1000]==client_data$default.payment.next.month)

plot_correlation(na.omit(data), maxcat = 5L)