library(tidyverse)
library(caret)
install.packages("DataExplorer")
library(DataExplorer)

url <-"https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"
download.file(url, "original_default.csv")
original_default <- read_csv("original_default.csv")

str(original_default)
summary(original_default)
#no NAs
plot_histogram(original_default, nrow=3, ncol=4)
plot_correlation(original_default)

####################
#logistic regression
####################

#remove ID
original_default <- original_default %>% select(-ID)
class(original_default)
str(original_default)

#scaling
ncol(original_default)
original_default[,1:23] <- scale(original_default[,1:23])

#train_set, test_set
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(original_default$default.payment.next.month, p=0.2, list=F, times=1)
test_set <- original_default[test_index,]
train_set <- original_default[-test_index,]

#model
glm_default <- glm(default.payment.next.month ~., data = train_set, family = binomial(link = "logit"))
summary(glm_default)
glm_pred <- predict(glm_default, test_set, type="response")
head(glm_pred)
glm_result<- ifelse(glm_pred > 0.5, 1, 0)

table(glm_result,test_set$default.payment.next.month)
confusionMatrix(glm_result,test_set$default.payment.next.month)
accuracy <- table(glm_result,test_set$default.payment.next.month)
sum(diag(accuracy))/sum(accuracy)

confusionMatrix(as.factor(glm_result),as.factor(test_set$default.payment.next.month))
###







#some of its columns are categorical value. So factorize them.
#change PAY_0 into PAY_1,default.payment.next.month into default

original_default <- original_default%>% mutate(
  SEX = as.factor(SEX),
  EDUCATION =as.factor(EDUCATION),
  MARRIAGE = as.factor(MARRIAGE),
  PAY_0 = as.factor(PAY_0),
  PAY_2 = as.factor(PAY_2),
  PAY_3 = as.factor(PAY_3),
  PAY_4 = as.factor(PAY_4),
  PAY_5 = as.factor(PAY_5),
  PAY_6 = as.factor(PAY_6),
  default.payment.next.month = as.factor(default.payment.next.month)
)
str(original_default)

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


#glm
train_set[,1:23] <-scale(train_set[,1:23])

train_glm <- glm(default.payment.next.month ~ .,
                 data= train_set,
                 family="binomial")

pred_lm <- predict(train_lm, test_set)