###########################
#package used in this code
###########################
#tidyverse, gridExtra, caret, rpart, DataExplorer, rager

if(!require(tidyverse)) install.packages("tidyverse") 
#basic library
if(!require(gridExtra)) install.packages("gridExtra") 
#expansion of ggplot
if(!require(caret)) install.packages("caret") 
#cross validation 
if(!require(rpart)) install.packages("rpart") 
#to make decision tree model
if(!require(rpart.plot)) install.packages("rpart.plot") 
#to plot decision tree
if(!require(DataExplorer)) install.packages("DataExplorer") 
#to do data exploration
if(!require(ranger)) install.packages("ranger") 
#random forest. new package that is much faster than "randomForest" package

######
#data
######
#data is stored in my GitHub repository.

url <-"https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"
#my GitHub direct link to the file

download.file(url, "original_default.csv")
#downloading a file into your working folder.

original_default <- read_csv("original_default.csv")
#using "read_csv" to download the file. 
#we will get the dataset whose values are numerical.

################################################################################
#data exploration
################################################################################

#check dataset
dim(original_default)
str(original_default)

sum(is.null(original_default))
sum(is.na(original_default))
sum(complete.cases(original_default))
#no NAs

#correlation
plot_correlation(original_default)
# DEFAULT has relatively strong correlations in terms of PAY, and PAY_AMT 

###########
#1 outcome
###########

#show distribution graph
ggplot(data=original_default, aes(default.payment.next.month)) +geom_bar()

#change name of "default.payment.next.month"
n <-which(names(original_default)=="default.payment.next.month")
names(original_default)[n] <- "DEFAULT"

#show distribution graph
ggplot(data=original_default, aes(default.payment.next.month)) +geom_bar()

#show proportion of 0,1
prop.table(table(original_default$default.payment.next.month))

#change outcome into factor
original_default$DEFAULT <- as.factor(original_default$DEFAULT)

#############
#2 LIMIT_BAL
#############

summary(original_default$LIMIT_BAL)
#numeric data

ggplot(data=original_default, aes(LIMIT_BAL, fill=DEFAULT)) +geom_histogram(bins=30))
#distribution is skewed right

unique(original_default$LIMIT_BAL)
#######
#2 SEX
#######

prop.table(table(original_default$SEX))

original_default %>% ggplot(aes(x=SEX, fill= DEFAULT)) +
  geom_bar() +
  ggtitle("SEX")+
  stat_count(aes(label = ..count..), geom = "label")# illustrate numbers

#stacked bar graph
original_default %>% ggplot(aes(x= SEX, fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("SEX")
#There seemed to be little difference between genders.


#############
#3 EDUCATION
#############

original_default %>% ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("EDUCATION")+
  stat_count(aes(label = ..count..), geom = "label")# illustrate numbers

#stacked bar graph
original_default %>% ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("EDUCATION")
  
############
#4 marriage
############

summary(original_default$ MARRIAGE)
unique(original_default$ MARRIAGE)
#categorical data

#the data explanation says;
#https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
#MARRIAGE: Marital status (1=married, 2=single, 3=others)
#[1] 1 2 3 0
#O is not defined. 0 can be included in 3 

original_default$MARRIAGE <- ifelse(original_default$MARRIAGE== 0, 3,
                                     original_default$MARRIAGE)

original_default %>% ggplot(aes(x=as.factor(MARRIAGE), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("MARRIAGE")+
  stat_count(aes(label = ..count..), geom = "label")# illustrate numbers

#stack bar graph
original_default %>% ggplot(aes(x=as.factor(MARRIAGE), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("MARRIAGE")

# There seems to be little difference among the groups.

#######
#5 AGE
#######

summary(original_default$AGE)
#numeric data

ggplot(data=original_default, aes(AGE, fill=DEFAULT)) +geom_histogram()

#######
#6 PAY
#######

#PAY_0
summary(original_default$PAY_0)
unique(original_default$PAY_0)
#categorical data

#the data explanation says;
#https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
#Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 
#2=payment delay for two months, … 8=payment delay for eight months, 
#9=payment delay for nine months and above)

graph_p0 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_0")+
  stat_count(aes(label = ..count..), geom = "label")# illustrate numbers
graph_p0

#stack bar graph PAY_0
original_default %>% ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_0")

#PAY_2 ~ PAY_6 's structures are almost as same as PAY_0

#PAY_0
graph_p0 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_0")

#PAY_2
graph_p2 <-  
  original_default %>% ggplot(aes(x=as.factor(PAY_2), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_2")


#PAY_3
graph_p3 <-  
  original_default %>% ggplot(aes(x=as.factor(PAY_3), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_3")


#PAY_4
graph_p4 <-
original_default %>% ggplot(aes(x=as.factor(PAY_4), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_4")

#PAY_5
graph_p5 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_5), fill= DEFAULT)) +
  geom_bar() 
  ggtitle("PAY_5")

#PAY_6
graph_p6 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_6), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_6")


#to show graph side by side, we use "grid.arrange" function in "gridExtra" package
grid.arrange(graph_p0, graph_p2, graph_p3, graph_p4, graph_p5, graph_p6, nrow=2, ncol=3)

############
#7 BILL_AMT
############

summary(original_default$BILL_AMT1)
#numerical data

#the data explanation says;
#https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
#Amount of bill statement in September, 2005 (NT dollar)

#Plot BILL_AMT1
ggplot(data=original_default, aes(BILL_AMT1,fill= DEFAULT)) +geom_histogram()

#BILL_AMT1 ~ BILL_AMT6 's structures are almost the same

b1 <- ggplot(data=original_default, aes(BILL_AMT1)) +geom_histogram()
b2 <- ggplot(data=original_default, aes(BILL_AMT2)) +geom_histogram()
b3 <- ggplot(data=original_default, aes(BILL_AMT3)) +geom_histogram()
b4 <- ggplot(data=original_default, aes(BILL_AMT4)) +geom_histogram()
b5 <- ggplot(data=original_default, aes(BILL_AMT5)) +geom_histogram()
b6 <- ggplot(data=original_default, aes(BILL_AMT6)) +geom_histogram()

grid.arrange(b1,b2,b3,b4,b5,b6, nrow=2, ncol=3)
#they show almost similar distribution

##############
#7 pay amount 
##############

summary(original_default$PAY_AMT1)
#numerical data

#the data explanation says;
#https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
#Amount of previous payment in September, 2005 (NT dollar)

#plot PAY_AMT1
ggplot(data=original_default, aes(PAY_AMT1,fill= DEFAULT)) +geom_histogram()

p1 <- ggplot(data=original_default, aes(PAY_AMT1)) +geom_histogram()
p2 <- ggplot(data=original_default, aes(PAY_AMT2)) +geom_histogram()
p3 <- ggplot(data=original_default, aes(PAY_AMT3)) +geom_histogram()
p4 <- ggplot(data=original_default, aes(PAY_AMT4)) +geom_histogram()
p5 <- ggplot(data=original_default, aes(PAY_AMT5)) +geom_histogram()
p6 <- ggplot(data=original_default, aes(PAY_AMT6)) +geom_histogram()

grid.arrange(p1,p2,p3,p4,p5,p6, nrow=2, ncol=3)
#they show almost similar distribution

##################
#data preparation
##################

#remove ID
original_default <- original_default %>% select(-ID)

#categorical data, change numeric to factor
#SEX,EDUCATION,MARRIAGE, PAY_0~PAY_6  are categorical data
original_default <- original_default %>%
  mutate(SEX = as.factor(SEX),
         EDUCATION = as.factor(EDUCATION),
         MARRIAGE = as.factor(MARRIAGE),
         PAY_0 = as.factor(PAY_0),
         PAY_2 = as.factor(PAY_2),
         PAY_3 = as.factor(PAY_3),
         PAY_4 = as.factor(PAY_4),
         PAY_5 = as.factor(PAY_5),
         PAY_6 = as.factor(PAY_6) )

#scaling
# we use "scale" function to standardize predictors. 

#categorical data columns. we assume these can be defined as factors.
cat_col <- c("SEX", "EDUCATION", "MARRIAGE",
             "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "DEFAULT")
#all columns
all_col <- names(original_default)

#numerical data columns
num_col <- all_col[-which(all_col %in% cat_col)]

#scaling numerical data
original_default[num_col] <-original_default %>% select(-all_of(cat_col)) %>% scale()
str(original_default)
summary(original_default)

##################################################
#spliting into train_set,validation_set, test_set
##################################################

#First we split data into test_set, and default.Test_set will be only used as evaluation
#we use "createDataPartition" function in "caret" package
set.seed(2021, sample.kind = "Rounding")
index_1 <- createDataPartition(original_default$DEFAULT, p=0.2, list=F, times=1)
test_set <- original_default[index_1,]
default <- original_default[-index_1,]

#as we tune hyperparameters, we split default into train_set and validation_set.
#validation set will be used when tuning models

set.seed(2021, sample.kind = "Rounding")
index_2 <- createDataPartition(default$DEFAULT, p=0.2, list=F, times=1)
validation_set <-default[index_2,]
train_set <- default[-index_2,]

#check default ratio
#train_set
prop.table(table(train_set$DEFAULT))
#validation_set
prop.table(table(validation_set$DEFAULT))
#test_set
prop.table(table(test_set$DEFAULT))

#almost similar ratio

################################################################################
#model analysis
################################################################################

####################
#baseline prediction
####################

#all predicted as non_default make factor vectors 
base_pred <-factor(numeric(length(test_set$DEFAULT)),levels=c("0","1"))

#confusion matrix
confusionMatrix(base_pred, test_set$DEFAULT)
# Accuracy : 0.7787
# Sensitivity : 1.0000         
# Specificity : 0.0000   
# Balanced Accuracy : 0.5000

#we need to find models which exceed these values(except sensitivity)
#in this model, sensitivity is 1, but specificity is 0.
#this means the credit company falsely give credit to a person who fail to repay a debt.
#The loss for the company would be huge.

#evaluation method
#as this is a classification problem, we calculate accuracy using confusion matrix.
#however, as is shown in this baseline prediction, default rate is imbalanced. 
#as well as accuracy, we will pay attention to specificity and balanced accuracy.

#####################
#logistic regression 
#####################

#as this is a classification, we use logistic regression. we use glm function
#there are 24 predictors in the train_set. 
#we use "step regression" to find the best logistic regression model.
#stepwise regression explanation

#a null model with no predictors
null_model <- glm(DEFAULT~1, data = train_set, family = binomial(link = "logit"))

#q full model using all of the potential predictors
full_model <- glm(DEFAULT~., data = train_set, family = binomial(link = "logit"))

#forward and backward stepwise algorithm
step_mdl   <- step(null_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "both")

#predict
step_prob <- predict(step_mdl, validation_set,type="response")
step_pred <- ifelse(step_prob >0.5,1,0)

#to show accuracy we use confusionMatrix function in caret library
confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)

#make a table
results <- tibble(method = "logistic regresion", 
                  Accuracy =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$overall[1],#Accuracy
                  Sensitivity =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[1],#Sensitivity
                  Specificity =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[2],#Specificity
                  Balanced_Accuracy =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[11],#Balanced Accuracy
)
results %>% knitr::kable()

################################################################################

#######################
#decision tree default
#######################

#use CART classification and regression tree
#rpart ~ using default minsplit=20, cp=0.01

set.seed(2021, sample.kind = "Rounding")
rpart_mdl <-rpart(DEFAULT ~ .,data = train_set)

#prediction
rpart_pred <- predict(rpart_mdl, validation_set, type="class")

#confusion Matrix
confusionMatrix(rpart_pred, validation_set$DEFAULT)

#draw decision tree rpart.plot is good function to show decision tree clearly.
rpart.plot(rpart_mdl)

#find used features
rpart_mdl$variable.importance
#this model illustrates that PAY_0 is overwhelmingly important.

#make a table
results <- bind_rows(
  results,
  tibble(method="CART default",  
         Accuracy = confusionMatrix(rpart_pred, validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[2],#Specificity
         Balanced_Accuracy = confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[11]) #Balanced accuracy
         )

results %>% knitr::kable()

##############################
#decision tree further tuning
##############################
#we use "train" function in "caret" package. and tune cp
#cross validation 
#rpart ~tuning using smaller cp, less than 0.01

set.seed(2021, sample.kind = "Rounding")
rpart_tuned_mdl <- train(DEFAULT ~ ., 
                      method = "rpart", 
                      tuneGrid = data.frame(cp = seq(0, 0.01, len = 25)),
                      control = rpart.control(minsplit = 0),
                      data = train_set)

plot(rpart_tuned_mdl)
opt_cp <-rpart_tuned_mdl$bestTune

#draw decision tree
rpart.plot(rpart_tuned_mdl$finalModel)
#note: numeric values are scaled

#prediction
rpart_tuned_pred <- predict(rpart_tuned_mdl, validation_set)

#confusion matrix
confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)

#make a table
results <- bind_rows(
  results,
  tibble(method="CART tuned cp",  
         Accuracy = confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[2],#Specificity
         Balanced_Accuracy = confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[11]) #Balanced accuracy
)
results %>% knitr::kable()
################################################################################

#######################
#random forest default
#######################

library(ranger)

set.seed(2021, sample.kind = "Rounding")
rf_mdl <- ranger(
  formula = DEFAULT ~ ., 
  data = train_set,
  probability = F)

rf_mdl

rf_pred <- predict(rf_mdl, validation_set)$predictions

confusionMatrix(rf_pred, validation_set$DEFAULT)

#make a table

results <- bind_rows(
  results,
  tibble(method="random forest default",  
         Accuracy = confusionMatrix(rf_pred, validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[2],#Specificity
         Balanced_Accuracy = confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[11]) #Balanced accuracy
         )

results %>% knitr::kable()

################################
#random forest cross validation
################################
#grid search
modelLookup("ranger")

set.seed(2021, sample.kind = "Rounding")
rf_cv_mdl <- 
  train(
    DEFAULT~ .,
    data = train_set,
    method = 'ranger',
    metric = 'Accuracy',
    num.trees = 1000,
    tuneGrid = expand.grid(mtry = 3:10, splitrule = 'gini', min.node.size = 1), 
    trControl = trainControl(method = 'cv', number = 5))

plot(rf_cv_mdl)

#validation
rf_cv_pred <- predict(rf_cv_mdl, validation_set)

#confusion Matrix
confusionMatrix(rf_cv_pred, validation_set$DEFAULT)

#make a table
results <- bind_rows(
  results,
  tibble(method="random forest tuned ",  
         Accuracy = confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[2],#Specificity
         Balanced_Accuracy= confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[11])
  )

results %>% knitr::kable()

################################################################################
#evaluation
################################################################################

#best performance in terms of balanced accuracy is "random forest default model"
#best performance in terms of accuracy is "CART default model"

final_pred_rpart <- predict(rpart_mdl, test_set,type="class")
confusionMatrix(final_pred_rpart, test_set$DEFAULT)

final_pred_rf <-predict(rf_mdl, test_set)$predictions
confusionMatrix(final_pred_rf, test_set$DEFAULT)$byClass



final_results <- tibble( method ="CART default",
                         Accuracy =confusionMatrix(final_pred_rpart, test_set$DEFAULT)$overall[1],
                         Sensitivity =confusionMatrix(final_pred_rpart, test_set$DEFAULT)$byClass[1],
                         Specificity =confusionMatrix(final_pred_rpart, test_set$DEFAULT)$byClass[2],
                         Balanced_Accuracy = confusionMatrix(final_pred_rpart, test_set$DEFAULT)$byClass[11]
)

final_results <-  bind_rows( final_results, 
                             tibble( method ="Random forest default",
                             Accuracy =confusionMatrix(final_pred_rf, test_set$DEFAULT)$overall[1],#Accuracy
                             Sensitivity =confusionMatrix(final_pred_rf, test_set$DEFAULT)$byClass[1],#Sensitivity
                             Specificity =confusionMatrix(final_pred_rf, test_set$DEFAULT)$byClass[2],#Specificity
                             Balanced_Accuracy = confusionMatrix(final_pred_rf, test_set$DEFAULT)$byClass[11])#Balanced Accuracy
)
final_results %>% knitr::kable()
###
