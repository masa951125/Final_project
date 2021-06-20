#package used in this code

if(!require(tidyverse)) install.packages("tidyverse") #basic library
library(tidyverse)

if(!require(gridExtra)) install.packages("gridExtra") #expansion of ggplot
library(gridExtra)

if(!require(caret)) install.packages("caret") #cross validation 
library(caret)

if(!require(rpart)) install.packages("rpart") #to make decision tree model
library(rpart)

#if(!require(rpart.plot)) install.packages("rpart.plot") #to show decision tree
#library(rpart.plot)

if(!require(pROC)) install.packages("pROC")#ROC value and graph
library(pROC)

#if(!require(PRROC)) install.packages("PRROC") #ROC graph
#library(PRROC)

if(!require(DataExplorer)) install.packages("DataExplorer") #to do data exploration
library(DataExplorer)

#if(!require(caTools)) install.packages("caTools")
#library(caTools)

if(!require(randomForest)) install.packages("randomForest") #random forest
library(randomForest)

#data
url <-"https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"
download.file(url, "original_default.csv")
original_default <- read_csv("original_default.csv")

###############################################################################
##################
#Data exploration
##################

#check dataset
str(original_default)
summary(original_default)
#no NAs

#correlation
plot_correlation(original_default)
# DEFAULT has relatively strong correlations in terms of PAY, and PAY_AMT 


#change name of "default.payment.next.month"
n <-which(names(original_default)=="default.payment.next.month")
names(original_default)[n] <- "DEFAULT"

###############################
#data exploration and cleansing
###############################

#outcomes ~ DEFAULT
#change it into factor

original_default$DEFAULT <- as.factor(original_default$DEFAULT)
ggplot(data=original_default, aes(DEFAULT)) +geom_bar()

mean(original_default$DEFAULT==1)
#[1] 0.2212

#1 LIMIT_BAL

summary(original_default$LIMIT_BAL)
ggplot(data=original_default, aes(LIMIT_BAL),fill=DEFAULT) +geom_histogram()
#distribution is skewed right

median(original_default$LIMIT_BAL)
mean(original_default$LIMIT_BAL)
sd(original_default$LIMIT_BAL)

#2 SEX
unique(original_default$SEX)

gender <- ifelse(original_default$SEX == 1, "male", "female")

original_default %>% ggplot(aes(x=gender, fill= DEFAULT)) +
  geom_bar() +
  ggtitle("SEX")

#stacked bar graph
original_default %>% ggplot(aes(x=gender, fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("SEX")
#There seemed to be little difference between genders.

#3 EDUCATION

unique(original_default$EDUCATION)
#1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown
#[1] 2 1 3 5 4 6 0
#O is not defined. 0,5 and 6 can be included into 4 

original_default$EDUCATION <- ifelse(original_default$EDUCATION== 0|
                                       original_default$EDUCATION == 5|
                                       original_default$EDUCATION == 6, 4,
                                     original_default$EDUCATION)

original_default %>% ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("EDUCATION")+
  stat_count(aes(label = ..count..), geom = "label")# illustrate numbers

#stacked bar graph
original_default %>% ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("EDUCATION")
  
# 4 is the smallest in terms of default rate. but its numbers are very small.

#4 marriage

unique(original_default$ MARRIAGE)
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

#5 AGE

summary(original_default$AGE)
ggplot(data=original_default, aes(AGE)) +geom_histogram()

#6 PAY

#PAY_0
summary(original_default$PAY_0)
unique(original_default$PAY_0)

original_default %>% ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_0")+
  stat_count(aes(label = ..count..), geom = "label")# illustrate numbers

#stack bar graph PAY_0
graph_P0 <-original_default %>% ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_0")

#PAY_2
original_default %>% ggplot(aes(x=as.factor(PAY_2), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_2")+
  stat_count(aes(label = ..count..), geom = "label")

graph_P2 <-original_default %>% ggplot(aes(x=as.factor(PAY_2), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_2")

#stack bar graph PAY_3
original_default %>% ggplot(aes(x=as.factor(PAY_3), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_3")+
  stat_count(aes(label = ..count..), geom = "label")

graph_P3 <-original_default %>% ggplot(aes(x=as.factor(PAY_3), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_3")

#stack bar graph PAY_4
original_default %>% ggplot(aes(x=as.factor(PAY_4), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_4")+
  stat_count(aes(label = ..count..), geom = "label")

graph_P4 <-original_default %>% ggplot(aes(x=as.factor(PAY_4), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_4")
 

#stack bar graph PAY_5
original_default %>% ggplot(aes(x=as.factor(PAY_5), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_5")+
  stat_count(aes(label = ..count..), geom = "label")

graph_P5 <-original_default %>% ggplot(aes(x=as.factor(PAY_5), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_5")

#stack bar graph PAY_6
original_default %>% ggplot(aes(x=as.factor(PAY_6), fill= DEFAULT)) +
  geom_bar() +
  ggtitle("PAY_6")+
  stat_count(aes(label = ..count..), geom = "label")

graph_P6 <-original_default %>% ggplot(aes(x=as.factor(PAY_6), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_6")

install.packages("gridExtra")
library(gridExtra)

grid.arrange(graph_P0, graph_P2, graph_P3, graph_P4, graph_P5, graph_P6, nrow=2, ncol=3)

#7 BILL_AMT
str(original_default$BILL_AMT1)
mean(original_default$BILL_AMT6)
sd(original_default$BILL_AMT6)

b1 <- ggplot(data=original_default, aes(BILL_AMT1)) +geom_histogram()
b2 <- ggplot(data=original_default, aes(BILL_AMT2)) +geom_histogram()
b3 <- ggplot(data=original_default, aes(BILL_AMT3)) +geom_histogram()
b4 <- ggplot(data=original_default, aes(BILL_AMT4)) +geom_histogram()
b5 <- ggplot(data=original_default, aes(BILL_AMT5)) +geom_histogram()
b6 <- ggplot(data=original_default, aes(BILL_AMT6)) +geom_histogram()

grid.arrange(b1,b2,b3,b4,b5,b6, nrow=2, ncol=3)
#they show almost similar distribution

#7 pay amount 

str(original_default$PAY_AMT1)
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

#categorical data
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

str(original_default)

names(original_default)

#scaling
cat_col <- c("SEX", "EDUCATION", "MARRIAGE",
             "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "DEFAULT")
all_col <- names(original_default)
num_col <- all_col[-which(all_col %in% cat_col)]

original_default[num_col] <-original_default %>% select(-all_of(cat_col)) %>% scale()
str(original_default)

#train_set, test_set
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(original_default$DEFAULT, p=0.2, list=F, times=1)
test_set <- original_default[test_index,]
train_set <- original_default[-test_index,]

################################################################################

####################
#baseline prediction
####################

#all predicted as non_default
base_pred <-factor(numeric(length(test_set$DEFAULT)),levels=c("0","1"))

#confusion matrix
confusionMatrix(base_pred, test_set$DEFAULT)
# Accuracy : 0.7787
# Sensitivity : 1.0000         
# Specificity : 0.0000   
# Balanced Accuracy : 0.5000


#####################
#logistic regression
#####################

#pick up features, SEX, EDUCATION, MARRIAGE, AGE, PAY_0~PAY_6, BILL_AMT1, and PAY_AMT1
glm_mdl <- glm(DEFAULT ~ SEX + EDUCATION + MARRIAGE + AGE + 
                 PAY_0 +BILL_AMT1 +PAY_AMT1, 
               data = train_set, 
               family = binomial(link = "logit"))


glm_prob <- predict(glm_mdl, test_set,type="response")
glm_pred <- ifelse(glm_prob >0.5,1,0)

summary(glm_mdl)
confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)
#Accuracy : 0.823
#Sensitivity : 0.9636         
#Specificity : 0.3283
#Balanced Accuracy : 0.6460

glm_roc <- roc(test_set$DEFAULT,glm_prob)
plot(glm_roc, col="red")
glm_roc$auc
#Area under the curve: 0.7483

#make a table
results <- tibble(method = "glm fewer predictors", 
                  Accuracy =confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)$overall[1],#Accuracy
                  Sensitivity =confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)$byClass[1],#Sensitivity
                  Specificity =confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)$byClass[2],#Specificity
                  AUC =as.numeric(glm_roc$auc))
results %>% knitr::kable()


#stepwise regression using both forward and backward

#a null model with no predictors
null_model <- glm(DEFAULT~1, data = train_set, family = "binomial")

#q full model using all of the potential predictors
full_model <- glm(DEFAULT~., data = train_set, family = "binomial")

#forward stepwise algorithm
step_mdl_f   <- step(null_model, 
                    scope = list(lower = null_model, upper = full_model), 
                    direction = "forward")

step_prob_f <- predict(step_mdl_f, test_set,type="response")
step_pred_f <- ifelse(step_prob_f >0.5,1,0)
confusionMatrix(as.factor(step_pred_f), test_set$DEFAULT)
#Accuracy : 0.825
#Sensitivity : 0.9574
#Specificity : 0.3592
#Balanced Accuracy : 0.6583

#ROC
step_roc_f <- roc(test_set$DEFAULT, step_prob_f)
plot(step_roc_f, col = "red")
step_roc_f$auc
#Area under the curve: 0.7729

#backward
step_mdl_b   <- step(full_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "backward")

step_prob_b <- predict(step_mdl_b, test_set,type="response")
step_pred_b <- ifelse(step_prob_b >0.5,1,0)
confusionMatrix(as.factor(step_pred_b), test_set$DEFAULT)
#Accuracy : 0.825
#Sensitivity : 0.9574
#Specificity : 0.3592
#Balanced Accuracy : 0.6583

#ROC
step_roc_b <- roc(test_set$DEFAULT,step_prob_b)
plot(step_roc_b, col = "red")
step_roc_b$auc
#Area under the curve: 0.7729

summary(step_mdl_b)
summary(step_mdl_f)

#both forward and backward come to the same conclusion

#this model uses predictors such as 
#LIMIT_BAL, SEX, EDUCATION, MARRIAGE, 
#PAY_0, PAY_3, PAY_4, PAY_5, PAY_6, BILL_AMT3, BILL_AMT5
#PAY_AMT1, PAY_AMT2, PAY_AMT5,  PAY_AMT6

#make a table
results <- bind_rows(
  results,
  tibble(method="glm stepwise",  
  Accuracy = confusionMatrix(as.factor(step_pred_b), test_set$DEFAULT)$overall[1],#Accuracy
  Sensitivity = confusionMatrix(as.factor(step_pred_b), test_set$DEFAULT)$byClass[1],#Sensitivity
  Specificity = confusionMatrix(as.factor(step_pred_b), test_set$DEFAULT)$byClass[2],#Specificity
  AUC = as.numeric(step_roc_b$auc)))

results %>% knitr::kable()



###############
#decision tree
###############

#rpart ~ using default minsplit=20, cp=0.01

set.seed(2021, sample.kind = "Rounding")

rpart_mdl <-rpart(formula = DEFAULT ~ .,data = train_set)
rpart_pred <- predict(rpart_mdl, test_set, type="class")
rpart_prob <- predict(rpart_mdl, test_set)

confusionMatrix(as.factor(rpart_pred), test_set$DEFAULT)
#Accuracy : 0.8229 
#Sensitivity : 0.9613  
#Specificity : 0.3358 
#Balanced Accuracy : 0.6486

#draw decision tree
plot(rpart_mdl,margin=0.1)
text(rpart_mdl, cex=1)

#AUC
rpart_roc <-roc(as.numeric(test_set$DEFAULT),as.numeric(rpart_prob[,2]))
plot(rpart_roc, col="red")
rpart_roc$auc
#Area under the curve: 0.6486

#find used features
rpart_mdl$variable.importance
#this model illustrates that PAY_0 is overwhelmingly important.


#make a table
results <- bind_rows(
  results,
  tibble(method="rpart default",  
         Accuracy = confusionMatrix(as.factor(rpart_pred), test_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rpart_pred), test_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rpart_pred), test_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rpart_roc$auc)))

results %>% knitr::kable()

#rpart ~ cross validation using caret

rpart_cv_mdl <- train(DEFAULT ~ ., 
                     method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), 
                     data = train_set)

#find optimal cp
plot(rpart_cv_mdl)
cp_opt <- rpart_cv_mdl$bestTune
cp_opt
#0.002083333

#calculate again
rpart_cv_mdl <- train(DEFAULT ~ ., 
                     method = "rpart", 
                     tuneGrid = cp_opt, 
                     data = train_set)

#draw decision tree
plot(rpart_cv_mdl$finalModel,margin=0.1)
text(rpart_cv_mdl$finalModel,cex=0.5)

rpart_cv_mdl_prob <- predict(rpart_cv_mdl, test_set, type="prob")
rpart_cv_mdl_pred <- ifelse(rpart_cv_mdl_prob[,1] >0.5,0,1)
confusionMatrix(as.factor(rpart_cv_mdl_pred), test_set$DEFAULT)
# Accuracy : 0.8194
# Sensitivity : 0.9628         
# Specificity : 0.3148  
# Balanced Accuracy : 0.6388

#AUC
rpart_cv_mdl_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(rpart_cv_mdl_prob[,2]))
plot(rpart_cv_mdl_roc, col="red")
rpart_cv_mdl_roc$auc
#Area under the curve: 0.7261

#pick up important predictors
rpart_cvmdl %>% 
  varImp() 
#PAY_0 ~ PAY_6, PAY_AMT1,2,3,5, BILL_AMT1,3

#make a table
results <- bind_rows(
  results,
  tibble(method="rpart cv",  
         Accuracy = confusionMatrix(as.factor(rpart_cv_mdl_pred), test_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rpart_cv_mdl_pred), test_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rpart_cv_mdl_pred), test_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rpart_cv_mdl_roc$auc)))

results %>% knitr::kable()

#rpart ~tuning using smaller cp and split ="information"

rpart_tuned_mdl <- rpart(DEFAULT ~ .,
                         data = train_set,
                         method = 'class',
                         parms = list(split='information'),
                         control = rpart.control(cp=0.001))

#draw decision tree
plot(rpart_tuned_mdl,margin=0.1)
text(rpart_tuned_mdl, cex=0.75)

rpart_tuned_pred <- predict(rpart_tuned_mdl, test_set, type="class")
rpart_tuned_prob <- predict(rpart_tuned_mdl, test_set)

confusionMatrix(as.factor(rpart_tuned_pred), test_set$DEFAULT)
#Accuracy : 0.8239 
#Sensitivity : 0.9542 
#Specificity : 0.3652  
#Balanced Accuracy : 0.6597

#AUC
rpart_tuned_roc <- roc(as.numeric(test_set$DEFAULT),as.numeric(rpart_tuned_prob[,2]))
plot(rpart_tuned_roc, col="red")
rpart_tuned_roc$auc
#Area under the curve: 0.7015

#important features
rpart_tuned_mdl %>% 
  varImp() 

#make a table
results <- bind_rows(
  results,
  tibble(method="rpart smaller cp",  
         Accuracy = confusionMatrix(as.factor(rpart_tuned_pred), test_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rpart_tuned_pred), test_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rpart_tuned_pred), test_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rpart_tuned_roc$auc)))

results %>% knitr::kable()

##############
#random forest
##############

set.seed(2021, sample.kind = "Rounding")

rf_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  data = train_set)

summary(rf_mdl)

rf_pred <- predict(rf_mdl, test_set)
rf_prob <- predict(rf_mdl, test_set, type = "prob")
confusionMatrix(as.factor(rf_pred), test_set$DEFAULT)
# Accuracy : 0.8195
# Sensitivity : 0.9489           
# Specificity : 0.3645   
# Balanced Accuracy : 0.6567

plot(rf_mdl)
rf_mdl$mtry
#4
rf_mdl$ntree
#500

#AUC
rf_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(rf_prob[,2]))
plot(rf_roc, col="red")
rf_roc$auc
#Area under the curve: 0.7725

rf_mdl %>% 
  varImp() 

#make a table

results <- bind_rows(
  results,
  tibble(method="random forest default",  
         Accuracy = confusionMatrix(as.factor(rf_pred), test_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rf_pred), test_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rf_pred), test_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rf_roc$auc)))

results %>% knitr::kable()

#tuning random forest using nodesize, using caret package
#caution! it takes a lot of time!

nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(DEFAULT ~ ., 
        method = "rf", 
        data = train_set,
        tuneGrid = data.frame(mtry = 4),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

rf_node_tuned_mdl <- randomForest(DEFAULT ~ ., 
                             data= train_set,
                             ntree=1000,
                             nodesize = nodesize[which.max(acc)])

rf_node_tuned_pred <-predict(rf_node_tuned_mdl, test_set)
rf_node_tuned_prob <-predict(rf_node_tuned_mdl, test_set, type="prob")
confusionMatrix(as.factor(rf_node_tuned_pred),test_set$DEFAULT) 
#Accuracy : 0.8227 
#Sensitivity : 0.9527        
#Specificity : 0.3652
#Balanced Accuracy : 0.6590

rf_tuned_mdl$ntree
  
plot(rf_tuned_mdl)
summary(rf_tuned_mdl)

#AUC
rf_node_tuned_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(rf_node_tuned_prob[,2]))
rf_node_tuned_roc$auc
plot(rf_node_tuned_roc, col="red")
#Area under the curve: 0.7805

#make a table
results <- bind_rows(
  results,
  tibble(method="random forest nodes_tuned",  
         Accuracy = confusionMatrix(as.factor(rf_node_tuned_pred),test_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rf_node_tuned_pred),test_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rf_node_tuned_pred),test_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rf_node_tuned_roc$auc)))

results %>% knitr::kable()

#to improve further, what happens if we leave out features which are not important?
varImp(rf_node_tuned_mdl)

#from this, we will leave out 3 features, SEX, EDUCATION, MARRIAGE
new_rf_node_tuned_mdl <- randomForest(DEFAULT ~ ., 
                             data= train_set %>% select(-SEX, -EDUCATION, -MARRIAGE),
                             ntree=1000,
                             nodesize = nodesize[which.max(acc)])


new_rf_node_tuned_pred <-predict(new_rf_node_tuned_mdl, test_set)
new_rf_node_tuned_prob <-predict(new_rf_node_tuned_mdl, test_set, type="prob")

confusionMatrix(as.factor(new_rf_node_tuned_pred),test_set$DEFAULT) 
#Accuracy : 0.8234   
#Sensitivity : 0.9508       
#Specificity : 0.3750      
#Balanced Accuracy : 0.6629        

#AUC
new_rf_node_tuned_roc <-roc(as.numeric(test_set$DEFAULT), 
                       as.numeric(new_rf_node_tuned_prob[,2]))
new_rf_node_tuned_roc$auc
plot(new_rf_node_tuned_roc, col="red")
#Area under the curve: 0.777 

varImp(new_rf_tuned_mdl)

#make a table
results <- bind_rows(
  results,
  tibble(method="random forest nodes_tuned fewer predictors",  
         Accuracy = confusionMatrix(as.factor(new_rf_node_tuned_pred),test_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(new_rf_node_tuned_pred),test_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(new_rf_node_tuned_pred),test_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(new_rf_node_tuned_roc$auc)))

results %>% knitr::kable()

###############################################################################
