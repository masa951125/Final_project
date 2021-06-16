#package used in this code

if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if(!require(caret)) install.packages("caret")
library(caret)

if(!require(rpart)) install.packages("rpart")
library(rpart)

if(!require(rpart.plot)) install.packages("rpart.plot")
library(rpart.plot)

if(!require(pROC)) install.packages("pROC")
library(pROC)

if(!require(PRROC)) install.packages("PRROC")
library(PRROC)

if(!require(caTools)) install.packages("caTools")
library(caTools)

if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

#data
url <-"https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"
download.file(url, "original_default.csv")
original_default <- read_csv("original_default.csv")


##################
#Data exploration
##################

#check dataset
str(original_default)
summary(original_default)
#no NAs

#remove ID
original_default <- original_default %>% select(-ID)

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
  ggtitle("EDUCATION")

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
  ggtitle("MARRIAGE")

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
  stat_count(aes(label = ..count..), geom = "label")

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
b1 <- ggplot(data=original_default, aes(BILL_AMT1)) +geom_histogram()
b2 <- ggplot(data=original_default, aes(BILL_AMT2)) +geom_histogram()
b3 <- ggplot(data=original_default, aes(BILL_AMT3)) +geom_histogram()
b4 <- ggplot(data=original_default, aes(BILL_AMT4)) +geom_histogram()
b5 <- ggplot(data=original_default, aes(BILL_AMT5)) +geom_histogram()
b6 <- ggplot(data=original_default, aes(BILL_AMT6)) +geom_histogram()

grid.arrange(b1,b2,b3,b4,b5,b6, nrow=2, ncol=3)

#7 pay amount 

str(original_default$PAY_AMT1)
p1 <- ggplot(data=original_default, aes(PAY_AMT1)) +geom_histogram()
p2 <- ggplot(data=original_default, aes(PAY_AMT2)) +geom_histogram()
p3 <- ggplot(data=original_default, aes(PAY_AMT3)) +geom_histogram()
p4 <- ggplot(data=original_default, aes(PAY_AMT4)) +geom_histogram()
p5 <- ggplot(data=original_default, aes(PAY_AMT5)) +geom_histogram()
p6 <- ggplot(data=original_default, aes(PAY_AMT6)) +geom_histogram()

grid.arrange(p1,p2,p3,p4,p5,p6, nrow=2, ncol=3)

#correlation
plot_correlation(original_default)
# DEFAULT has relatively strong correlations in terms of PAY, and PAY_AMT 

##################
#data preparation
##################
#scaling
original_default[,1:23] <- scale(original_default[,1:23])

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

base_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(base_pred))
plot(base_roc, lty = 1, legacy.axes = TRUE)
#AUC 0.5

####################
#logistic regression
####################

#logistic regression
glm_mdl <- glm(DEFAULT ~., 
               data = train_set, 
               family = binomial(link = "logit"))


glm_prob <- predict(glm_mdl, test_set,type="response")
ggplot(data=data.frame(glm_prob),aes(glm_prob))+ geom_histogram(bins = 50)

glm_pred <- ifelse(glm_prob >0.5,1,0)
class(glm_pred)
class(test_set$DEFAULT)
confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)
# Accuracy : 0.8122  
# Sensitivity : 0.9741 
# Specificity : 0.2425 
# Balanced Accuracy : 0.6083 

glm_roc <- roc(as.numeric(test_set$DEFAULT),glm_pred)
plot(glm_roc)
glm_roc$auc
#Area under the curve: 0.6083

###############
#decision tree
###############

#rpart ~ using default minsplit=20, cp=0.01

set.seed(2021, sample.kind = "Rounding")

rpart_mdl <-rpart(formula = DEFAULT ~ .,data = train_set)
rpart_pred <- predict(rpart_mdl, test_set, type="class")

confusionMatrix(as.factor(rpart_pred), test_set$DEFAULT)
#Accuracy : 0.8229 
#Sensitivity : 0.9613  
#Specificity : 0.3358 
#Balanced Accuracy : 0.6486

#draw decision tree
plot(rpart_mdl,margin=0.1)
text(rpart_mdl, cex=1)

#AUC
rpart_roc <-roc(as.numeric(test_set$DEFAULT),as.numeric(rpart_pred))
plot(rpart_roc)
rpart_roc$auc
#Area under the curve: 0.6486

#find used features
rpart_mdl$variable.importance
#this model illustrates that PAY_0 is overwhelmingly important.

#rpart ~tuning using smaller cp and split ="information"

rpart_tuned_mdl <- rpart(DEFAULT ~ .,
                         data = train_set,
                         method = 'class',
                         parms = list(split='information'),
                         control = rpart.control(cp=0.001))

#draw decision tree
plot(rpart_tuned_mdl,margin=0.1)
text(rpart_tuned_mdl, cex=0.75)

printcp(rpart_tuned_mdl)
plotcp(rpart_tuned_mdl)

rpart_tuned_pred <- predict(rpart_tuned_mdl, test_set, type="class")

confusionMatrix(as.factor(rpart_tuned_pred), test_set$DEFAULT)
#Accuracy : 0.8192 
#Sensitivity : 0.9437 
#Specificity : 0.3810 
#Balanced Accuracy : 0.6624

#AUC
rpart_tuned_roc <- roc(as.numeric(test_set$DEFAULT),as.numeric(rpart_tuned_pred))
plot(rpart_tuned_roc)
rpart_tuned_roc$auc
#Area under the curve: 0.6624

#important features
rpart_tuned_mdl %>% 
  varImp() 

#rpart ~ cross validation using caret
rpart_cvmdl <- train(DEFAULT ~ ., 
                     method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), 
                     data = train_set)

#find optimal cp
plot(rpart_cvmdl)
cp_opt <- rpart_cvmdl$bestTune

#calculate again
new_rpart_cvmdl <- train(DEFAULT ~ ., 
                     method = "rpart", 
                     tuneGrid = cp_opt, 
                     data = train_set)

plot(new_rpart_cvmdl$finalModel,margin=0.1)
text(new_rpart_cvmdl$finalModel,cex=1)

new_rpart_cvmdl_prob <- predict(new_rpart_cvmdl, test_set, type="prob")
new_rpart_cvmdl_pred <- ifelse(new_rpart_cvmdl_prob[,1] >0.5,0,1)
confusionMatrix(as.factor(new_rpart_cvmdl_pred), test_set$DEFAULT)
# Accuracy : 0.8215
# Sensitivity : 0.9636          
# Specificity : 0.3215 
# Balanced Accuracy : 0.6426

#AUC
new_rpart_cvmdl_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(new_rpart_cvmdl_pred))
plot(new_rpart_cvmdl_roc)
new_rpart_cvmdl_roc$auc
#Area under the curve: 0.6426

rpart_cvmdl %>% 
  varImp() 

##############
#random forest
##############

set.seed(2021, sample.kind = "Rounding")

#default ntree=500, mtry=sqrt(23)

rf_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  data = train_set)

summary(rf_mdl)

rf_pred <- predict(rf_mdl, test_set)
confusionMatrix(as.factor(rf_pred), test_set$DEFAULT)
# Accuracy : 0.8187
# Sensitivity : 0.9495          
# Specificity : 0.3584
# Balanced Accuracy : 0.6540

plot(rf_mdl)

#AUC
rf_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(rf_pred))
rf_roc$auc
#Area under the curve: 0.654

rf_mdl %>% 
  varImp() 
  
#tuning random forest using nodesize, also using caret package
#caution! it takes a lot of time!

nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(DEFAULT ~ ., 
        method = "rf", 
        data = train_set,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

rf_tuned_mdl <- randomForest(DEFAULT ~ ., 
                             data= train_set,
                             ntree=1000,
                             nodesize = nodesize[which.max(acc)])

rf_tuned_pred <-predict(rf_tuned_mdl, test_set)
confusionMatrix(as.factor(rf_tuned_pred),test_set$DEFAULT) 
#Accuracy : 0.824
#Sensitivity : 0.9531          
#Specificity : 0.3697
#Balanced Accuracy : 0.6614

rf_tuned_mdl$ntree
  
plot(rf_tuned_mdl)
summary(rf_tuned_mdl)

#AUC
rf_tuned_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(rf_tuned_pred))
rf_tuned_roc$auc
#Area under the curve: 0.6614

#to improve further, what happens if we leave out features which are not important?
varImp(rf_tuned_mdl)

#from this, we will leave out 3 features, SEX, EDUCATION, MARRIAGE

nodesize <- seq(1, 51, 10)
new_acc <- sapply(nodesize, function(ns){
  train(DEFAULT ~ ., 
        method = "rf", 
        data = train_set %>% select(-SEX, -EDUCATION, -MARRIAGE),
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})

new_rf_tuned_mdl <- randomForest(DEFAULT ~ ., 
                             data= train_set %>% select(-SEX, -EDUCATION, -MARRIAGE),
                             ntree=1000,
                             nodesize = nodesize[which.max(acc)])


new_rf_tuned_pred <-predict(new_rf_tuned_mdl, test_set)
confusionMatrix(as.factor(new_rf_tuned_pred),test_set$DEFAULT) 
#Accuracy : 0.824
#Sensitivity : 0.9527
#Specificity : 0.3712  
#Balanced Accuracy : 0.6620   

#AUC
new_rf_tuned_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(new_rf_tuned_pred))
new_rf_tuned_roc$auc
#Area under the curve: 0.662

varImp(new_rf_tuned_mdl)

#ensemble
ensemble <- 
  cbind(rf =new_rf_tuned_pred,rpart = rpart_tuned_pred,glm= as.factor(glm_pred))
ensemble_preds <- ifelse(rowMeans(ensemble) > 1.5, 1, 0)
confusionMatrix(as.factor(ensemble_preds),test_set$DEFAULT)
#Accuracy : 0.8242
#Sensitivity : 0.9555
#Specificity : 0.3622  
#Balanced Accuracy : 0.6588

#AUC
ensemble_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(ensemble_preds))
ensemble_roc$auc
#Area under the curve: 0.6588
###############################################################################
