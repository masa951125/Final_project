

set.seed(2021, sample.kind = "Rounding")
index <- createDataPartition(original_default$default.payment.next.month, p=0.2, list=F, times=1)
test <- original_default[index,]
default <-original_default[-index,]

index_2 <-createDataPartition(default$default.payment.next.month, p=0.2, list=F, times=1)
validation <- default[index_2,]
train <- default[-index_2,]

################################################################################
#regression all

glm_all_mdl <- glm(DEFAULT ~., 
                   data = train_set, 
                   family = binomial(link = "logit"))
glm_all_prob <- predict(glm_all_mdl, validation_set,type="response")
glm_all_pred <- ifelse(glm_all_prob >0.5,1,0)

confusionMatrix(as.factor(glm_all_pred), validation_set$DEFAULT)
glm_all_roc <- roc(validation_set$DEFAULT,glm_all_prob)
plot(glm_all_roc, col="red")
glm_all_roc$auc


summary(glm_all_mdl)


#VIF variation inflation factor 
if(!require(car)) install.packages("car") #basic library
library(car)

#check vif

names(Filter(is.numeric, train_set))

vif_mdl <- glm(DEFAULT ~LIMIT_BAL+ AGE + 
                 BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
                 PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,  
                 data= train_set,family = binomial(link = "logit"))
dat <-vif(vif_mdl)
dat[dat>10]

#BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5 BILL_AMT6, vif >10 
#leave out these columns

glm_vif_mdl <- glm(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE +
                             PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                             PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                             data = train_set, 
                   family = binomial(link = "logit"))

glm_vif_prob <- predict(glm_vif_mdl, test_set,type="response")
glm_vif_pred <- ifelse(glm_vif_prob >0.5,1,0)

confusionMatrix(as.factor(glm_vif_pred), test_set$DEFAULT)
#Accuracy : 0.815
#Sensitivity : 0.9548
#Specificity : 0.3230
#Balanced Accuracy : 0.6389

glm_vif_roc <- roc(validation_set$DEFAULT,glm_vif_prob)
plot(glm_vif_roc, col="red")
glm_vif_roc$auc
#Area under the curve: 0.7622
################################################################################

#pick up features, SEX, EDUCATION, MARRIAGE, AGE, 
#PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6, BILL_AMT1, and PAY_AMT1
glm_mdl <- glm(DEFAULT ~ SEX + EDUCATION + MARRIAGE + AGE + 
                 PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                 BILL_AMT1 +PAY_AMT1, 
               data = train_set, 
               family = binomial(link = "logit"))

glm_prob <- predict(glm_mdl, validation_set,type="response")
glm_pred <- ifelse(glm_prob >0.5,1,0)

summary(glm_mdl)
confusionMatrix(as.factor(glm_pred), validation_set$DEFAULT)
#Accuracy : 0.8175
#Sensitivity : 0.9537         
#Specificity : 0.3380
#Balanced Accuracy : 0.6459

glm_roc <- roc(validation_set$DEFAULT,glm_prob)
plot(glm_roc, col="red")
glm_roc$auc
#Area under the curve: 0.7543

#make a table
results <- tibble(method = "glm fewer predictors", 
                  Accuracy =confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)$overall[1],#Accuracy
                  Sensitivity =confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)$byClass[1],#Sensitivity
                  Specificity =confusionMatrix(as.factor(glm_pred), test_set$DEFAULT)$byClass[2],#Specificity
                  AUC =as.numeric(glm_roc$auc))
results %>% knitr::kable()

#####################################################
#stepwise regression using both forward and backward
#####################################################

#stepwise regression explanation

#a null model with no predictors
null_model <- glm(DEFAULT~1, data = train_set, family = binomial(link = "logit"))

#q full model using all of the potential predictors
full_model <- glm(DEFAULT~., data = train_set, family = binomial(link = "logit"))

#forward and backward stepwise algorithm
step_mdl   <- step(null_model, 
                     scope = list(lower = null_model, upper = full_model), 
                     direction = "both")

step_prob <- predict(step_mdl_f, validation_set,type="response")
step_pred <- ifelse(step_prob_f >0.5,1,0)
confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)

#backward
step_mdl_b   <- step(full_model, 
                     scope = list(lower = null_model, upper = full_model), 
                     direction = "backward")

step_prob_b <- predict(step_mdl_b, validation_set,type="response")
step_pred_b <- ifelse(step_prob_b >0.5,1,0)

confusionMatrix(as.factor(step_pred_b), validation_set$DEFAULT)

summary(step_mdl_b)
summary(step_mdl_f)

#both forward and backward come to the same conclusion

#this model uses predictors such as 
#LIMIT_BAL, SEX, EDUCATION, MARRIAGE, 
#PAY_0, PAY_3, PAY_4, PAY_5, PAY_6, BILL_AMT3, BILL_AMT5
#PAY_AMT1, PAY_AMT2, PAY_AMT5,  PAY_AMT6

#there is a possibility of multicolliearity
#to find multicollinerity, we use "vif" function from "car"package
#https://www.rdocumentation.org/packages/regclass/versions/1.6/topics/VIF


if(!require(car)) install.packages("car") #VIF variation inflation factor 
#to check multicollinerity, we pick up numeric predictors
names(Filter(is.numeric, train_set))

#make numeric only model and check vif
vif_mdl <- glm(DEFAULT ~LIMIT_BAL+ AGE + 
                 BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
                 PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,  
               data= train_set, family = binomial(link = "logit"))
dat <-vif(vif_mdl)

#finding values which are exceeding 10 
dat[dat>10]

#BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5 BILL_AMT6, vif >10 
#leave out these columns

#make a model
glm_vif_mdl <- glm(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE +
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                     PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                   data = train_set, 
                   family = binomial(link = "logit"))

#predict
glm_vif_prob <- predict(glm_vif_mdl, validation_set,type="response")
glm_vif_pred <- ifelse(glm_vif_prob >0.5,1,0)

#to show accuracy we use confusionMatrix function in caret library
confusionMatrix(as.factor(glm_vif_pred), validation_set$DEFAULT) #accept only factor






################################################################################
#parallel
library(parallel)
i <- detectCores()
install.packages("doParallel")
if(!require(doParallel)) install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
################################################################################

###################
#find optimal mtry
###################


rf_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  data = train_set)

plot(rf_mdl)
rf_mdl$mtry
#[1] 4

set.seed(2021, sample.kind = "Rounding")
tuned_mtry<- tuneRF(train_set%>%select(-DEFAULT), train_set$DEFAULT,doBest=T)
tuned_mtry$mtry
#[1] 4

plot(tuned_mtry)
tuned_mtry$forest
summary(tuned_mtry)

rf_tuned_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  data = train_set,
  mtry=4)

#increase ntree=1000

rf_tuned_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  ntree=1000,
  data = train_set,
  mtry=4)

plot(rf_tuned_mdl)

rf_tuned_pred <- predict(rf_tuned_mdl, test_set)
rf_tuned_prob <- predict(rf_tuned_mdl, test_set, type = "prob")
confusionMatrix(as.factor(rf_tuned_pred), test_set$DEFAULT)
#Accuracy : 0.8195
#Sensitivity : 0.9482          
#Specificity : 0.3667
#Balanced Accuracy : 0.6575

rf_tuned_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(rf_tuned_prob[,2]))
plot(rf_tuned_roc, col="red")
rf_tuned_roc$auc
#Area under the curve:  0.774

ntree_tuned <- c(10, 20, 50, 100, 500, 1000)

acc_tree <- sapply(ntree_tuned, function(n){
  train(DEFAULT ~ ., 
        method = "rf", 
        data = train_set,
        tuneGrid = data.frame(mtry = 4),
        ntree =n)
})

  qplot(nodesize, acc)

##########
#rpart cv
##########

rpart_cv_mdl <-train_set %>%
    mutate_if(is.factor, funs(make.names)) %>% 
    train(
      DEFAULT ~ .,
      data = .,
      method = 'rpart',
      parms = list(split='information'),
      control = rpart.control(minsplit = 10, cp = 0.001),
      metric = 'ROC',
      trControl = 
        trainControl(method = 'repeatedcv', 
                     number = 10, 
                     repeats = 10, 
                     allowParallel = TRUE, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary))   
rpart_cv_mdl  
rpart_cv_mdl$bestTune    
rpart_cv_mdl$finalModel$variable.importance
rpart_cv_mdl$bestTune[,2]    
rpart_cv_mdl$finalModel
plot(rpart_cv_mdl)
rpart.plot(rpart_cv_mdl$finalModel)  

#validation
rpart_cv_prob <- predict(rpart_cv_mdl, validation_set%>%
                           mutate_if(is.factor, funs(make.names)), type="prob")
rpart_cv_pred <- predict(rpart_cv_mdl, validation_set%>%
                           mutate_if(is.factor, funs(make.names)))
rpart_cv_pred <- ifelse(rpart_cv_pred=="X0",0,1)

confusionMatrix(as.factor(rpart_cv_pred), validation_set$DEFAULT)
#Accuracy : 0.8152
#Sensitivity : 0.9596         
#Specificity : 0.3070 
# Balanced Accuracy : 0.6333

#AUC
rpart_cv_roc <- roc(as.numeric(validation_set$DEFAULT),as.numeric(rpart_cv_prob[,2]))
plot(rpart_tuned_roc, col="red")
rpart_cv_roc$auc
#Area under the curve: 0.7224

################################################################################

################################
#random forest cross validation
################################
#grid search

rf_cv_mdl <-train_set %>%
  mutate_if(is.factor, funs(make.names)) %>% 
  train(
    DEFAULT~ .,
    data = .,
    method = 'ranger',
    metric = 'ROC',
    num.trees = 1000,
    tuneGrid = expand.grid(mtry = 3:10, splitrule = 'gini', min.node.size = 1), 
    trControl = trainControl(method = 'cv', number = 5, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary))

plot(rf_cv_mdl)
rf_cv_mdl$bestTune

#validation
rf_cv_pred <- predict(rf_cv_mdl, validation_set%>%
                        mutate_if(is.factor, funs(make.names)))
rf_cv_prob <- predict(rf_cv_mdl, validation_set%>%
                        mutate_if(is.factor, funs(make.names)), type="prob")
rf_cv_pred <- ifelse(rf_cv_pred=="X0",0,1)

confusionMatrix(as.factor(rf_cv_pred), validation_set$DEFAULT)
#Accuracy : 0.8136  
#Sensitivity : 0.9601
#Specificity : 0.2976
#Balanced Accuracy : 0.6289

#AUC
rf_cv_roc <- roc(as.numeric(validation_set$DEFAULT),as.numeric(rf_cv_prob[,2]))
plot(rpart_tuned_roc, col="red")
rf_cv_roc$auc
#Area under the curve: 0.7768
################################################################################

#ensemble
ensemble <- 
  cbind(rf =rf_node_tuned_pred, rpart = new_rpart_cvmdl_pred,glm= as.factor(step_pred_b))
ensemble_preds <- ifelse(rowMeans(ensemble) > 1.5, 1, 0)
confusionMatrix(as.factor(ensemble_preds),test_set$DEFAULT)
#Accuracy : 0.8242 0.8247 
#Sensitivity : 0.9555 0.9585
#Specificity : 0.3622 0.3539 
#Balanced Accuracy : 0.6588 0.6562   

#AUC
ensemble_roc <-roc(as.numeric(test_set$DEFAULT), as.numeric(ensemble_preds))
ensemble_roc$auc
#Area under the curve: 0.6588 0.6562   


levels(train_set$PAY_0)

head(scale(train_set$LIMIT_BAL, scale = T))

rpart_tuned_mdl <- rpart(DEFAULT ~ .,
                         data = train_set,
                         method = 'class',
                         parms = list(split='information'),
                         control = rpart.control(cp=0.001))



###############################################################################
##################################
#random forest tuning mtry, ntree
##################################

#check default model's mtry and ntree
rf_mdl$mtry
#4
rf_mdl$ntree
#500

#mtry tuning using tuneRF default ntree=500
set.seed(2021, sample.kind = "Rounding")
tuned_mtry<- tuneRF(train_set%>%select(-DEFAULT), train_set$DEFAULT,doBest=T)
tuned_mtry$mtry
#[1] 8

set.seed(2021, sample.kind = "Rounding")
rf_tuned_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  data = train_set,
  mtry=8)

rf_tuned_pred <- predict(rf_tuned_mdl, validation_set)
rf_tuned_prob <- predict(rf_tuned_mdl, validation_set, type = "prob")

confusionMatrix(as.factor(rf_tuned_pred), validation_set$DEFAULT)

rf_tuned_roc <-roc(as.numeric(validation_set$DEFAULT), as.numeric(rf_tuned_prob[,2]))
plot(rf_tuned_roc, col="red")
rf_tuned_roc$auc

#plot ntrees
plot(rf_tuned_mdl)
#errors are almost flat ntrees value over 100
#then use ntree=100

set.seed(2021, sample.kind = "Rounding")
new_tuned_mtry<- tuneRF(train_set%>%select(-DEFAULT), train_set$DEFAULT,doBest=T,
                        ntree = 100)
new_tuned_mtry$mtry

#increase ntree=1000
set.seed(2021, sample.kind = "Rounding")
new_rf_tuned_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  ntree=100,
  data = train_set,
  mtry=2)

plot(rf_tuned_mdl)

new_rf_tuned_pred <- predict(new_rf_tuned_mdl, validation_set)
new_rf_tuned_prob <- predict(new_rf_tuned_mdl, validation_set, type = "prob")

confusionMatrix(as.factor(new_rf_tuned_pred), validation_set$DEFAULT)

new_rf_tuned_roc <-roc(as.numeric(validation_set$DEFAULT), as.numeric(new_rf_tuned_prob[,2]))
plot(rf_tuned_roc, col="red")
new_rf_tuned_roc$auc
#got worse

#increese ntree=1000
set.seed(2021, sample.kind = "Rounding")
new_tuned_mtry<- tuneRF(train_set%>%select(-DEFAULT), train_set$DEFAULT,doBest=T,
                        ntree = 1000)
new_tuned_mtry$mtry
#[1] 4

#increase ntree=1000
set.seed(2021, sample.kind = "Rounding")
new_rf_tuned_mdl <- randomForest(
  formula = DEFAULT ~ ., 
  ntree=1000,
  data = train_set,
  mtry=4)

plot(rf_tuned_mdl)

new_rf_tuned_pred <- predict(new_rf_tuned_mdl, validation_set)
new_rf_tuned_prob <- predict(new_rf_tuned_mdl, validation_set, type = "prob")

confusionMatrix(as.factor(new_rf_tuned_pred), validation_set$DEFAULT)

new_rf_tuned_roc <-roc(as.numeric(validation_set$DEFAULT), as.numeric(new_rf_tuned_prob[,2]))
plot(rf_tuned_roc, col="red")
new_rf_tuned_roc$auc

###################
#################################
#decision tree cross validataion
#################################

#rpart ~ cross validation using caret to find optimal cp value

set.seed(2021, sample.kind = "Rounding")
rpart_cv_mdl <- train(DEFAULT ~ ., 
                      method = "rpart", 
                      tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), 
                      data = train_set)

#find optimal cp
plot(rpart_cv_mdl)
cp_opt <- rpart_cv_mdl$bestTune
cp_opt

#draw decision tree
rpart.plot(rpart_cv_mdl$finalModel)
#note: numeric values are scaled

#predict
rpart_cv_mdl_prob <- predict(rpart_cv_mdl, validation_set, type="prob")
rpart_cv_mdl_pred <- ifelse(rpart_cv_mdl_prob[,1] >0.5,0,1)

confusionMatrix(as.factor(rpart_cv_mdl_pred), validation_set$DEFAULT)

#AUC
rpart_cv_mdl_roc <-roc(as.numeric(validation_set$DEFAULT), as.numeric(rpart_cv_mdl_prob[,2]))
plot(rpart_cv_mdl_roc, col="red")
rpart_cv_mdl_roc$auc

#pick up important predictors
rpart_cv_mdl %>% varImp()
#PAY_0 ~ PAY_6, PAY_AMT1,5,6

#make a table
results <- bind_rows(
  results,
  tibble(method="rpart cv",  
         Accuracy = confusionMatrix(as.factor(rpart_cv_mdl_pred), validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rpart_cv_mdl_pred), validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rpart_cv_mdl_pred), validation_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rpart_cv_mdl_roc$auc)))

results %>% knitr::kable()
####################
#find a model which produces the highest AUC 

set.seed(2021, sample.kind = "Rounding")
rpart_cv_mdl <-train_set %>%
  mutate_if(is.factor, funs(make.names)) %>% #need to change factors to characters
  train(
    DEFAULT ~ .,
    data = .,
    method = 'rpart',
    parms = list(split='gini'), #default gini
    control = rpart.control(minsplit = 10, cp = 0.001),
    metric = 'ROC', #show ROC value
    trControl = 
      trainControl(method = 'repeatedcv', 
                   number = 10, 
                   repeats = 10, 
                   allowParallel = TRUE, 
                   classProbs = TRUE, 
                   summaryFunction = twoClassSummary))   

rpart_cv_mdl$  
  rpart_cv_mdl$finalModel
plot(rpart_cv_mdl)
rpart.plot(rpart_cv_mdl$finalModel)  

#validation
rpart_cv_prob <- predict(rpart_cv_mdl, validation_set%>%
                           mutate_if(is.factor, funs(make.names)), type="prob")
rpart_cv_pred <- predict(rpart_cv_mdl, validation_set%>%
                           mutate_if(is.factor, funs(make.names)))
rpart_cv_pred <- ifelse(rpart_cv_pred=="X0",0,1) #need to change characters to factors

confusionMatrix(as.factor(rpart_cv_pred), validation_set$DEFAULT)
#Accuracy : 0.8163
#Sensitivity : 0.9593         
#Specificity : 0.3126
# Balanced Accuracy : 0.6360

#AUC
rpart_cv_roc <- roc(as.numeric(validation_set$DEFAULT),as.numeric(rpart_cv_prob[,2]))
plot(rpart_tuned_roc, col="red")
rpart_cv_roc$auc
#Area under the curve: 0.6799

#make a table
results <- bind_rows(
  results,
  tibble(method="rpart cv",  
         Accuracy = confusionMatrix(as.factor(rpart_cv_pred), validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rpart_cv_pred), validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rpart_cv_pred), validation_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rpart_cv_roc$auc)))

results %>% knitr::kable()

################################

#tuning random forest using nodesize, using caret package mtry=4, nttee=100
#https://rafalab.github.io/dsbook/examples-of-algorithms.html#random-forests
#caution! it takes a lot of time!

set.seed(2021, sample.kind = "Rounding")
nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(DEFAULT ~ ., 
        method = "rf", 
        data = train_set,
        ntrees =1000,
        tuneGrid = data.frame(mtry = 5),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

set.seed(2021, sample.kind = "Rounding")
rf_node_tuned_mdl <- randomForest(DEFAULT ~ ., 
                                  data= train_set,
                                  ntree=1000,
                                  nodesize = nodesize[which.max(acc)])

rf_node_tuned_pred <-predict(rf_node_tuned_mdl, validation_set)
rf_node_tuned_prob <-predict(rf_node_tuned_mdl, validation_set, type="prob")
confusionMatrix(as.factor(rf_node_tuned_pred),validation_set$DEFAULT) 

plot(rf_tuned_mdl)
summary(rf_tuned_mdl)

#AUC
rf_node_tuned_roc <-roc(as.numeric(validation_set$DEFAULT), as.numeric(rf_node_tuned_prob[,2]))
rf_node_tuned_roc$auc
plot(rf_node_tuned_roc, col="red")

#make a table
results <- bind_rows(
  results,
  tibble(method="random forest nodes_tuned",  
         Accuracy = confusionMatrix(as.factor(rf_node_tuned_pred),valdation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(rf_node_tuned_pred),validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(rf_node_tuned_pred),validation_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(rf_node_tuned_roc$auc)))

results %>% knitr::kable()


###############################
#random forest tuning nodesize
###############################


##############################
#random forest fewer features
##############################

#to improve further, what happens if we leave out features which are not important?
varImp(rf_tuned_mdl)

#from this, we will leave out 3 features, SEX, EDUCATION, MARRIAGE

set.seed(2021, sample.kind = "Rounding")
new_rf_node_tuned_mdl <- randomForest(DEFAULT ~ ., 
                                      data= train_set %>% select(-SEX, -EDUCATION, -MARRIAGE),
                                      ntree=1000,
                                      nodesize = nodesize[which.max(acc)])


new_rf_node_tuned_pred <-predict(new_rf_node_tuned_mdl, validation_set)
new_rf_node_tuned_prob <-predict(new_rf_node_tuned_mdl, validation_set, type="prob")

confusionMatrix(as.factor(new_rf_node_tuned_pred),validation_set$DEFAULT) 

#AUC
new_rf_node_tuned_roc <-roc(as.numeric(validation_set$DEFAULT), 
                            as.numeric(new_rf_node_tuned_prob[,2]))
new_rf_node_tuned_roc$auc
plot(new_rf_node_tuned_roc, col="red")


varImp(new_rf_tuned_mdl)

#make a table
results <- bind_rows(
  results,
  tibble(method="random forest nodes_tuned fewer predictors",  
         Accuracy = confusionMatrix(as.factor(new_rf_node_tuned_pred),validation_set$DEFAULT)$overall[1],#Accuracy
         Sensitivity = confusionMatrix(as.factor(new_rf_node_tuned_pred), validation_set$DEFAULT)$byClass[1],#Sensitivity
         Specificity = confusionMatrix(as.factor(new_rf_node_tuned_pred),validation_set$DEFAULT)$byClass[2],#Specificity
         AUC = as.numeric(new_rf_node_tuned_roc$auc)))

results %>% knitr::kable()

###############################################################################

t_rpart_pred <- predict(rpart_mdl, test_set, type="class")

confusionMatrix(t_rpart_pred, test_set$DEFAULT)$byClass[7]

class(rpart_pred)

t_rpart_tuned_pred <- predict(rpart_tuned_mdl, test_set)
confusionMatrix(t_rpart_tuned_pred, test_set$DEFAULT)$byClass[7]


