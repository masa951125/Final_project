#regression all

glm_all_mdl <- glm(DEFAULT ~., 
                   data = train_set, 
                   family = binomial(link = "logit"))
glm_all_prob <- predict(glm_all_mdl, test_set,type="response")
glm_all_pred <- ifelse(glm_all_prob >0.5,1,0)

confusionMatrix(as.factor(glm_all_pred), test_set$DEFAULT)
glm_all_roc <- roc(test_set$DEFAULT,glm_all_prob)
plot(glm_all_roc, col="red")
glm_all_roc$auc

summary(glm_all_mdl)

#VIF
if(!require(car)) install.packages("car") #basic library
library(car)

dat <-vif(glm_all_mdl)
class(dat)
dat <-vif(step_mdl_b)
dat[dat[,1]>10]

cor(train_set)

#parallel


library(parallel)
i <- detectCores()
install.packages("doParallel")
if(!require(doParallel)) install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

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






##################
#cross validation
##################

#cross validation 5 times
control <- trainControl(method="cv", number = 5)

rf_mdl_cv <- train(
  formula = DEFAULT ~ ., 
  method="rf",
  trControl= control,
  tuneGrid = data.frame(mtry = 2),
  data = train_set)

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
