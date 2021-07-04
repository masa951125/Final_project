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
#read_csv in tidyverse package to download the file. 
#it creates a tibble dataset whose values are numerical.

################################################################################
#data exploration
################################################################################

#check dataset
str(original_default)

#number of NAs
sum(is.na(original_default))

#number of Nulls
sum(is.null(original_default))

#draw correlation matrix graph
plot_correlation(original_default)
#plot_correlation in DataExplorer package. 

###########
#1 outcome
###########

#calculate the proportion of default and non-default
prop.table(table(original_default$default.payment.next.month))

#change name of "default.payment.next.month"
n <-which(names(original_default)=="default.payment.next.month")
names(original_default)[n] <- "DEFAULT"

#change outcome into factor
original_default$DEFAULT <- as.factor(original_default$DEFAULT)

#############
#2 LIMIT_BAL
#############

#show summary of the column
summary(original_default$LIMIT_BAL)

#draw histogram filling the bar with proportion of defaults
ggplot(data=original_default, aes(LIMIT_BAL, fill=DEFAULT), ) +
  geom_histogram(bins=50)

#######
#2 SEX
#######

#proportion of genders
prop.table(table(original_default$SEX))

#draw distribution and proportion side by side
sex_ditribution <- original_default %>% 
  ggplot(aes(x=as.factor(SEX), fill= DEFAULT)) + #factoring to make graph clearer
  geom_bar() +
  ggtitle("SEX Distribution")

#stacked bar graph
sex_stackedbar <-original_default %>% 
  ggplot(aes(x=as.factor(SEX), fill= DEFAULT)) +
  geom_bar(position="fill") + #argument position is used to make stacked bar graph
  ggtitle("SEX Proportion") 

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(sex_ditribution, sex_stackedbar, nrow=1, ncol=2)


#############
#3 EDUCATION
#############

#find unique values
unique(original_default$EDUCATION)


#draw distribution and proportion side by side
EDU_distribution <-original_default %>% 
  ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) + #factoring to make graph clearer
  geom_bar() +
  ggtitle("EDUCATION Distribution")

EDU_stackedbar <-original_default %>% 
  ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) +
  geom_bar(position="fill") + #argument position is used to make stacked bar graph
  ggtitle("EDUCATION Proportion")

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(EDU_distribution, EDU_stackedbar, nrow=1, ncol=2)

############
#4 marriage
############

#find unique values
unique(original_default$ MARRIAGE)

#find number of 0 value
sum(original_default$MARRIAGE==0)

#draw distribution and proportion side by side
MAR_distribution <- original_default %>% 
  ggplot(aes(x=as.factor(MARRIAGE), fill= DEFAULT))+ #factoring to make graph clearer
  geom_bar() +
  ggtitle("MARRIAGE Distribution")

MAR_proportion <-original_default %>% 
  ggplot(aes(x=as.factor(MARRIAGE), fill= DEFAULT)) +
  geom_bar(position="fill") + #argument position is used to make stacked bar graph
  ggtitle("MARRIAGE Proportion")

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(MAR_distribution, MAR_proportion, nrow=1, ncol=2)

#######
#5 AGE
#######

#show summary
summary(original_default$AGE)

#draw distribution and proportion side by side
AGE_distribution <- original_default %>%
  ggplot(aes(AGE, fill=DEFAULT)) +
  geom_histogram(bins=30)+
  ggtitle("AGE Distribution")

AGE_proportion <- original_default %>% 
  ggplot(aes(x=AGE, fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("AGE Proportion")

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(AGE_distribution, AGE_proportion, nrow=2, ncol=1)

#######
#6 PAY
#######

#PAY_0 unique values
unique(original_default$PAY_0)

#draw distribution and proportion side by side
PAY_distribution <-original_default %>% 
  ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) + #factoring to make graph clearer
  geom_bar() +
  ggtitle("PAY_0 Distribution")

PAY_stackedbar <- original_default %>% 
  ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_0 Proportion")

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(PAY_distribution, PAY_stackedbar, nrow=2, ncol=1)

#draw all PAY graphs 
graph_p0 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_0))) +
  geom_bar() +
  ggtitle("PAY_0")

graph_p2 <-  
  original_default %>% ggplot(aes(x=as.factor(PAY_2))) +
  geom_bar() +
  ggtitle("PAY_2")

graph_p3 <-  
  original_default %>% ggplot(aes(x=as.factor(PAY_3))) +
  geom_bar() +
  ggtitle("PAY_3")

graph_p4 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_4))) +
  geom_bar() +
  ggtitle("PAY_4")

graph_p5 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_5))) +
  geom_bar() +
  ggtitle("PAY_5")

graph_p6 <-
  original_default %>% ggplot(aes(x=as.factor(PAY_6))) +
  geom_bar() +
  ggtitle("PAY_6")

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(graph_p0, graph_p2, graph_p3, graph_p4, graph_p5, graph_p6, nrow=2, ncol=3)

############
#7 BILL_AMT
############

#show summary
summary(original_default$BILL_AMT1)

#plot BILL_AMT1 filling with default rates
ggplot(data=original_default, aes(BILL_AMT1,fill= DEFAULT)) +
  geom_histogram(bins=30)

#Plot BILL_AMT1
ggplot(data=original_default, aes(BILL_AMT1,fill= DEFAULT)) +geom_histogram()

#draw all BILL_AMT graphs
b1 <- ggplot(data=original_default, aes(BILL_AMT1)) +geom_histogram(bins=30)

b2 <- ggplot(data=original_default, aes(BILL_AMT2)) +geom_histogram(bins=30)

b3 <- ggplot(data=original_default, aes(BILL_AMT3)) +geom_histogram(bins=30)

b4 <- ggplot(data=original_default, aes(BILL_AMT4)) +geom_histogram(bins=30)

b5 <- ggplot(data=original_default, aes(BILL_AMT5)) +geom_histogram(bins=30)

b6 <- ggplot(data=original_default, aes(BILL_AMT6)) +geom_histogram(bins=30)

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(b1,b2,b3,b4,b5,b6, nrow=2, ncol=3)

##############
#7 pay amount 
##############

#show summary
summary(original_default$PAY_AMT1)

#plot PAY_AMT1
ggplot(data=original_default, aes(PAY_AMT1,fill= DEFAULT)) +
  geom_histogram(bins=30)

#draw all PAY_AMT graphs
p1 <- ggplot(data=original_default, aes(PAY_AMT1)) +geom_histogram(bins=30)

p2 <- ggplot(data=original_default, aes(PAY_AMT2)) +geom_histogram(bins=30)

p3 <- ggplot(data=original_default, aes(PAY_AMT3)) +geom_histogram(bins=30)

p4 <- ggplot(data=original_default, aes(PAY_AMT4)) +geom_histogram(bins=30)

p5 <- ggplot(data=original_default, aes(PAY_AMT5)) +geom_histogram(bins=30)

p6 <- ggplot(data=original_default, aes(PAY_AMT6)) +geom_histogram(bins=30)

#grid.arrange in gridExtra package to draw two graphs side by side
grid.arrange(p1,p2,p3,p4,p5,p6, nrow=2, ncol=3)

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

#pick up categorical data columns. 
cat_col <- c("SEX", "EDUCATION", "MARRIAGE",
             "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "DEFAULT")

#make all columns' name vector
all_col <- names(original_default)

#extract numerical data columns leaving out categorical columns
num_col <- all_col[-which(all_col %in% cat_col)]

#scaling numerical data using function scale
original_default[num_col] <-original_default %>% select(-all_of(cat_col)) %>% scale()
str(original_default)
summary(original_default)

##################################################
#spliting into train_set,validation_set, test_set
##################################################

#set seed
set.seed(2021, sample.kind = "Rounding")

#use createDataPartition function in caret package
index_1 <- createDataPartition(original_default$DEFAULT, p=0.2, list=F, times=1)

#split into 20% and 80% of the dataset  
test_set <- original_default[index_1,]
pre_train_set <- original_default[-index_1,]

#split again
set.seed(2021, sample.kind = "Rounding")

index_2 <- createDataPartition(pre_train_set$DEFAULT, p=0.2, list=F, times=1)

#split into 20% and 80% of the dataset
validation_set <-pre_train_set[index_2,]
train_set <- pre_train_set[-index_2,]

#check default ratio and make a table
tibble(outcome =c(0,1),
       "train_set"= prop.table(table(train_set$DEFAULT)),
       "validation_set" = prop.table(table(validation_set$DEFAULT)),
       "test_set" = prop.table(table(test_set$DEFAULT)))

################################################################################
#model analysis
################################################################################

####################
#baseline prediction
####################

#all predicted as non_default make factor vectors 
base_pred <-factor(numeric(length(test_set$DEFAULT)),levels=c("0","1"))
#function numeric(n) produces n 0 vectors.

#confusion matrix using confusionMatrix function in caret package
confusionMatrix(base_pred, test_set$DEFAULT)$table

#showing statistical metrics
confusionMatrix(base_pred, test_set$DEFAULT)$overall[1] #accuracy
confusionMatrix(base_pred, test_set$DEFAULT)$byClass[1] #sensitivity
confusionMatrix(base_pred, test_set$DEFAULT)$byClass[2] #specificity
confusionMatrix(base_pred, test_set$DEFAULT)$byClass[11] #balanced accuracy

#####################
#logistic regression 
#####################

#stepwise regression
#a null model with no predictors
null_model <- glm(DEFAULT~1, data = train_set, family = binomial(link = "logit"))

#q full model using all of the potential predictors
full_model <- glm(DEFAULT~., data = train_set, family = binomial(link = "logit"))

#stepwise logistic regression 
step_mdl   <- step(null_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "both")
#function step conducts stepwise regression

#show summary of the model
summary(step_mdl)

#fit the model
step_prob <- predict(step_mdl, validation_set,type="response")
step_pred <- ifelse(step_prob >0.5,1,0) #this is predicted outcomes

#confusion matrix
confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
glm_s_results <- tibble(method = "glm step wise", 
                        Accuracy =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$overall[1],
                        Sensitivity =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[1],
                        Specificity =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[2],
                        Balanced_Accuracy =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[11])

glm_s_results %>% knitr::kable() #show in a table

#fewer variable logistic regression model
glm_fewer_mdl <- glm(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                               PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + PAY_AMT1,
                     data= train_set, family= binomial(link = "logit"))

#show summary of the model
summary(glm_fewer_mdl)

#fit the model
glm_fewer_prob <- predict(glm_fewer_mdl, validation_set,type="response")
glm_fewer_pred <- ifelse(glm_fewer_prob >0.5,1,0)

#confusion matrix
confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
glm_f_results <- tibble(method = "glm fewer features",
                        Accuracy =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$overall[1],
                        Sensitivity =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$byClass[1],
                        Specificity =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$byClass[2],
                        Balanced_Accuracy =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$byClass[11])

glm_f_results %>% knitr::kable() #show in a table

#######################
#decision tree default
#######################

#rpart function in rpart package making decision tree model
set.seed(2021, sample.kind = "Rounding")
rpart_mdl <-rpart(DEFAULT ~ .,data = train_set)
rpart_mdl

#draw decision tree using rpart.plot function in rpart.plot package
rpart.plot(rpart_mdl)

#fit the model
rpart_pred <- predict(rpart_mdl, validation_set, type="class")

#confusion Matrix
confusionMatrix(rpart_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rpart_results <- tibble(method = "rpart",
                        Accuracy =confusionMatrix(rpart_pred, validation_set$DEFAULT)$overall[1],
                        Sensitivity =confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[1],
                        Specificity =confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[2],
                        Balanced_Accuracy =confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[11])

rpart_results %>% knitr::kable() #show in a table

#find parameters using modelLookup function in caret package
modelLookup("rpart")

#show used cp in the model
rpart_mdl$control$cp


#find used features
rpart_mdl$variable.importance
#this model illustrates that PAY_0 is overwhelmingly important.

##############################
#decision tree further tuning
##############################

#we use train function in caret package. and tune cp
set.seed(2021, sample.kind = "Rounding")
rpart_tuned_mdl <- train(DEFAULT ~ ., 
                      method = "rpart", 
                      tuneGrid = data.frame(cp = seq(0, 0.01, len = 25)),
                      control = rpart.control(minsplit = 0),
                      data = train_set)
rpart_tuned_mdl

#plot cp and accuracy
plot(rpart_tuned_mdl)


#draw a decision tree
rpart.plot(rpart_tuned_mdl$finalModel)
#note: numeric values are scaled

#fit the model
rpart_tuned_pred <- predict(rpart_tuned_mdl, validation_set)

#confusion matrix
confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rpart_tuned_results <- tibble(method = "rpart tuned",
                              Accuracy =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$overall[1],
                              Sensitivity =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[1],
                              Specificity =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[2],
                              Balanced_Accuracy =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[11])

rpart_tuned_results %>% knitr::kable() #show in a table

#comparing two decision tree models
#compare the two decision tree models performance
bind_rows(rpart_results, rpart_tuned_results) %>% knitr::kable()

#######################
#random forest default
#######################

#to make random forest model using ranger function in ranger package
set.seed(2021, sample.kind = "Rounding")
rf_mdl <- ranger(
  formula = DEFAULT ~ ., 
  data = train_set,
  importance = "impurity",#use gini index
  probability = F)
rf_mdl

#plot the variable's importance
#"rf_mdl$variable.importance" shows importance of the variables 
imp_df <- data.frame(Variable = names(rf_mdl$variable.importance),
                     Importance = as.numeric(rf_mdl$variable.importance)) 

ggplot(imp_df, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity") +coord_flip() #flip x axis and y axis 

#fit the model
rf_pred <- predict(rf_mdl, validation_set)$predictions

#confusion matrix
confusionMatrix(rf_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rf_results <- tibble(method = "random forest",
                     Accuracy =confusionMatrix(rf_pred, validation_set$DEFAULT)$overall[1],
                     Sensitivity =confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[1],
                     Specificity =confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[2],
                     Balanced_Accuracy =confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[11])

rf_results %>% knitr::kable() #show in a table

################################
#random forest cross validation
################################

#find parameters which can be tuned
modelLookup("ranger")

#10 folds(default) cross validation
set.seed(2021, sample.kind = "Rounding")
rf_cv_mdl <- train( DEFAULT~ .,
                    data = train_set,
                    method = 'ranger',
                    metric = 'Accuracy', 
                    importance = "impurity",
                    tuneGrid = expand.grid(
                      mtry = 3:10, splitrule = 'gini', min.node.size = 1), 
                    trControl = trainControl(method = 'cv'))
rf_cv_mdl

#plot mtry and accuracy
plot(rf_cv_mdl)

#plot the variable's importance
#"(rf_cv_mdl$finalModel$variable.importance" shows importance of the variables 
imp_cv_df <- data.frame(Variable = names(rf_cv_mdl$finalModel$variable.importance),
                        Importance = as.numeric(rf_cv_mdl$finalModel$variable.importance))

ggplot(imp_cv_df, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity") +coord_flip() #flip x axis and y axis 

#fit the model
rf_cv_pred <- predict(rf_cv_mdl, validation_set)

#confusion matrix
confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rf_tuned_results <- tibble(method = "random forest tuned",
                           Accuracy =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$overall[1],
                           Sensitivity =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[1],
                           Specificity =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[2],
                           Balanced_Accuracy =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[11])

rf_tuned_results %>% knitr::kable()#show in a table

#compare the two random forest models performance
bind_rows(rf_results, rf_tuned_results) %>% knitr::kable()

################################################################################
#evaluation
################################################################################

#showing all the models results
bind_rows(glm_s_results, 
          glm_f_results, 
          rpart_results, 
          rpart_tuned_results, 
          rf_results, 
          rf_tuned_results)%>% 
  knitr::kable()

#fit the best performance model
final_rf_pred <- predict(rpart_mdl, test_set,type="class")

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
final_results <- tibble( method ="final random forest",
                         Accuracy =confusionMatrix(final_rf_pred, test_set$DEFAULT)$overall[1],
                         Sensitivity =confusionMatrix(final_rf_pred, test_set$DEFAULT)$byClass[1],
                         Specificity =confusionMatrix(final_rf_pred, test_set$DEFAULT)$byClass[2],
                         Balanced_Accuracy = confusionMatrix(final_rf_pred, test_set$DEFAULT)$byClass[11]) %>% knitr::kable()
final_results
###
