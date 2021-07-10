################################################################################
# package and dataset
################################################################################

#tidyverse, gridExtra, caret, e1071, rpart, DataExplorer, ranger
#use "require" to install the packages

if (!require(tidyverse)){install.packages("tidyverse")
library(tidyverse)
}

if (!require(gridExtra)){install.packages("gridExtra")
  library(gridExtra)
}
#expansion of ggplot

if (!require(e1071)){install.packages("e1071")
  library(e1071)
}
#misc Functions of the Department of Statistics

if (!require(caret)){install.packages("caret")
  library(caret)
}
#cross validation 

if(!require(rpart)) {install.packages("rpart") 
  library(rpart)
}
#to make decision tree model

if (!require(rpart.plot)){install.packages("rpart.plot")
  library(rpart.plot)
}
#to plot decision tree

if (!require(DataExplorer)){install.packages("DataExplorer")
  library(DataExplorer)
}
#to do data exploration (correlation matrix etc.)

if (!require(ranger)){install.packages("ranger")
  library(ranger)
}
#random forest. new package that is much faster than "randomForest" package

#my GitHub direct link to the file
url <-"https://github.com/masa951125/Final_project/raw/main/UCI_Credit_Card.csv"

#download the file into your working folder.
download.file(url, "original_default.csv")

#"read_csv" in tidyverse package to download the file. 
#it creates a tibble dataset whose values are numerical.
original_default <- read_csv("original_default.csv")

################################################################################
#data exploration
################################################################################

#see overview of the dataset
str(original_default)

#number of NAs in the dataset. "is.na" checks whether there are any NAs or not 
sum(is.na(original_default))

#number of Nulls in the dataset."is.null" checks whether there are any nulls or not 
sum(is.null(original_default))

#draw correlation matrix graph
#"plot_correlation" in DataExplorer package show matrix graph. 
plot_correlation(original_default)

###########
#1 outcome
###########

#calculate the proportion of default and non-default. "prop.table" shows its proportion
prop.table(table(original_default$default.payment.next.month))

#change name of "default.payment.next.month"
n <-which(names(original_default)=="default.payment.next.month") 
#"default.payment.next.month" is "n" th column in the dataset

#change it to "DEFAULT"
names(original_default)[n] <- "DEFAULT"

#change outcome into factor using "as.factor"function
original_default$DEFAULT <- as.factor(original_default$DEFAULT)

#############
#2 LIMIT_BAL
#############

#show summary of the column
summary(original_default$LIMIT_BAL)

#draw histogram filling the bar with proportion of defaults("fill"argument)
ggplot(data=original_default, aes(LIMIT_BAL, fill=DEFAULT), ) +
  geom_histogram(bins=50) #narrower bins than default(30)

#######
#2 SEX
#######
#find unique values in the column
sort(unique(original_default$SEX))
#use "sort" function to show values in order

#proportion of genders, using"prop.table" as before
prop.table(table(original_default$SEX))

#draw distribution and proportion side by side
#distribution graph filling the bar with proportion of defaults("fill"argument)
sex_ditribution <- original_default %>% 
  ggplot(aes(x=as.factor(SEX), fill= DEFAULT)) + #factoring to make x axis simple
  geom_bar() +
  ggtitle("SEX Distribution")

#stacked bar graph filling the bar with proportion of defaults("fill"argument)
sex_stackedbar <-original_default %>% 
  ggplot(aes(x=as.factor(SEX), fill= DEFAULT)) +
  geom_bar(position="fill") + #argument position is used to make stacked bar graph
  ggtitle("SEX Proportion") 

#"grid.arrange" in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(sex_ditribution, sex_stackedbar, nrow=1, ncol=2)

#############
#3 EDUCATION
#############

#find unique values in the column
sort(unique(original_default$EDUCATION))
#use "sort" function to show values in order

#draw distribution and proportion side by side
#distribution graph filling the bar with proportion of defaults("fill"argument)
EDU_distribution <-original_default %>% 
  ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) + 
  #factoring to make x axis simple
  geom_bar() +
  ggtitle("EDUCATION Distribution")

#stacked bar graph filling the bar with proportion of defaults("fill"argument)
EDU_stackedbar <-original_default %>% 
  ggplot(aes(x=as.factor(EDUCATION), fill= DEFAULT)) +
  geom_bar(position="fill") + #argument position is used to make stacked bar graph
  ggtitle("EDUCATION Proportion")

#"grid.arrange" in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(EDU_distribution, EDU_stackedbar, nrow=1, ncol=2)

############
#4 marriage
############

#find unique values in the column
sort(unique(original_default$ MARRIAGE))
#use "sort" function to show values in order

#draw distribution and proportion side by side
#distribution graph filling the bar with proportion of defaults("fill"argument)
MAR_distribution <- original_default %>% 
  ggplot(aes(x=as.factor(MARRIAGE), fill= DEFAULT))+ 
  #factoring to make x axis simple
  geom_bar() +
  ggtitle("MARRIAGE Distribution")

#stacked bar graph filling the bar with proportion of defaults("fill"argument)
MAR_proportion <-original_default %>% 
  ggplot(aes(x=as.factor(MARRIAGE), fill= DEFAULT)) +
  geom_bar(position="fill") + #argument position is used to make stacked bar graph
  ggtitle("MARRIAGE Proportion")

#"grid.arrange" in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(MAR_distribution, MAR_proportion, nrow=1, ncol=2)

#######
#5 AGE
#######

#show summary
summary(original_default$AGE)

#draw distribution and proportion side by side
#distribution graph filling the bar with proportion of defaults("fill"argument)
AGE_distribution <- original_default %>%
  ggplot(aes(AGE, fill=DEFAULT)) +
  geom_histogram(bins=30)+ #using bins=30
  ggtitle("AGE Distribution")

#stacked bar graph filling the bar with proportion of defaults("fill"argument)
AGE_proportion <- original_default %>% 
  ggplot(aes(x=AGE, fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("AGE Proportion")

#"grid.arrange" in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(AGE_distribution, AGE_proportion, nrow=2, ncol=1)

#######
#6 PAY
#######

#PAY_0 unique values
sort(unique(original_default$PAY_0))
#use "sort" function to show values in order

#draw distribution and proportion side by side
#distribution graph filling the bar with proportion of defaults("fill"argument)
PAY_distribution <-original_default %>% 
  ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) + #factoring to make graph clearer
  geom_bar() +
  ggtitle("PAY_0 Distribution")

#stacked bar graph filling the bar with proportion of defaults("fill"argument)
PAY_stackedbar <- original_default %>% 
  ggplot(aes(x=as.factor(PAY_0), fill= DEFAULT)) +
  geom_bar(position="fill") +
  ggtitle("PAY_0 Proportion")

#"grid.arrange" in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(PAY_distribution, PAY_stackedbar, nrow=2, ncol=1)

#draw each PAY graph and stored
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

#"grid.arrange" in gridExtra package to place 6 graphs in a sheet
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(graph_p0, graph_p2, graph_p3, graph_p4, graph_p5, graph_p6, nrow=2, ncol=3)

############
#7 BILL_AMT
############

#show summary
summary(original_default$BILL_AMT1)

#distribution graph filling the bar with proportion of defaults("fill"argument)
ggplot(data=original_default, aes(BILL_AMT1,fill= DEFAULT)) +
  geom_histogram(bins=30)# use 30 bins

#draw each BILL_AMT graph and stored. use 30 bins
b1 <- ggplot(data=original_default, aes(BILL_AMT1)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#x axis labels are skewed to avoid overlapping

b2 <- ggplot(data=original_default, aes(BILL_AMT2)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

b3 <- ggplot(data=original_default, aes(BILL_AMT3)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

b4 <- ggplot(data=original_default, aes(BILL_AMT4)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

b5 <- ggplot(data=original_default, aes(BILL_AMT5)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

b6 <- ggplot(data=original_default, aes(BILL_AMT6)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#grid.arrange in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(b1,b2,b3,b4,b5,b6, nrow=2, ncol=3)

##############
#7 pay amount 
##############

#show summary
summary(original_default$PAY_AMT1)

#distribution graph filling the bar with proportion of defaults("fill"argument)
ggplot(data=original_default, aes(PAY_AMT1,fill= DEFAULT)) +
  geom_histogram(bins=30)# use 30 bins

#proportion of clients whose PAY_AMT is 0
mean(original_default$PAY_AMT1==0)

#draw all PAY_AMT graphs and stored. use 30 bins
p1 <- ggplot(data=original_default, aes(PAY_AMT1)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#x axis labels are skewed to avoid overlapping

p2 <- ggplot(data=original_default, aes(PAY_AMT2)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(data=original_default, aes(PAY_AMT3)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(data=original_default, aes(PAY_AMT4)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(data=original_default, aes(PAY_AMT5)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(data=original_default, aes(PAY_AMT6)) +geom_histogram(bins=30)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#grid.arrange in gridExtra package to draw two graphs side by side
#using nrow(how many graphs placed horizontally) 
#and ncol(how many graphs placed vertically) arguments)
grid.arrange(p1,p2,p3,p4,p5,p6, nrow=2, ncol=3)

################################################################################
#data preparation
################################################################################

#remove ID column using "select "function
original_default <- original_default %>% select(-ID)

#Regarding categorical data, change numeric to factor, using "as.factor" function
#SEX,EDUCATION,MARRIAGE, PAY_0~PAY_6  are categorical data
#change attributes of columns using "mutate" function
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

#pick up categorical data columns and make a vector"cat_col"
cat_col <- c("SEX", "EDUCATION", "MARRIAGE",
             "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "DEFAULT")

#make a vector which contains all columns' name
all_col <- names(original_default)

#extract numerical data columns leaving out categorical columns
num_col <- all_col[-which(all_col %in% cat_col)]

#scaling numerical data using function "scale"
original_default[num_col] <-original_default %>% select(-all_of(cat_col)) %>% scale()
str(original_default)
summary(original_default)

#"set seed" to fixate results as we will use random sampling
set.seed(2021, sample.kind = "Rounding")

#use "createDataPartition" function in caret package to produce index
index_1 <- createDataPartition(original_default$DEFAULT, p=0.2, list=F, times=1)

#split the dataset. 20% test_ set, 80% pre_train_set  
test_set <- original_default[index_1,]
pre_train_set <- original_default[-index_1,]


#"set seed" to fixate results as we will use random sampling
# set.seed(2021) if using R 3.5 or earlier
set.seed(2021, sample.kind = "Rounding") # if using R 3.6 or later

#split again
index_2 <- createDataPartition(pre_train_set$DEFAULT, p=0.2, list=F, times=1)

#split the pre_train_set. train_set 80%, validation_set 20%
validation_set <-pre_train_set[index_2,]
train_set <- pre_train_set[-index_2,]

#check "default" ratio and make a tibble
tibble(outcome =c(0,1),
       "train_set"= prop.table(table(train_set$DEFAULT)),
       "validation_set" = prop.table(table(validation_set$DEFAULT)),
       "test_set" = prop.table(table(test_set$DEFAULT)))

################################################################################
#model analysis
################################################################################

####################
#evaluation metrics
####################

#make a factor vector, all values are 0 (non_default) 
base_pred <-factor(numeric(length(validation_set$DEFAULT)),levels=c("0","1"))
#function numeric(n) produces n 0 vectors.

#make a confusion matrix using "confusionMatrix" function in caret package
confusionMatrix(base_pred, validation_set$DEFAULT)$table

#showing statistical metrics
confusionMatrix(base_pred, validation_set$DEFAULT)$overall[1] #accuracy
confusionMatrix(base_pred, validation_set$DEFAULT)$byClass[1] #sensitivity
confusionMatrix(base_pred, validation_set$DEFAULT)$byClass[2] #specificity
confusionMatrix(base_pred, validation_set$DEFAULT)$byClass[11] #balanced accuracy

#####################
#logistic regression 
#####################
#fewer variable logistic regression model using "glm" function
glm_fewer_mdl <- glm(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                       PAY_0 +  PAY_AMT1 + BILL_AMT1,
                     data= train_set, family= binomial(link = "logit"))

#show summary of the model
summary(glm_fewer_mdl)

#fit the model using "predict" function
glm_fewer_prob <- predict(glm_fewer_mdl, validation_set,type="response")
#type ="response" returns probability

glm_fewer_pred <- ifelse(glm_fewer_prob >0.5,1,0)
#"ifelse" function. if the probability is more than 0.5, it produces 1, otherwise 0.

#show confusion matrix
confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
glm_f_results <- tibble(method = "glm fewer features",
                        Accuracy =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$overall[1],
                        Sensitivity =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$byClass[1],
                        Specificity =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$byClass[2],
                        Balanced_Accuracy =confusionMatrix(as.factor(glm_fewer_pred), validation_set$DEFAULT)$byClass[11])

glm_f_results %>% knitr::kable() 
#show in a table using "kable" function in knitr package

#stepwise regression
#a null model with no predictors using "glm" function
null_model <- glm(DEFAULT~1, data = train_set, family = binomial(link = "logit"))

#a full model using all of the potential predictors using "glm" function
full_model <- glm(DEFAULT~., data = train_set, family = binomial(link = "logit"))

#stepwise logistic regression. function "step" conducts stepwise regression
step_mdl   <- step(null_model, 
                   scope = formula(full_model), 
                   direction = "both") #in this case, forward and backward 

#show summary of the model
summary(step_mdl)

#fit the model using "predict" function
step_prob <- predict(step_mdl, validation_set,type="response")
#type ="response" returns probability

step_pred <- ifelse(step_prob >0.5,1,0) 
#"ifelse" function. if the probability is more than 0.5, it produces 1, otherwise 0.

#show confusion matrix
confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
glm_s_results <- tibble(method = "glm step wise", 
                        Accuracy =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$overall[1],
                        Sensitivity =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[1],
                        Specificity =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[2],
                        Balanced_Accuracy =confusionMatrix(as.factor(step_pred), validation_set$DEFAULT)$byClass[11])

glm_s_results %>% knitr::kable() 
#show in a table using "kable" function in knitr package

#comparing two models using "bind_rows" function to make table
bind_rows(glm_f_results, glm_s_results) %>% knitr::kable()
#show in a table using "kable" function in knitr package

###############
#decision tree 
###############

#"rpart" function in rpart package makes a decision tree model
# set.seed(2021) if using R 3.5 or earlier
set.seed(2021, sample.kind = "Rounding") # if using R 3.6 or later
rpart_mdl <-rpart(DEFAULT ~ .,data = train_set)

#show model information
rpart_mdl

#draw decision tree using "rpart.plot" function in rpart.plot package
rpart.plot(rpart_mdl)

#fit the model using "predict" function
rpart_pred <- predict(rpart_mdl, validation_set, type="class")
#type="class" returns predicted outcome 0 or 1

#show confusion Matrix
confusionMatrix(rpart_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rpart_results <- tibble(method = "rpart",
                        Accuracy =confusionMatrix(rpart_pred, validation_set$DEFAULT)$overall[1],
                        Sensitivity =confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[1],
                        Specificity =confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[2],
                        Balanced_Accuracy =confusionMatrix(rpart_pred, validation_set$DEFAULT)$byClass[11])

rpart_results %>% knitr::kable() 
#show in a table using "kable" function in knitr package

#comarison decision tree and logistic regression
bind_rows(rpart_results, glm_s_results, glm_f_results) %>% knitr::kable()

#find parameters using "modelLookup" function in caret package
modelLookup("rpart")

#show used cp in the decision tree model
rpart_mdl$control$cp

#we use "train" function in caret package.
#as we do not define the number of "cV(cross validation)", 
#it conducts 10 folds(default) cross validation.
# set.seed(2021) if using R 3.5 or earlier
set.seed(2021, sample.kind = "Rounding") # if using R 3.6 or later
rpart_tuned_mdl <- train(DEFAULT ~ ., 
                      method = "rpart", 
                      tuneGrid = data.frame(cp = seq(0, 0.01, len = 25)),
                      control = rpart.control(minsplit = 0),
                      data = train_set)
#"rpart.control" determines Various parameters that control aspects of the rpart fit.
#"tuneGrid" is a data frame with possible tuning values.
#"minsplit" is the minimum number of observations that must exist in a node in order for a split to be attempted.

#show model information
rpart_tuned_mdl

#plot cp and accuracy
plot(rpart_tuned_mdl)

#draw a decision tree using "rpart.plot" function
rpart.plot(rpart_tuned_mdl$finalModel)

#fit the model using"predict"
rpart_tuned_pred <- predict(rpart_tuned_mdl, validation_set)

#show confusion matrix
confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rpart_tuned_results <- tibble(method = "rpart tuned",
                              Accuracy =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$overall[1],
                              Sensitivity =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[1],
                              Specificity =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[2],
                              Balanced_Accuracy =confusionMatrix(rpart_tuned_pred, validation_set$DEFAULT)$byClass[11])

rpart_tuned_results %>% knitr::kable()  
#show in a table using "kable" function in knitr package

#compare the two decision tree models' and logistic regression models' performance
bind_rows(rpart_tuned_results,rpart_results, 
          glm_s_results, glm_f_results) %>% knitr::kable()
#show in a table using "kable" function in knitr package

###############
#random forest
###############

#make random forest model using "ranger" function in ranger package
# set.seed(2021) if using R 3.5 or earlier
set.seed(2021, sample.kind = "Rounding") # if using R 3.6 or later
rf_mdl <- ranger(
  formula = DEFAULT ~ ., 
  data = train_set,
  importance = "impurity",#use gini index
  probability = F) # "probability = F" returns binary outcome

#show model information
rf_mdl

#plot the variable's importance
#"rf_mdl$variable.importance" shows importance of the variables. make a data.frame and plot 
imp_df <- data.frame(Variable = names(rf_mdl$variable.importance),
                     Importance = as.numeric(rf_mdl$variable.importance)) 

ggplot(imp_df, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity") +coord_flip() #flip x axis and y axis 

#fit the model using "predict"
rf_pred <- predict(rf_mdl, validation_set)$predictions
#it returns lists. So we need to select prediction outcomes.

#show confusion matrix
confusionMatrix(rf_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rf_results <- tibble(method = "random forest",
                     Accuracy =confusionMatrix(rf_pred, validation_set$DEFAULT)$overall[1],
                     Sensitivity =confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[1],
                     Specificity =confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[2],
                     Balanced_Accuracy =confusionMatrix(rf_pred, validation_set$DEFAULT)$byClass[11])

rf_results %>% knitr::kable() 
#show in a table using "kable" function in knitr package

#compare random forest result with other models
bind_rows(rf_results, 
          rpart_tuned_results, rpart_results,
          glm_s_results, glm_f_results) %>% knitr::kable()
#show in a table using "kable" function in knitr package

#find parameters which can be tuned using "modelLookup" function
modelLookup("ranger")

#we use "train" function in caret package.
#as we do not define the number of "cV(cross validation)", 
#it conducts 10 folds(default) cross validation.
# set.seed(2021) if using R 3.5 or earlier
set.seed(2021, sample.kind = "Rounding") # if using R 3.6 or later
rf_cv_mdl <- train( DEFAULT~ .,
                    data = train_set,
                    method = 'ranger',
                    metric = 'Accuracy', #show its performance based on accuracy
                    importance = "impurity", #using Gini index
                    tuneGrid = expand.grid(
                      mtry = 3:10, splitrule = 'gini', min.node.size = 1), 
                    trControl = trainControl(method = 'cv'))
#"tuneGrid" is a data frame with possible tuning values.(mtry, split rule, and minimal node size)
#"trControl" generates parameters that further control how models are created, with possible values. in this case, "cv"

#show model's information
rf_cv_mdl

#plot mtry and accuracy
plot(rf_cv_mdl)

#plot the variable's importance
#"(rf_cv_mdl$finalModel$variable.importance" shows importance of the variables 
imp_cv_df <- data.frame(Variable = names(rf_cv_mdl$finalModel$variable.importance),
                        Importance = as.numeric(rf_cv_mdl$finalModel$variable.importance))

ggplot(imp_cv_df, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity") +coord_flip() #flip x axis and y axis 

#fit the model using "predict"
rf_cv_pred <- predict(rf_cv_mdl, validation_set)

#show confusion matrix
confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$table

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
rf_cv_results <- tibble(method = "random forest tuned",
                           Accuracy =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$overall[1],
                           Sensitivity =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[1],
                           Specificity =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[2],
                           Balanced_Accuracy =confusionMatrix(rf_cv_pred, validation_set$DEFAULT)$byClass[11])

rf_cv_results %>% knitr::kable()
#show in a table using "kable" function in knitr package

#compare the two random forest models performance
bind_rows(rf_cv_results, rf_results) %>% knitr::kable()
#show in a table using "kable" function in knitr package

################################################################################
#evaluation
################################################################################

#showing all the models results
bind_rows(glm_s_results, 
          glm_f_results, 
          rpart_results, 
          rpart_tuned_results, 
          rf_results, 
          rf_cv_results)%>% 
  knitr::kable()
#show in a table using "kable" function in knitr package

#plot the variable's importance
#"rf_mdl$variable.importance" shows importance of the variables. make a data.frame and plot 
imp_df <- data.frame(Variable = names(rf_mdl$variable.importance),
                     Importance = as.numeric(rf_mdl$variable.importance)) 
ggplot(imp_df, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity") +coord_flip() #flip x axis and y axis 

#fit the best performance model using predict
final_rf_pred <- predict(rpart_mdl, test_set,type="class") 

#show metrics, accuracy, sensitivity, specificity, balanced accuracy
final_results <- tibble( method ="final random forest",
                         Accuracy =confusionMatrix(final_rf_pred, test_set$DEFAULT)$overall[1],
                         Sensitivity =confusionMatrix(final_rf_pred, test_set$DEFAULT)$byClass[1],
                         Specificity =confusionMatrix(final_rf_pred, test_set$DEFAULT)$byClass[2],
                         Balanced_Accuracy = confusionMatrix(final_rf_pred, test_set$DEFAULT)$byClass[11]) %>% 
  knitr::kable()
#show in a table using "kable" function in knitr package

final_results
###
