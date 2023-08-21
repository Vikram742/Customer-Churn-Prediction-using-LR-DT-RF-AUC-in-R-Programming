#read the data
data<- read.csv("clipboard", sep ='\t', header = T)
names(data)
str(data)
## convert data type
data$Churn<- as.factor(data$Churn)
data$ContractRenewal<-as.factor(data$ContractRenewal)
data$DataPlan<-as.factor(data$DataPlan)

## check for missing values
sum(is.na(data)) # No missing values found

# run logistic regression model
model<- glm(Churn~., data = data, family = "binomial")
summary(model)

# use stepwise model which will include all the variables and start removing the insignificant variable one
# after the other 
model1<- step(model, direction = "backward", trace = 0)
summary(model1)

# Variables that has been removed are "AccountWeek" , "DayCalls", "DataUsage", "MonthlyCharge"

# Let us check for multicollinearity
library(car)
vif(model1)
# All the absolute values are below 5. This means that there is no high correlation between the independent variables

# split the data into train and test
library(caret)
set.seed(1234)
index<- createDataPartition(data$Churn, p = 0.8 ,list = FALSE )
train<- data[index,]
test<-data[-index,]

# use the train data to create the model
model2<- glm(Churn~., data = train , family = "binomial")
summary(model2)
model3<-step(model2, direction = "backward", trace = 0)
summary(model3)

# use the test data for prediction
predicted<-predict(model3, newdata = test , type = "response")
predicted # this shows the probability values
test$predicted<- predicted # we create a new column called predicted in the test data which show the probability values
test$class<-ifelse(test$predicted >= 0.5,1,0) # we create a new column called class in the test data which converts
# the probability values to levels of 1 and 0
str(test$class) # this is numerical vector which needs to be changed to factor vector as our dependent variable is
# also a factor vector
test$class<- as.factor(test$class)
confusionMatrix(test$class, test$Churn, positive = "1")
# We use positive = 1 as we are more interested in finding out the churners who are assigned as 1
# Accuracy is 85.89% , Sensitivity is 11.45% , Specificity is 98.42%

# One reason for the sensitivity to be so low could be if the data is imbalanced. Let's check for it

table(train$Churn)
barplot(table(train$Churn))
# 2280 nos are non churners and 387 are churners. It is a very imbalanced data. We need to balance the data

2280*2 = 4560 # for oversampling
387*2 = 774 # for undersampling
(4560+774)/2 = 2667 # for both sampling
?ovun.sample
# Logistic regression for oversampling, undersampling and both sampling
set.seed(1234)
library(caret)
install.packages("ROSE")
library(ROSE)
set.seed(1234)
over_data<- ovun.sample(Churn~., data= train, method = "over", N = 4560)$data
table(over_data$Churn)

set.seed(1234)
under_data<-ovun.sample(Churn~., data = train, method = "under" , N = 774)$data
table(under_data$Churn)

set.seed(1234)
both_data<-ovun.sample(Churn~., data = train, method = "both", p =0.5, N = 2667)$data
table(both_data$Churn)

# Predicting the test data for over, under and both
set.seed(1234)
over_model<- glm(Churn~., data = over_data, family = "binomial") 
predict_over<- predict(over_model, newdata = test , type = "response") 
predict_over
test$predict_over<-predict_over
test$predict_over_class<- ifelse(test$predict_over >= 0.5 , 1 , 0)
str(test$predict_over_class)
test$predict_over_class<- as.factor(test$predict_over_class)
confusionMatrix(test$predict_over_class, test$Churn, positive = "1")
# Accuracy 79.13% , Sensitivity : 75% , Specificity : 79.82% - Logistic Regression for Over Sampling

set.seed(1234)
under_model<-glm(Churn~., data = under_data, family ="binomial")
predict_under<-predict(under_model, newdata = test, type = "response")
predict_under
test$predict_under<-predict_under
test$predict_under_class<-ifelse(test$predict_under >=0.5 , 1 , 0)
str(test$predict_under_class)
test$predict_under_class<-as.factor(test$predict_under_class)
confusionMatrix(test$predict_under_class, test$Churn , positive = "1")
# Accuracy 79.13% , Sensitivity : 72.92% , Specificity : 80.18% - Logistic Regression for Under sampling

set.seed(1234)
both_model<- glm(Churn~., data = both_data, family = "binomial")
predict_both<- predict(both_model, newdata = test , type = "response")
predict_both
test$predict_both<- predict_both
test$predict_both_class<- ifelse(test$predict_both >=0.5 , 1 , 0)
str(test$predict_both_class)
test$predict_both_class<-as.factor(test$predict_both_class)
confusionMatrix(test$predict_both_class, test$Churn, positive = "1")
# Accuracy 79.13% , Sensitivity : 75% , Specificity : 79.82% - Logistic Regression for both sample

# Using Decision Tree for oversampling, undersampling and both sample
library(rpart)
set.seed(1234)
dt_model<- rpart(Churn~., data = over_data) 
pred_dt<- predict(dt_model, newdata = test, type ="class")
pred_dt # probability is in the form of levels 1,0
test$pred_dt<-pred_dt
str(test$pred_dt) #just checking to see if it is a factor variable
confusionMatrix(test$pred_dt, test$Churn, positive = "1")
# Accuracy 87.84% , Sensitivity : 82.29% , Specificity : 88.77% - Decision Tree over sample

set.seed(1234)
dt_model1<- rpart(Churn~., data = under_data)
pred_dt1<- predict(dt_model1, newdata = test, type = "class")
pred_dt1
test$pred_dt1<-pred_dt1
str(test$pred_dt1)
confusionMatrix(test$pred_dt1, test$Churn, positive = "1")
# Accuracy 90.24% , Sensitivity : 83.33% , Specificity : 91.40% - Decision Tree under sample

set.seed(1234)
dt_model2<-rpart(Churn~., data = both_data)
pred_dt2<- predict(dt_model2, newdata =test, type = "class")
pred_dt2
test$pred_dt2<-pred_dt2
str(test$pred_dt2)
confusionMatrix(test$pred_dt2, test$Churn, positive = "1")
# Accuracy 87.84% , Sensitivity : 84.38% , Specificity : 88.42% - Decision Tree both sample

# RANDOM FOREST for oversampling , undersampling and both
library(randomForest)
set.seed(1234)
rf_model<- randomForest(Churn~., data = over_data)
predict_rf<- predict(rf_model, newdata = test, type ="class")
predict_rf
test$predict_rf<-predict_rf
str(test$predict_rf)
confusionMatrix(test$predict_rf, test$Churn, positive = "1")
# Accuracy 93.09% , Sensitivity : 66.67% , Specificity : 97.54% - Random Forest over sample

set.seed(1234)
rf_model1<- randomForest(Churn~., data = under_data)
predict_rf1<- predict(rf_model1, newdata = test , type = "class")
predict_rf1
test$predict_rf1<-predict_rf1
str(test$predict_rf1)
confusionMatrix(test$predict_rf1, test$Churn, positive = "1")
# Accuracy 88.74% , Sensitivity : 81.25% , Specificity : 90% - Random Forest under sample

set.seed(1234)
rf_model2<- randomForest(Churn~., data= both_data)
predict_rf2<- predict(rf_model2, newdata = test , type ="class")
predict_rf2
test$predict_rf2<- predict_rf2
str(test$predict_rf2)
confusionMatrix(test$predict_rf2, test$Churn, positive = "1")
# Accuracy 92.79% , Sensitivity : 72.92% , Specificity : 96.14% - Random Forest both sample

### Checking via ROC and AUC
library(pROC)
#Logistic Regression for oversampling (AUC)
set.seed(1234)
pred_lr<- predict(over_model, newdata= test, type ="response")
pred_lr
test$pred_lr<-pred_lr
par(pty = "s")
roc(test$Churn,test$pred_lr,plot=TRUE,percent=TRUE,legacy.axes = TRUE,
    xlab="False Positive Percentage",ylab="True Positive Percentage",col="#8BC3BE",lwd=1,print.auc=TRUE)
# AUC under Logistic Regression for oversampling is 82.9%

#Logistic Regression for undersampling (AUC)
set.seed(1234)
pred_lr1<-predict(under_model,newdata= test,type="response")
pred_lr1
test$pred_lr1<-pred_lr1
par(pty="s")
roc(test$Churn,test$pred_lr1,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab="False Positive Percentage",ylab="True Positive Percentage",col="#BC8BC3",lwd=1,print.auc=TRUE)
# AUC under Logistic Regression for undersampling is 82.9%


#Logistic Regression for bothsampling (AUC)
set.seed(1234)
pred_lr2<-predict(both_model, newdata=test, type="response")
pred_lr2
test$pred_lr2<-pred_lr2
par(pty="s")
roc(test$Churn,test$pred_lr2,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab="False Positive Percentage",ylab="True Positive Percentage",col="#BFC38B",lwd=1,print.auc=TRUE)
# AUC under Logistic Regression for both sampling is 82.5%

# Decision Tree for oversampling (AUC)
set.seed(1234)
pred_dt3<- predict(dt_model, newdata = test , type = "prob")
pred_dt3
test$pred_dt3<-pred_dt3[,2]
par(pty = "s")# pty sets the aspect ratio of the plot region. "s" creates a square plotting region
roc(test$Churn, test$pred_dt3, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=1, print.auc=TRUE)
# AUC under Decision Tree for oversampling is 89.2%

# Decision Tree for undersampling (AUC)
set.seed(1234)
pred_dt4<- predict(dt_model1, newdata = test , type = "prob")
pred_dt4
test$pred_dt4<-pred_dt4[,2]
par(pty = "s")
roc(test$Churn,test$pred_dt4,plot=TRUE,percent = TRUE,legacy.axes= TRUE,
    xlab= "False Positive Percentage", ylab="True Postive Percentage", col="#D40D52", lwd=1, print.auc= TRUE )
# AUC under Decision Tree for undersampling is 88.9%

# Decision Tree for bothsampling (AUC)
set.seed(1234)
pred_dt5<- predict(dt_model2, newdata= test, type = "prob")
pred_dt5
test$pred_dt5<- pred_dt5[,2]
par(pty = "s")
roc(test$Churn,test$pred_dt5,plot=TRUE,percent = TRUE,legacy.axes = TRUE,
    xlab= "False Positive Percentage" , ylab = "True Positive Percentage", col ="#377eb8", lwd=1, print.auc=TRUE )
# AUC under Decision Tree for both sampling is 88.2%

# RANDOM FOREST for oversampling (AUC)
set.seed(1234)
pred_rf4<- predict(rf_model, newdata = test, type = "prob")
pred_rf4
test$pred_rf4<-pred_rf4[,2]
par(pty="s")
roc(test$Churn, test$pred_rf4,plot=TRUE,percent=TRUE,legacy.axes = TRUE, 
    xlab= "False Positive Percentage" , ylab = "True Positive Percentage",col= "#D4AC0D", lwd=1, print.auc=TRUE)
# AUC under Random Sampling for over sampling is 89.6%

# RANDOM FOREST for undersampling (AUC)
set.seed(1234)
pred_rf5<- predict(rf_model1,newdata = test, type="prob")
pred_rf5
test$pred_rf5<-pred_rf5[,2]
par(pty="s")
roc(test$Churn,test$pred_rf5,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab= "False Positive Percentage" , ylab = "True Positive Percentage",col="#FF5733",lwd=1,print.auc=TRUE)
# AUC under Random Sampling for under sampling is 90.4%

# RANDOM FOREST for bothsampling (AUC)
set.seed(1234)
pred_rf6<-predict(rf_model2,newdata = test, type = "prob")
pred_rf6
test$pred_rf6<-pred_rf6[,2]
par(pty="s")
roc(test$Churn,test$pred_rf6,plot=TRUE,percent=TRUE,legacy.axes=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage", col="#8CEC5C",lwd=1,print.auc=TRUE)
# AUC under Random Sampling for both sampling is 89.4%

