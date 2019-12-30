# Load the data

setwd("/Users/wuweihan/Wu's Folder/§j•|/Statisitcal Learning/Final Project")
Data<-read.csv("heart.csv",header = T)


sum(is.na(Data)) # There is no NA. 
lapply(Data,class) # To check the class of all variables
# Change the data type of variables with the wrong type.
Data$sex <- as.factor(Data$sex)
Data$cp <- as.factor(Data$cp)
Data$fbs <- as.factor(Data$fbs)
Data$restecg <- as.factor(Data$restecg)
Data$exang <- as.factor(Data$exang)
Data$slope <- as.factor(Data$slope)
Data$thal <- as.factor(Data$thal)
Data$target <- as.factor(Data$target)

# Check again.
lapply(Data,class) 

# Check whether there is outlier.
# The result seems to be none.
summary(Data) 

##### Wu #####
head(Data)
library(MASS)
library(e1071)
library(class)
library(tidyverse)
attach(Data)
set.seed(10)
index=sample(1:nrow(Data),200)
train=Data[index,]
test=Data[-index,]

###### Logistic Regression #####
fit.logistic=glm(target~age+sex+cp+trestbps+chol+fbs+restecg
                 +thalach+exang+oldpeak+slope+ca+thal, family="binomial")
summary(fit.logistic)
pred.logistic=predict(fit.logistic, test)
pred.logistic <- ifelse(pred.logistic > 0.5, 1, 0)
table(pred.logistic,test$target)
logi.pred.error=mean(pred.logistic!=test$target)
logi.pred.error # error rate=11.65%

library(ROCR)
ROCR.logi=prediction(pred.logistic,test$target)
ROCRp.logi=performance(ROCR.logi,'tpr','fpr')
plot(ROCRp.logi,colorize=TRUE)
as.numeric(performance(ROCR.logi,'auc')@y.values) #88.35%

fit.logistic.modified=glm(target~age+sex+cp+oldpeak+ca, family="binomial")
summary(fit.logistic.modified)
pred.logistic.modified=predict(fit.logistic.modified, test)
pred.logistic.modified <- ifelse(pred.logistic.modified > 0.5, 1, 0)
table(pred.logistic.modified,test$target)
logi2.pred.error=mean(pred.logistic.modified!=test$target)
logi2.pred.error # error rate=17.48%

# Naive Bayes
fit.naive=naiveBayes(target~.,data=train)
pred.naive=predict(fit.naive,newdata=test)
table(pred.naive,test$target)
error=mean(pred.naive!=test$target)
error # error=12.62%

fit.naive2=naiveBayes(target~.-cp-thalach,data=train)
pred.naive2=predict(fit.naive2,newdata=test)
table(pred.naive2,test$target)
error2=mean(pred.naive2!=test$target)
error2 # error=15.53%

ROCR.naive=prediction(c(pred.naive),test$target)
ROCRp.naive=performance(ROCR.naive,'tpr','fpr')
plot(ROCRp.naive,colorize=TRUE)
as.numeric(performance(ROCR.naive,'auc')@y.values) #87.35%
