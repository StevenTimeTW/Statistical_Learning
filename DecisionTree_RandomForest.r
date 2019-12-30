# Load the data
Data=read.csv("C:/Users/USER/Desktop/heart.csv",header=T)
Data
dim(Data)

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

#split data to 200:103
set.seed(10)
xxx=sample(1:nrow(Data),200)
train=Data[xxx,]
test=Data[-xxx,]
train

#decision tree ®œ•Œrpart
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
install.packages('party')
install.packages('caret')
install.packages('e1071')
library(caret)
library(e1071)
library(party)
tree.1=rpart(target~.,data=train,method='class')
tree.1
rpart.plot(tree.1)
pred.1=predict(tree.1,newdata=test,type='class')
#∑«ΩT≤v
table(test$target,pred.1)
#®œ•ŒROC AUC
install.packages('pROC')
install.packages('ROCR')
library(pROC)
library(ROCR)
pred.1.1=predict(tree.1,newdata=test)
ROCR.1=prediction(pred.1.1[,2],test$target)
ROCRp.1=performance(ROCR.1,'tpr','fpr')
plot(ROCRp.1,colorize=TRUE,main='CART ROC¶±Ωu')
as.numeric(performance(ROCR.1,'auc')@y.values)
###ßQ•Œcp®”≠◊∞≈rpart###
printcp(tree.1)
plotcp(tree.1)
prune_tree.1=prune(tree.1, cp = tree.1$cptable[which.min(tree.1$cptable[,"xerror"]),"CP"])
rpart.plot(prune_tree.1)
prune_tree.pred=predict(prune_tree.1,newdata=test,type='class')
table(test$target,prune_tree.pred)
pred.1.2=predict(prune_tree.1,newdata=test)
ROCR.2=prediction(pred.1.2[,2],test$target)
ROCRp.2=performance(ROCR.2,'tpr','fpr')
plot(ROCRp.2,colorize=TRUE,main='∏g≠◊∞≈CART ROC¶±Ωu')
as.numeric(performance(ROCR.2,'auc')@y.values)
#############################

#######decision tree ¶€§v∞≈
tree.2=tree(target~.,data=train,method='class')
plot(tree.2)
text(tree.2,pretty=0)
pred.2=predict(tree.2,newdata=test,type='class')
table(test$target,pred.2)
pred.2.1=predict(tree.2,newdata=test)
ROCR.2=prediction(pred.2.1[,2],test$target)
ROCRp.2=performance(ROCR.2,'tpr','fpr')
plot(ROCRp.2,coloriZe=TRUE)
as.numeric(performance(ROCR.2,'auc')@y.values)

set.seed(10)
cv_tree = cv.tree(tree.2,FUN=prune.tree)  
cv_tree  
plot(cv_tree$size ,cv_tree$dev ,type="b")

prune_tree = prune.tree(tree.2,best =3)  #≠◊∞≈
plot(prune_tree)
text(prune_tree,pretty=0)
pred.3=predict(prune_tree,newdata=test,type='class')
table(test$target,pred.3)
pred.3.1=predict(prune_tree,newdata=test)
ROCR.3=prediction(pred.3.1[,2],test$target)
ROCRp.3=performance(ROCR.3,'tpr','fpr')
plot(ROCRp.3,colorize=TRUE)
as.numeric(performance(ROCR.3,'auc')@y.values)
####
########•Œcv#################
train_control=trainControl(method="cv", number=10)
train_control.model=train(target~., data=train, method="rpart", trControl=train_control)
train_control.model
#############################
#randomforest
set.seed(10)
rf.0=randomForest(target~.,data=train)
plot(rf.0)
rf.1=randomForest(target~.,data=train,importance=TRUE,ntree=2000)
pred.4=predict(rf.1,newdata=test,type = 'class')
table(test$target,pred.4) 
importance(rf.1)
varImpPlot(rf.1,main="Random Forest variable importance plot")
pred.4.1=predict(rf.1,newdata=test,type='prob')
pred.4.1
ROCR.4=prediction(pred.4.1[,2],test$target)
ROCRp.4=performance(ROCR.4,'tpr','fpr')
plot(ROCRp.4,colorize=TRUE,main='Random Forest ROC¶±Ωu')
as.numeric(performance(ROCR.4,'auc')@y.values)


rf.2=randomForest(target~.,data=train,importance=TRUE,ntree=2000,mtry=10)
rf.3=randomForest(target~.,data=train,importance=TRUE,ntree=2000,mtry=11)
rf.4=randomForest(target~.,data=train,importance=TRUE,ntree=2000,mtry=12)
pred.5=predict(rf.2,newdata=test,type = 'class')
pred.6=predict(rf.3,newdata=test,type = 'class')
pred.7=predict(rf.4,newdata=test,type = 'class')
table(test$target,pred.5)
table(test$target,pred.6)
table(test$target,pred.7)

fit.lda=lda(target~.,data=train)
pred.lda=predict(fit.lda,newdata=test,type='class')
table(test$target,pred.lda$class)
