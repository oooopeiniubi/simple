library(caret)
library(ggplot2)
library(foreach)
library(doParallel)
library(iterators)
library(parallel)
library(adabag)
data("iris")
ind=sample(2,nrow(iris),replace=TRUE,prob = c(0.8,0.2))#分为20%和80%
traindata=iris[ind==1,]
testdata=iris[ind==2,]
model=boosting(Species~.,data = traindata,boos = TRUE,mfinal = 5)
result=predict.boosting(model,testdata)
result$confusion