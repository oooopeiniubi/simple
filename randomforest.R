library(randomForest)
data("iris")
ind=sample(2,nrow(iris),replace=TRUE,prob = c(0.8,0.2))#分为20%和80%
traindata=iris[ind==1,]
testdata=iris[ind==2,]
rf.model=randomForest(Species~.,data = traindata,ntree=100)
preddict.result=predict(rf.model,testdata)
table(preddict.result,testdata$Species)