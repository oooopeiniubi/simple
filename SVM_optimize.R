library(e1071)
data("iris")
index<-1:nrow(iris)
np=ceiling(0.1*nrow(iris))#10%为测试数据
testindex=sample(1:nrow(iris),np)
testset=iris[testindex,]
trainset=iris[-testindex,]
tuned.model<-tune.svm(Species~.,data = trainset,gamma=10^(-3:-1),cost=10^(-1:-1))
summary(tuned.model)
model.optimize<-svm(Species~.,data = trainset,type="C-classification",cost=10,gamma=0.1)
svm.predict<-predict(model.optimize,testset)
table(svm.predict,testset$Species)
