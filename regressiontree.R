library(rpart)
data("iris")
np<-ceiling(0.1*nrow(iris))#随机取10%作为测试集
iris_test<-sample(1:nrow(iris),np)
nomalize<-function(x){return((x-min(x))/(max(x)-min(x)))}#归一化
iris[,1:4]=as.data.frame(lapply(iris[,1:4],nomalize))
iris_test<-sample(1:nrow(iris),np)
iris.testdata<-iris[iris_test,]
iris.testdata<-iris[-iris_test,]
iris.testdata<-iris[iris_test,]
iris.traindata<-iris[-iris_test,]
irirs.tree<-rpart(Species~.,data = iris.traindata,method = "class",control=rpart.control(minsplit = 5,cp=0.0001,maxdepth = 30))
plot(irirs.tree);text(irirs.tree)
species.traindata=iris$Species[-iris_test]
train.predict=factor(predict(irirs.tree,data=iris.traindata,type = "class"),levels = levels(species.traindata))
table(species.traindata,train.predict)
