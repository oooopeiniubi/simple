library(Rcpp)
library(RSNNS)
data(iris)
iris=iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]#内部清洗（随机排序）
iris.value<-iris[,1:4]
iris.target<-decodeClassLabels(iris[,5])#讲类别改为0和1表示
iris<-splitForTrainingAndTest(iris.value,iris.target,ratio = 0.15)
iris<-normTrainingAndTestSet(iris)
model<-mlp(iris$inputsTrain,
           iris$targetsTrain,size = 5,
           learnFuncParams = c(0.1),
           maxit = 50,
           inputsTest = iris$inputsTest,
           targetsTest = iris$targetsTest)
summary(model)
weightMatrix(model)
plotIterativeError(model)
predictions<-predict(model,iris$inputsTest)
plotRegressionError(predictions[,2],iris$targetsTest[,2])
confusionMatrix(iris$targetsTrain,fitted.values(model))
confusionMatrix(iris$targetsTest,predictions)
par(mfrow=c(1,2))
plotROC(fitted.values(model)[,2],iris$targetsTrain[,2])
plotROC(predictions[,2],iris$targetsTest[,2])
