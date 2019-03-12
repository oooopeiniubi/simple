concrete<-read.csv("concrete.csv")
nomalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
concrete_nomalize<-as.data.frame(lapply(concrete, nomalize))
options(digits = 7)
concrete_train<-concrete_nomalize[1:800,]
concrete_test<-concrete_nomalize[801:1030,]
#index=sample(1:nrow(data),round(0.7*nrow(data)))
#traindata<-as.data.frame(data[indexx,])
#testdata<-as.data.frame(data[-index,])随机取百分之70作为训练集
library(neuralnet)
concrete_model<-neuralnet(strength~.,data = concrete_nomalize,hidden = 10)#hidden=x
plot(concrete_model)#画图
model_result<-compute(concrete_model,concrete_test[1:8])
predict_result<-model_result$net.result
cor(predict_result,concrete_test$strength)
table(predict_result,concrete_test$strength)
library(lattice)
library(grid)
library(DMwR)
regr.eval(predict_result,concrete_test$strength,stats = c("mae","rmse"))


