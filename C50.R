library(C50)
data("iris")
# np<-ceiling(0.1*nrow(iris))#随机取10%作为测试集
# iris_test<-sample(1:nrow(iris),np)
nomalize<-function(x){return((x-min(x))/(max(x)-min(x)))}#归一化
iris[,1:4]=as.data.frame(lapply(iris[,1:4],nomalize))
c=C5.0Control(subset = FALSE,bands = 0,winnow = FALSE,CF=0.25,minCases = 2,
              fuzzyThreshold = FALSE,
              sample = 0.9,#取90%的数据训练
              seed = sample.int(4096,size = 1) - 1L,
              earlyStopping = TRUE)
irirs.treemodel=C5.0(x=iris[,-5],y=iris$Species,control = c)
tt=as.character(irirs.treemodel$output)
library(stringr)
x=str_locate_all(tt,"<<")
y=substr(tt,x[[1]][2]-9,x[[1]][2]-6)
test.error=as.numeric(y)
test.eccorect=100-test.error
test.eccorect
# iris_test<-sample(1:nrow(iris),np)
# iris.testdata<-iris[iris_test,]
# iris.testdata<-iris[-iris_test,]
# iris.testdata<-iris[iris_test,]
# iris.traindata<-iris[-iris_test,]
# c=C5.0Control(subset = FALSE,bands = 0,winnow = FALSE,CF=0.25,minCases = 2,
#               fuzzyThreshold = FALSE,sample = 0,seed = sample.int(4096,size = 1) - 1L,
#               earlyStopping = TRUE)
# irirs.treemodel=C5.0(x=iris.traindata[,-5],y=iris.traindata$Species,control = c)
# test.output=predict(irirs.treemodel,iris.testdata[,-5],type="class")
# n=length(test.output)
# number=0
# for (i in 1:n) {if (test.output[i]==iris.testdata[i,5]){number=number+1}
#   
# }
# test.accuracy<-number/n*100
# test.accuracy
table(test.output,iris.testdata$Species)