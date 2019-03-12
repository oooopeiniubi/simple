data("iris")
iris.sub=iris[1:100,c(1,3,5)]
names(iris.sub)=c("sepal","petal","species")
library(ggplot2)
ggplot(iris.sub,aes(x=sepal,y=petal))+
  geom_point(aes(colour=species,shape=species),size=3)+
  xlab("species length")+
  ylab("petal length")+
  ggtitle("species vs sepal and petal lengths")
euclidean.norm<-function(x){sqrt(sum(x*x))}
distance.from.plane<-function(z,w,b){sum(z*w)+b}
classfy.linear<-function(x,w,b){
  distances=apply(x,1,distance.from.plane,w,b)
  return(ifelse(distances<0,-1,1))}
perceptron<-function(x,y,learning.rate=1){
  w=vector(length = ncol(x))
  b=0
  k=0
  R=max(apply(x, 1, euclidean.norm))
  mark.complete=TRUE
  
  while (mark.complete) {
  mark.complete=FALSE
  yc=classfy.linear(x,w,b)
  for (i in 1:nrow(x)) {
    if(y[i]!=yc[i]){ 
    w=w+learning.rate*y[i]*x[i,]
    b=b+learning.rate*y[i]*R^2
    k=k+1
    mark.complete=TRUE
      }
    }
  }
s=euclidean.norm(w)
return(list(w=w/s,b=b/s,updates=k))
}
x=cbind(iris.sub$sepal,iris.sub$petal)
y=ifelse(iris.sub$species=="setosa",1,-1)
p=perceptron(x,y)
plot(x,cex=0.2)
points(subset(x,y==1),col="black",pch="+",cex=2)
points(subset(x,y==-1),col="red",pch="*",cex=2)
intercept=-p$b/p$w[[2]]
slope=-p$w[[1]]/p$w[[2]]
abline(intercept,slope,col="green")