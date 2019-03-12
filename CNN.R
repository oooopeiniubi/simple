library(rattle.data)
library(rnn)
data("weatherAUS")
data<-weatherAUS[1:3040,c(1,16)]
data.clean<-na.omit(data)
data_used<-data.clean[1:3000,]
x=data_used[,1]
y=data_used[,2]
x=matrix(x,nrow = 30)
y=matrix(y,nrow = 30)
source("nomalize,R")
y_scaled<-nomalize(y)
Y=t(y_scaled)
train=1:70
test=71:100
model<-trainr(Y=Y[train,],
              X=Y[train,],
              learningrate = 0.05,
              hidden_dim = 16,
              numepochs = 1000)
plot(colMeans(model$error),type = "l",xlab = "epoch",ylab = "error")
result<-predictr(model,Y[test,])
plot(as.vector(t(Y[test,])),
     col="red",
     type = "l",
     main = "testing set",
     ylab = "Y,result"
     )
lines(as.vector(t(result)),
      type = "l",
      col="blue"
      )
legend("bottomright",
       c("predict","actual"),
       col=c("red","blue"),
       lty = c(1,1),
       lwd = c(1,1)
       )