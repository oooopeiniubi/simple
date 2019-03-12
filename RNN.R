library(digest)
library(rnn)
X1=sample(0:127,7000,replace = TRUE)
X2=sample(0:127,7000,replace = TRUE)
Y=X1+X2
x1<-int2bin(X1,length = 8)
x2<-int2bin(X2,length = 8)
X=array(c(x1,x2),dim=c(dim(x1),2))
Y=int2bin(Y,length = 8)
model<-trainr(Y=Y[,dim(Y)[2]:1],
              X=X[,dim(X)[2]:1,],
              learningrate=0.1,
              hidden_dim=10,
              batch_size=100,
              numepochs=100)
plot(colMeans(model$error),type = "l",xlab = "epoch",ylab = "errors")
A1=int2bin(sample(0:127,7000,replace = TRUE),length = 8)
A2=int2bin(sample(0:127,7000,replace = TRUE),length = 8)
A=array(c(A1,A2),dim=c(dim(A1),2))
B=predictr(model,A[,dim(A)[2]:1,])
B=B[,dim(B)[2]:1]
A1=bin2int(A1)
A2=bin2int(A2)
B=bin2int(B)
hist(B-(A1+A2))
