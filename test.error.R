test.error<-function(xx){c<-C5.0Control(subset = FALSE,
                                        bands = 0,
                                        winnow = FALSE,
                                        noGlobalPruning = FALSE,
                                        CF=xx[1]/100,
                                        minCases = floor(xx[2]),
                                        fuzzyThreshold = FALSE,
                                        sample = 0,
                                        seed = sample.int(4096,size = 1)-1L,
                                        earlyStopping = TRUE
)
treeModel<-C5.0(x=iris.train[,-5],y=iris.train$Species,control = c) 
summary(treeModel)
test.output<-predict(treeModel,iris.test[,-5],type="class")
n=length(test.output)
number=0
for (i in 1:n) {
  if(test.output[i] != iris.test[i,5])
     {number=number+1  #error}
  }
}
  error.value<-number/n*100
  if(start_index==TRUE){
    best_CF<<-c$CF
    best_minCases<<-c$minCases
    min_error<<-error.value
  }
  if(error.value<min_error){
    best_CF<<-c$CF
    best_minCases<<-c$minCases
    min_error<<-error.value
  }
  error=error.value
  return(error)
  
  
  
  
}