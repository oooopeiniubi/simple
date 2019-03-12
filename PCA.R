library(h2o)
h2o.init()
ausPath<-system.file("extdata","australia.csv",package = "h2o")
australia.hex<-h2o.uploadFile(path = ausPath)
pca_model<-h2o.prcomp(training_frame = australia.hex,
                      k=8,
                      transform = "STANDARDIZE")

barplot(as.numeric(pca_model@model$importance[2,]),
        main = "PCA model",
        xlab = "PCA component",
        ylab = "Proportion of Variance")
