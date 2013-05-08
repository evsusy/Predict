# REFERENCE:
# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# http://caret.r-forge.r-project.org/


### include
library(pls)
library(reshape) 
library(grid)
library(caret)
library(e1071)

#establece el path

fichero<-setwd("/home/susana/Documents/maestria/data bases UCI")
files <- file.path(fichero, list.files(fichero))


# leer los datos
X<- matrix(nrow=0, ncol=128)
Y<- matrix(nrow=0, ncol=1)
for(i in 1:length(files)){
  data <- read.matrix.csr(files[i])
          X0<-(as.matrix(data$x))
          X<-rbind(X,X0)
  
          Y0<-as.matrix(data$y)
          Y<-rbind(Y,Y0)
}

mod<-prcomp(X,scale=TRUE,center=TRUE)
#scoreplot(mod, labels=Y)
#scoreplot(mod, labels=Y,col=Y)
scoreplot(mod, labels=Y,col=as.numeric(Y))
legend("bottomright", fill=1:8, border="BLUE",  levels(Y), bty="n", ) 

## el 75% de los datos para entrenamiento = 150 datos
inTrain <- createDataPartition(as.matrix(uci1), p = .75, list = FALSE)
str(inTrain)

dat<-as.data.frame(cbind(X,classGas))
training <- dat[inTrain,]
testing <- dat[-inTrain,]
nrow(training)
nrow(testing)

knnFit <- train(training[,1:17], training[,18],
  tuneGrid=data.frame(.k = c(2, 3, 4, 5),
  method = "knn", preProcess = c("center", "scale"), 
  trControl= trainControl(method = "repeatedcv" )))

knnFit
plot(knnFit, main="knn-cv con Gas A,B y C - UNIMANshort")

############################################

knnFit <- train(training[,1:17], training[,18], tuneGrid=data.frame(.k = c(2, 3, 4, 5),
  method = "knn", preProcess = c("center", "scale"), 
 trControl= trainControl(method = "repeatedcv")))
