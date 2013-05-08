# REFERENCE:
# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# http://caret.r-forge.r-project.org/


### include
library(pls)
library(reshape) 
library(grid)
library(caret)

load("/home/susana/Documents/projetcs/01-component-correction/Data/UNIMANshort.X-Y-classGas.RData")

Y1<-gsub("\\.| ", "", Y)


str(Y1)


## el 75% de los datos para entrenamiento = 150 datos
inTrain <- createDataPartition(Y1, p = .75, list = FALSE)
str(inTrain)

dat<-as.data.frame(cbind(X,as.data.frame(Y1)))
training <- dat[inTrain,]
testing <- dat[-inTrain,]
nrow(training)
nrow(testing)


trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
tuneGrid <- data.frame(.k = c(2, 3, 4, 5))
knnFit <- train(training[,1:17], training[,18], 
  preProcess = c("center", "scale"),
  #tuneLength = 5, 
  method = "knn", tuneGrid = tuneGrid,
  trControl = trControl)

knnFit
plot(knnFit, main="knn-boot con Gas A,B y C en 8 concentraciones - UNIMANshort")

