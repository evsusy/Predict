# REFERENCE:
# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# http://caret.r-forge.r-project.org/


### include
library(pls)
library(reshape) 
library(grid)
library(caret)
library(class)

load("/home/susana/Documents/projetcs/01-component-correction/Data/UNIMANshort.X-Y-classGas.RData")
load("/home/susana/Documents/projetcs/01-component-correction/Data/UNIMANshort.Xc.RData")

train<-Xc[1:150,]
test<-Xc[151:200,]
clases<-factor(Y[1:150])
test.clases<-factor(Y[151:200])

##ajuste de parámetros
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
tuneGrid <- data.frame(.k = c(2, 3, 4, 5, 7, 9))

## knn Fit, hallar K
knnFit <- train(train, clases, 
  preProcess = c("center", "scale"),
  method = "knn", tuneGrid = tuneGrid,
  trControl = trControl)

knnFit
plot(knnFit, main="selección de k - UNIMANshort  - 8 clases")

kfit<-knnFit$bestTune
kfit


#otra forma de hallar el k
kfit2<-knn3(train, clases)
kfit2$k

#clasificación lineal
predict.train<-predict(knnFit,train)
predict.test <- predict(knnFit,test)
predict.test

error.lin<-1 - sum(predict.test == factor(Y[151:200]))/50
error.lin

### clasificaciòn usando knn

resultado.knn <- knn(train, test, clases, k=kfit, prob=TRUE)


error.knn<- 1 - sum(resultado.knn == factor(Y[151:200]))/50
error.knn

## Al calcular tasa de error aparente consideramos el mismo conjunto
## de entrenamiento y de test.

train1<-X[1:100,]
test1<-X[101:200,]
clases1<-factor(Y[1:100])
resultado.knn1 <- knn(train1, train1, clases1, k=kfit, prob=TRUE)

error.aparente <- 1 - sum(resultado.knn1 == clases1)/100
error.aparente

##tasa de error por validación cruzada en training
resultado.knn.cv <- knn.cv(train, clases, kfit)
error.knn.cv <- 1 - sum(resultado.knn.cv == clases)/150
error.knn.cv


##tasa de error por validación cruzada en test
resultado.knn.cv2 <- knn.cv(test, factor(Y[151:200]), kfit)
error.knn.cv2 <- 1 - sum(resultado.knn.cv2 == factor(Y[151:200]))/50
error.knn.cv2

##tasa de error por validación cruzada en todo el conjunto de datos X
resultado.knn.cv3 <- knn.cv(X, Y, kfit)
error.knn.cv3 <- 1 - sum(resultado.knn.cv3 == Y)/200
error.knn.cv3
