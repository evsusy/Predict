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
load("/home/susana/Documents/projetcs/02-validation-caret-package/Data/UNIMANshort.conc-ind-gas.RData")


## el 75% de los datos para entrenamiento = 150 datos

train<-Xc[1:150,]
test<-Xc[151:200,]

train.clases<-as.matrix(conc[1:150])
test.clases<-as.matrix(conc[151:200])


##ajuste de parámetros
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

## pls Fit

plsFit1 <- train(train, train.clases, method = "pls",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trControl)



##Regresión pls

predict.test<-predict(plsFit1, newdata = test)
predict.test.round<-round(predict.test,2)


## Dempeño de PLS en el cojunto de validación

error.pls<- RMSE((predict.test), test.clases)
error.pls.round<-RMSE((predict.test.round), test.clases)

error.pls
round(error.pls.round,2)


###Máquinas de soporte vectorial

svmFit1 <- train(train, train.clases, method = "svmRadial",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trControl)

#Regresión svm

predict.test1<-predict(svmFit1, newdata = test)
predict.test1.round<-round(predict.test1,2)

## Dempeño de svm en el cojunto de validación

error.svm <- RMSE((predict.test1), test.clases)
error.svm.round<-RMSE((predict.test1.round), test.clases)

error.svm
round(error.svm.round,2)

