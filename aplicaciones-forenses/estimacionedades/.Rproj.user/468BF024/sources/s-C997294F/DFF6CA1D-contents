#Aplicaciones de ciencia de datos:

#Estimación de edad, a partir de datos craneales.

#**************************************************
#Generamos las normales
#**************************************************

tabla<-as.data.frame(read.csv("data/data.csv", header = T, sep=","))
tabla_sin_edad<-tabla[,2:10]
edad<-tabla[,1]
install.packages("fitdistrplus")
library("fitdistrplus")

#Obtemos las normales y distribuciones en funcion de las curvas de crecimiento

lista_normales<-apply(tabla_sin_edad, 1, fitdist, "norm")

medias=vector()
sd=vector()
for (i in 1:38) {
  medias[i]<-lista_normales[[i]]$estimate[1]
  sd[i]<-lista_normales[[i]]$estimate[2]
}

#Comparamos las quantiles con el gráfico objetivo para comprobar si está bien

list_quantiles<-lapply(lista_normales, quantile, c(0.03, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.97))

quantiles=data.frame()
for (i in 1:38) 
{
  for(j in 1:9){
  quantiles[i,j]<-unlist(list_quantiles[[i]]$quantiles[j])
  }
}


plot(tabla[,1],quantiles$V1, ylim = c(20,54))
     lines(tabla[,1],quantiles$V2,col=2)
     lines(tabla[,1],quantiles$V3,col=3)
     lines(tabla[,1],quantiles$V4,col=4)
     lines(tabla[,1],quantiles$V5,col=5)
     lines(tabla[,1],quantiles$V6,col=6)
     lines(tabla[,1],quantiles$V7,col=7)
     lines(tabla[,1],quantiles$V8,col=8)
     lines(tabla[,1],quantiles$V9,col=9)

     
#Los gráficos se adaptan vamos a comprobar ahora sobre los otros datos
     
    
plot(tabla[,1],quantiles$V1, ylim = c(20,54), type="l")
     lines(tabla[,1], tabla_sin_edad[,1],col=1)
     lines(tabla[,1], quantiles$V5,col=2)
     lines(tabla[,1], tabla_sin_edad[,5],col=2)
     lines(tabla[,1], quantiles$V9,col=3)
     lines(tabla[,1], tabla_sin_edad[,9],col=3)
     
     
#**************************************************
#Generamos la muestra
#**************************************************
     

lista=list()
for(i in 1:38)
{
  lista[[i]]<-rnorm(1000, mean =lista_normales[[i]]$estimate[1], sd=lista_normales[[i]]$estimate[2]) 
}     

muestra<-as.data.frame(do.call(cbind,lista))
colnames(muestra)<-tabla[,1]
     

#**************************************************
#Aplicamos regresión para predecir cada edad
#**************************************************

#Cargamos los datos de uno de los compañeros para evitar soluciones distintas

muestra<-read.csv("data/Muestra.csv")

clase <- vector()
for(i in 1:38) {
  clase <- c(clase, rep.int(edad[i], 1000))
}
dfregresion <- data.frame(datos = as.vector(as.matrix(muestra[,2:39])), clase)
train<-dfregresion

#Usamos caret para regresión.

library(caret)

#Partimos en train y test
set.seed(123)
index <- createDataPartition(train$clase, p=0.75, list=FALSE)
trainSet <- train[ index,]
testSet <- train[-index,]
test<-testSet
#Entrenamos regresores

linearMod <- lm(clase ~ datos, data=trainSet)
print(linearMod)
summary(linearMod)


cuadraticMod <- lm(clase ~ datos + I(datos^2), data=trainSet)
print(cuadraticMod)
summary(cuadraticMod)
cuadraticMod$fitted.values
#Hemos encontrado que el método cuadrático funciona algo mejor, 
#vamos a probarlo con cross validation.

model <- train(
  clase ~ datos + I(datos^2), trainSet,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

print(model)
testSet$clase=NULL

testSet$prediction<-predict(model, testSet)
test

testSet$prediction<-ifelse(testSet$prediction<0,0,testSet$prediction)

