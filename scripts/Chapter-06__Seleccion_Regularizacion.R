# Librerias
library(ISLR)
library(ggplot2)

# Leer los datos
data("Hitters")
names(Hitters) # nombre de las variables
str(Hitters)
head(Hitters)
help(Hitters)
dim(Hitters)

# Eliminando datos perdidos
library(dplyr)
Hitters = Hitters %>%
  na.omit

## Mejores Subconjuntos
#######################
library(leaps)

# Identificar el mejor modelo para un determinado numero de predictores
# (por defecto se muestra subconjuntos hasta el tamanho 8)
regfit.full=regsubsets(Salary~.,Hitters)
sum8 = summary(regfit.full)
sum8

# Para visualizar y evaluar cual subconjunto es mejor
summary(regfit.full)$adjr2   # R^2 ajustado
summary(regfit.full)$bic     # Criterio de información bayesiano
summary(regfit.full)$cp      # Estadístico Cp de Mallows

which.min(sum8$bic) # Mejor modelo en base a BIC
which.max(sum8$adjr2) # Mejor modelo en base a R2adj

# El mejor modelo basado en BIC es 6
mod6 <- which.min(sum8$bic)
sum8$which[mod6, ]

# Covariables del modelo 6 basado en BIC
vars6 <- names(which(sum8$which[mod6, ] == TRUE))

# Incrementando el tamaho hasta 19
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
reg.summary
names(reg.summary)

#  R2 para cada tamanho de subconjunto
reg.summary$rsq


# Grafica para los R2 (no apropiado para seleccionar modelos)
library(ggvis)
rsq <- as.data.frame(reg.summary$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "N?mero de Variables")




# Grafica comparativa para la RSS, el R2 ajustado, Cp y BIC
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Numero de Variables",ylab="RSS",type="l")

plot(reg.summary$adjr2,xlab="N?mero de Variables",ylab="R2 Ajustado",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Numero de Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

plot(reg.summary$bic,xlab="Numero de Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
par(mfrow=c(1,1))


# Variables seleccionadas para el mejor modelo 
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# Coeficientes asociados al mejor modelo con 6 variables segun BIC
coef(regfit.full,6)

# El mejor modelo basado en BIC
modbic <- which.min(reg.summary$bic)
reg.summary$which[modbic, ]

# Covariables del modelo 6 basado en BIC
varsbic <- names(which(reg.summary$which[modbic, ] == TRUE))


# Seleccion paso a paso hacia adelante
######################################

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
# Mejor modelo segun el Cp
plot(regfit.fwd, scale = "Cp")

# Seleccion paso a paso hacia atras
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

# Comparando el mejor modelo con 6 variables
coef(regfit.full,6)
coef(regfit.fwd,6)
coef(regfit.bwd,6)

# Comparando el mejor modelo con 7 variables
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)




# Seleccion de modelos usando el esquema de validacion  (retencion) 
###################################################################
library(dplyr)
Hitters = Hitters %>%
  na.omit
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train) # test ---> validation

# Mejores subconjuntos en los datos de entrenamiento
regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax=19)

# Matriz de disenho para las regresiones
# (regsubsets no tiene metodo predict)
test.mat=model.matrix(Salary~.,data=Hitters[test,])

# Calcular el error en el conjunto de validacion
val.errors=rep(NA,19)

# iterar para cada tama?o i
for(i in 1:19){
  # Extraer el vector de predictores del mejor modelo con i predictores
  coefi=coef(regfit.best,id=i)
  
  # Realiza las predicciones multplicando los coeficientes por la matriz de dise?o 
  pred=test.mat[,names(coefi)]%*%coefi
  
  # Calculo del MSE
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors

# Encontrar el modelo con el menor MSE 
min = which.min(val.errors)
min
coef(regfit.best,min)






###################################################################
# Regularizacion
###################################################################
x=model.matrix(Salary~.,Hitters)[,-1] # eliminar la primera columna
# dejando solo los predictores
y=Hitters$Salary

# Regresion Ridge                                                           
###################
library(glmnet)

# Busqueda de valores para lambda (desde 10^10 hasta 10^-2 )
grid=10^seq(10,-2,length=100)

# Ajustar la regresion ridge
# alpha=0 indica regresión ridge; los predictores son estandarizados por defecto.
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # por defecto standardize = TRUE

# Dimension de la matriz que almacena los coeficientes de la regresion ridge
dim(coef(ridge.mod)) #Una fila por cada predictor y una colunma por valor de lambda

# Grafica de los coeficientes 
plot(ridge.mod,  xvar = "lambda", label = TRUE)      


# Se espera que los coeficientes se contraigan (tiendan a cero)
# a medida que lambda se incrementa.
ridge.mod$lambda[50] # Mostrar el valor 50 de lambda
coef(ridge.mod)[,50] # Mostrar los coeficientes asociados con el valor 50 de lambda
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # Calcular la norma L2 (suma de cuadrados)

# Comparando con el valor 60
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# Los coeficientes para lambda = 50.
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# Dividiendo el conjunto de datos para estimar el error de test
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Estimamos la regresion ridge en el conjunto de entrenamiento 
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)

# Evaluamos el MSE en el conjunto de test para un lambda = 4
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Comparando con el MSE del modelo nulo (media)
mean((mean(y[train])-y.test)^2)

# Esto se aproxima a la predicción del modelo nulo cuando lambda tiende infinito.
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Comparando con MCO que equivale a ridge con lambda = 0.
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]


# Realizando la validacion cruzada para seleccionar el valor de lambda
set.seed(1)

# Ajustar la regresion ridge a los datos de entrenamiento
cv.out=cv.glmnet(x[train,],y[train],alpha=0) 
bestlam=cv.out$lambda.min # elegir el valor de lambda que minimiza el MSE
bestlam

# Graficando el MSE por cv en funcion de lambda
plot(cv.out)

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Modelo final (con todos los datos)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# Nota: Para elegir el valor de lambda segun la regla de 1 error estandar
# lambda.1se corresponde al valor de lambda más grande cuyo error
# de validación está dentro de 1 error estándar del mínimo.
cv.out$lambda.1se 




###################################################################
# Regresion Lasso
###################################################################

# Ajustar el modelo a los datos de entrenamiento
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod, xvar = "lambda", label = TRUE) 

# Realizando la validacion cruzada para seleccionar el valor de lambda
set.seed(1)

# Ajustar el modelo a los datos de entrenamiento 
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

# Seleccionar el valor de lambda que minimiza el MSE
bestlam=cv.out$lambda.min
bestlam

# Usar el valor de lambda para predecir los datos de test
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

# MSE de test
mean((lasso.pred-y.test)^2)

# Estimacion del modelo final (todos los datos)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

# Coeficientes distintos de 0
lasso.coef[lasso.coef!=0]
