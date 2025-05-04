#########################################################################################
# Lectura de los datos
#setwd(<aquí ponen el nombre de la carpeta donde tienen sus datos)
Linnerud <- read.csv("Linnerud.csv",row.names=1) # lectura del archivo eventualmente read.table con sus opciones
attach(Linnerud)                                 # archivo en uso
lab <- rownames(Linnerud)                        # etiquetas de las unidades en lab
########################################################################################
# regresión a mano
# definición de los datos para el modelo
n     <- dim(Linnerud)[1]; n                 # n es el número de observaciones
x     <- as.matrix(Linnerud[,1:3]); x        # x es el caracter descriptivo
p     <- dim(x)[2]; p                        # numero de regresores
y     <- Pulls                               # y es el caracter respuesta   
nomy  <- deparse(substitute(Pulls)); nomy    # nomy es su nombre
nom   <- c(colnames(x),nomy) ; nom           # etiquetas de las variables en nom
########################################################################################
# estadísticas
xm    <- apply(x,2,mean); xm                 # cálculo de xm, promedio de x
ym    <- sum(y)/n; ym                        # cálculo de ym, promedio de y
ssx   <- apply(x^2,2,sum); ssx               # ssx es la suma de los x cuadros
ssy   <- sum(y^2); ssy                       # ssy es la suma de los y cuadros
sxy   <- t(x)%*%y ; sxy                      # sxy es la suma de los productos xy
ssxc  <- ssx-n*xm^2 ; ssxc                   # ssxc es ssx centrado sobre el promedio
ssyc  <- ssy-n*ym^2 ; ssyc                   # ssyc es ssy centrado sobre el promedio
sxyc  <- sxy-n*xm*ym; sxyc                   # sxyc es sxy centrado sobre el promedio
varx  <- ssxc/n; varx                        # varx es la varianza de x
vary  <- ssyc/n; vary                        # vary es la varianza de y
covxy <- sxyc/n; covxy                       # covxy es la covarianza de xy
stats <- matrix(8*(2*p+1)*0,nrow=8,ncol=(2*p+1)) # construcción de tabla de estadísticas
rownames(stats) <- c("Mínimo","Máximo","Promedio","Total","Suma de cuadrados",
                    "Cuadrados centrados","Varianza covarianza","Desvío estándar")
colnames(stats) <- c(nom,"x1y","x2y","x3y")
stats[,1:p] <- rbind(apply(x,2,min),apply(x,2,max),xm,apply(x,2,sum),ssx,ssxc,varx,sqrt(varx))
stats[,p+1] <- rbind(min(y),max(y),ym,sum(y),ssy,ssyc,vary,sqrt(vary))
stats[,c((p+2):(2*p+1))] <- rbind(t(rep(0,p)),t(rep(0,p)),t(rep(0,p)),t(rep(0,p)),t(sxy),t(sxyc),t(covxy),t(rep(0,p)))
stats                                        # impresión de la tabla
print("Matriz de varianza y covarianza de los regresores")
vcovx <- (n-1)/n * cov(x) ; vcovx            # matriz de varianza/covarianza de los x
#########################################################################
# estimación
X      <- cbind(rep(1,n),x); X               # se necesita ajuntar la columna de unos
p1     <- p + 1; p1                          # un regresor más
XX     <- t(X)%*%X; XX                       # matriz X'X
XXm1   <- solve(XX); XXm1                    # su inversa
bh     <- XXm1 %*% t(X) %*% y                # estimación de los betas
print("Estimaciones de los beta")
bh 
eta    <- X %*% bh; eta                      # valores estimados
res    <- y - eta; res                       # residuos
unidad <- cbind(x,y,eta,res)                 # tabla de resultados
rownames(unidad) <- lab
colnames(unidad) <- c(nom,"eta","residuos")
unidad                                       # impresión de la tabla
###########################################################################
# gráficos clásicos: ejemplo con el primero regresor
plot(x[,1],y,xlab="x",ylab="y",ylim=c(min(y,eta),max(y,eta)),pch=".")
  text(x[,1],y,labels=lab,pos=2)
  points(x[,1],eta,col="red",pch=".")
  text(x[,1],eta,labels=lab,col="red",pos=2)
  dev.copy(pdf,"01-datos1-reg.pdf")          # copia en un archivo pdf
dev.off()                                    # fin de la copia
###########################################################################
# gráficos de valores estimados y residuos
plot(eta,y,asp=1,xlab="estimados",ylab="observados")
  abline(0,1,col="red")
  text(eta,y,labels=lab,pos=2)
  points(eta,eta,col="red")
  dev.copy(pdf,"02-est-obs.pdf")             # copia en un archivo pdf
dev.off()                                    # fin de la copia
plot(eta,res,asp=1,xlab="estimados",ylab="residuos")
  abline(0,0,col="red")
  text(eta,res,labels=lab,pos=2)
  dev.copy(pdf,"03-est-res.pdf")             # copia en un archivo pdf
dev.off()                                    # fin de la copia
###################################################################################
# datos para anova
ssr   <- t(eta) %*% eta; ssr       # suma de  cuadrados por regresión
sse   <- ssy - ssr; sse            # suma por residuos
msr   <- ssr/p1; msr               # cuadrado promedio por regresión
mse   <- sse/(n-p1); mse           # cuadrado promedio por residuos
df    <- c(p1,n-p1,n)              # grados de libertad
ssq   <- c(ssr,sse,ssy)            # sumas de cuadrados
msq   <- c(msr,mse,0)              # suma de residuos
anova <- (cbind(df,ssq,msq))       # tabla de anova
colnames(anova) <- c("Degrees of Freedom","Sum of squares","Mean squares")
rownames(anova) <- c("Regression","Residuals","Total")
anova
#####################################################################################
# apalancamiento
Hat   <- X %*% XXm1 %*% t(X)                  # matriz H sombrero
rownames(Hat) <- lab                          # inclusivo de su etiquetas
colnames(Hat) <- lab
Hat
lev   <- diag(Hat); lev                       # apalancamientos
pesos <- cbind(x,y,lev)                       # construcción de una salida conjunta
rownames(pesos) <- lab
pesos                                         # salida
niv   <- 2 * p1 / n; niv                      # nivel para apalancamiento
level <- which(lev>niv)                       # unidades con fuerte apalancamiento
out   <- rbind(level,lev[level])              # salida
rownames(out) <- c("unidad","apalancamiento"); out
###################################################################################
# comparación entre modelo total y modelo sin unidades con apalancamiento
# ejemplo sobre Linnerud
# modelo completo
lm1 <- lm(y~Weight+Waist+Pulse,data=Linnerud); summary(lm1) # regresión con y sin
# y su plot estimados observados
plot(lm1$fitted.values,y,pch=20,xlab="estimados",ylab="observados")
  abline(0,1,col="red")
  text(lm1$fitted.values,y,labels=lab,pos=3,offset(0.5))
  points(lm1$fitted.values,lm1$fitted.values,pch=20,col="red")
  dev.copy(pdf,"04-lin-est-obs.pdf")           # copia en un archivo pdf
dev.off()                                      # fin de la copia
# y su plot estimados residuos
plot(lm1$fitted.values,lm1$residuals,pch=20,xlab="estimados",ylab="residuos")
  abline(0,0,col="red")
  text(lm1$fitted.values,lm1$residuals,labels=lab,pos=3,offset(0.5))
  dev.copy(pdf,"05-lin-est-res.pdf")           # copia en un archivo pdf
dev.off()                                      # fin de la copia
# modelo sin unidades con fuerte apalancamiento
lm2 <- lm(y[-level]~Weight+Waist+Pulse,data=Linnerud[-level,]); summary(lm2)
# hay que tirar las unidades también de la representación gráfica
plot(lm2$fitted.values,y[-level],pch=20,asp=1,xlab="estimados",ylab="observados")
  abline(0,1,col="red")
  text(lm2$fitted.values,y[-level],labels=lab,pos=3,offset(0.5))
  points(lm2$fitted.values,lm2$fitted.values,pch=20,col="red")
  dev.copy(pdf,"06-est-obs2.pdf")            # copia en un archivo pdf
dev.off()                                    # fin de la copia
plot(lm2$fitted.values,lm2$residuals,asp=1,pch=20,xlab="estimados",ylab="residuos")
  abline(0,0,col="red")
  text(lm2$fitted.values,lm2$residuals,labels=lab,pos=3,offset(0.5))
  dev.copy(pdf,"07-est-res2.pdf")            # copia en un archivo pdf
dev.off()                                    # fin de la copia
# gráficos de comparación de estimados y residuos
# ambos contra y y no contra los estimados
plot(y,lm1$fitted.values,pch=20,asp=1,ylab="fitted values")
  abline(0,1)
  text(y,lm1$fitted.values,y,labels=lab,pos=4,offset(0.5))
  points(y[-out[1,]],lm2$fitted.values,pch=20,col="red")
  text(y[-out[1,]],lm2$fitted.values,labels=lab[-out[1,]],col="red",pos=3,offset(0.5))
  dev.copy(pdf,"08-lin-fit-apalancamiento.pdf")
dev.off()
# por esto hay que cambiar el signo a los residuos
plot(y,-lm1$residuals,pch=20,asp=1,ylab="residuals")
  abline(h=0)
  text(y,-lm1$residuals,labels=lab,pch=20,pos=4,offset(0.5))
  points(y[-out[1,]],-lm2$residuals,pch=20,col="red")
  text(y[-out[1,]],-lm2$residuals,labels=lab[-out[1,]],col="red",pos=3,offset(0.5))
  dev.copy(pdf,"09-lin-res-apalancamiento.pdf")
dev.off()
##########################################################################