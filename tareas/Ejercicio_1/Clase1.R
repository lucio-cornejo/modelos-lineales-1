#########################################################################################
# Lectura de los datos y primeros gráficos
#setwd(<aquí ponen el nombre de la carpeta donde tienen sus datos)
SudAmerica <- read.csv("SudAmerica.csv", row.names = 1) # lectura del archivo eventualmente read.table con sus opciones
attach(SudAmerica)                                  # archivo en uso
lab        <- rownames(SudAmerica)                  # etiquetas de las unidades en lab 
plot(Tajo_urbano,IDH,xlab="Tajo_urbano",ylab="IDH") # diagrama de dispersión de dos carácteres
text(Tajo_urbano,IDH,lab)                           # escritura de etiquetas sobre el gráfico
plot(Tajo_urbano,IDH,xlab="Tajo_urbano",ylab="IDH") # otro diagrama
identify(Tajo_urbano,IDH,lab)                       # escritura de etiquetas cliqueando cadauna

#########################################################################################
# regresión lm dibujo de datos con recta y predictos
attach(SudAmerica)
plot(Tajo_urbano,IDH,xlab="Tajo_urbano",ylab="IDH") # otro diagrama
identify(Tajo_urbano,IDH,lab)                       # escritura de etiquetas cliqueando cadauna
lmSA       <- lm(IDH~Tajo_urbano,data=SudAmerica)   # regresión
abline(lmSA,col="red")                              # se traza la recta estimada                    
points(Tajo_urbano,lmSA$fitted.values,col="red")    # y los IDH estimados

# salida de resultados y graficós estándar
summary(lmSA)
plot(lmSA)
#########################################################################################