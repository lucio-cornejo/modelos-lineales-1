# Cargar librería necesaria
library(MASS)  # Contiene la función rlm()

# Datos de ejemplo: relación entre peso y altura de un grupo de estudiantes
# Incluimos un valor atípico para mostrar la robustez
altura <- c(160, 165, 170, 175, 180, 185, 190, 200)
peso <- c(55, 60, 65, 70, 75, 80, 85, 150)  # 150 es un outlier

# Ajuste usando Mínimos Cuadrados Ordinarios (OLS)
modelo_ols <- lm(peso ~ altura)
summary(modelo_ols)

# Ajuste usando Regresión Robusta (Huber M-estimator)
modelo_robusto <- rlm(peso ~ altura)
summary(modelo_robusto)

# Comparar coeficientes
coef(modelo_ols)
coef(modelo_robusto)

# Graficar resultados
plot(altura, peso, pch = 19, col = "black", main = "Comparación OLS vs Regresión Robusta")
abline(modelo_ols, col = "red", lwd = 2)     # Recta OLS
abline(modelo_robusto, col = "blue", lwd = 2) # Recta Robusta
legend("topleft", legend = c("OLS", "Robusta"), col = c("red", "blue"), lwd = 2)





#### Aplicacion
#####################################
# Cargar la base
data(stackloss)

# Ver resumen
head(stackloss)

# Ajuste OLS
ols <- lm(stack.loss ~ ., data = stackloss)

# Ajuste robusto Huber
library(MASS)
robust <- rlm(stack.loss ~ ., data = stackloss)

summary(ols)
summary(robust)


# Residuos OLS y robustos
stackloss$res_ols <- residuals(ols)
stackloss$res_robust <- residuals(robust)




# 1) Gráfico de dispersión: stack.loss vs Air.Flow
plot(stackloss$Air.Flow, stackloss$stack.loss,
     pch = 19, col = "black",
     main = "Stack Loss vs Air Flow",
     xlab = "Air Flow",
     ylab = "Stack Loss")
abline(lm(stack.loss ~ Air.Flow, data = stackloss), col = "red", lwd = 2)
abline(rlm(stack.loss ~ Air.Flow, data = stackloss), col = "blue", lwd = 2)
legend("topleft",
       legend = c("OLS", "Robusta"),
       col = c("red", "blue"),
       lwd = 2)



# 2) Gráfico de residuos OLS
plot(ols$fitted.values, stackloss$res_ols,
     main = "Residuos OLS",
     xlab = "Valores ajustados OLS",
     ylab = "Residuos",
     pch = 19)
abline(h = 0, col = "red")
# Indicar observaciones con residuos grandes (e.g., > 2 std)
outliers <- which(abs(stackloss$res_ols) > 2.5)
points(ols$fitted.values[outliers], stackloss$res_ols[outliers],
       col = "red", pch = 19)




# 3) Gráfico de residuos robustos
plot(fitted(robust), stackloss$res_robust,
     main = "Residuos Regresión Robusta",
     xlab = "Valores ajustados Robustos",
     ylab = "Residuos",
     pch = 19)
abline(h = 0, col = "blue")
# Indicar observaciones con residuos grandes (e.g., > 2.5 std)
outliers <- which(abs(stackloss$res_robust) > 2.5)
points(robust$fitted.values[outliers], stackloss$res_robust[outliers],
       col = "red", pch = 19)





#### Ejercicio
#####################################
library(carData)
data(Prestige)

head(Prestige)
