library(nlme)
library(ggplot2)
data(Orange)

# Ajustar modelos
modelo1 <- lm(circumference ~ age, data = Orange)
modelo2 <- gls(
  circumference ~ age,
  data = Orange,
  weights = varIdent(form = ~ 1 | Tree)
)

summary(modelo2)

# Varianzas por Árbol
library(ggplot2)
varianzas <- data.frame(
  Arbol = factor(c(1, 2, 3, 4, 5)),
  Varianza = c(1.000, 5.493, 1.177, 6.306, 2.531)
)
ggplot(varianzas, aes(x = Arbol, y = Varianza, fill = Arbol)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Varianzas Relativas por Árbol",
    x = "Árbol",
    y = "Varianza (vs Árbol 1)"
  )


# residuos y valores ajustados
datos_residuos <- data.frame(
  age = Orange$age,
  residuo1 = resid(modelo1),
  residuo2 = resid(modelo2),
  Tree = Orange$Tree
)

# Gráfico comparativo (ggplot2)
ggplot(datos_residuos, aes(x = age)) +
  geom_point(aes(y = residuo1, color = "MCO"), alpha = 0.6) +
  geom_point(aes(y = residuo2, color = "GLS"), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "MCO vs GLS con Matriz Diagonal",
    x = "Edad del árbol (días)",
    y = "Residuos",
    color = "Modelo"
  ) +
  scale_color_manual(values = c("red", "blue")) +
  facet_wrap(~ Tree, scales = "free_y")  # Facetas por árbol para claridad


# ¿En qué árboles la heterocedasticidad es más evidente?




## Comparación con MCO (homocedástico)
######################################
modelo_gls_homocedastico <- gls(
  circumference ~ age,
  data = Orange,
  method = "ML"
)

modelo_gls_heterocedastico <- gls(
  circumference ~ age,
  data = Orange,
  weights = varIdent(form = ~ 1 | Tree),
  method = "ML"
)

anova(modelo_gls_homocedastico, modelo_gls_heterocedastico)


# Tarea: Replicar el análisis excluyendo árboles 2 y 4. ¿Mejora el ajuste?





## Modelos AR(1) y varianza ponderada
######################################
library(nlme)
data(Orange)

# 1. Modelo WLS (varPower)
modelo1_wls <- gls(
  circumference ~ age,
  data = Orange,
  weights = varPower(form = ~ age),
  # varPower explanation:
  # \epsilon_i ~ N(0, \sigma^2 v_i^2); v_i = |x_i|^{\delta}
  # V(\epsilon_i) ) \sigma^2 |age_i|^{2\delta}
  method = "ML"
)

# 2. Modelo WLS (varPower) por tipo de arbol
modelo2_wls <- gls(
  circumference ~ age,
  data = Orange,
  weights = varPower(form = ~ age|Tree),
  method = "ML"
)

# 3. Modelo AR(1)
modelo1_ar1 <- gls(
  circumference ~ age,
  data = Orange,
  correlation = corAR1(form = ~ 1),
  method = "ML"
)

# 4. Modelo AR(1) por tipo de arbol
modelo2_ar1 <- gls(
  circumference ~ age,
  data = Orange,
  correlation = corAR1(form = ~ 1 | Tree),
  method = "ML"
)


# Comparación con MCO
modelo_mco <- lm(circumference ~ age, data = Orange, method = "ML")
anova(m_mco, m_wls1)
anova(m_mco, m_ar1)







##  Pruebas de heterocedasticidad y autocorrelación
####################################################

## heterocedasticidad
#####################

## Datos 1: cars
data(cars)

plot(
  cars$speed,
  cars$dist,
  xlab = "Velocidad (mph)",
  ylab = "Distancia de frenado (ft)",
  pch = 16,
  col = "blue"
)

## Modelo clasico MCO para Datos 1
modelo1_ols <- lm(dist ~ speed, data = cars)
summary(modelo1_ols)

## Prueba de heterocedasticidad (Breusch-Pagan) para Datos 1
y <- resid(modelo1_ols)**2
x <- cars$speed
mod0 <- lm(y ~ x)
n <- length(y)
BP <- n*summary(mod0)$r.squared
alpha <- 0.05
qchi <- qchisq(1-alpha,1) #X_1-alpha,k
# Se rechaza si
(BP > qchi)

# Usando libreria
library(lmtest)
bptest(modelo1_ols)



## Datos 2: Orange
data(Orange)
plot(
  Orange$age, Orange$circumference,
  xlab = "Edad del árbol",
  ylab = "Circunferencia",
  pch = 16,
  col = "blue"
)

## Modelo clasico MCO para Datos 2
modelo2_ols <- lm(circumference ~ age, data = Orange)
summary(modelo2_ols)

## test de Breusch-Pagan para Datos 1
library(lmtest)
bptest(modelo2_ols)

modelo1_wls <- gls(
  circumference ~ age,
  data = Orange,
  weights = varPower(form = ~ age),
  method = "ML"
)


#Visualización de los modelos
plot(
  Orange$age,
  Orange$circumference,
  xlab = "Edad del árbol",
  ylab = "Circunferencia",
  pch = 16, 
  col = "blue"
)
abline(modelo2_ols, col = "red", lwd = 2)          # Línea MCO
lines(Orange$age, predict(modelo1_wls), col = "darkgreen", lwd = 2)  # Línea GLS
legend("topleft", legend = c("MCO", "GLS WLS"), col = c("red", "darkgreen"), lwd = 2)



## autocorrelación AR
#####################

# Durbin-Watson para Datos 1
#H₀ (nula): Los residuos no están autocorrelacionados
modelo1_ols <- lm(dist ~ speed, data = cars)
acf(residuals(modelo1_ols))  # Si hay picos en lag=0 y lag=1, AR(1) es plausible.

library(lmtest)
lmtest::dwtest(modelo1_ols)

## Datos 2: Orange
data(Orange)
modelo2_ols <- lm(circumference ~ age, data = Orange)
summary(modelo2_ols)
acf(residuals(modelo2_ols))

library(lmtest)
lmtest::dwtest(modelo2_ols)


# Modelo AR(1)
modelo1_ar1 <- gls(circumference ~ age, data = Orange,
                   correlation = corAR1(form = ~ 1), method = "ML")

#Visualización de los modelos
plot(Orange$age, Orange$circumference, 
     xlab = "Edad del árbol",
     ylab = "Circunferencia",
     pch = 16, col = "blue")
abline(modelo2_ols, col = "red", lwd = 2)          # Línea MCO
lines(Orange$age, predict(modelo1_ar1), col = "darkgreen", lwd = 2)  # Línea GLS
legend("topleft", legend = c("MCO", "GLS AR1"), col = c("red", "darkgreen"), lwd = 2)









