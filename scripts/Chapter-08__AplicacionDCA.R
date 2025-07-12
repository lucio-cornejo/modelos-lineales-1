#
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Datos
data(InsectSprays)
head(InsectSprays)

# Estructura de las variables
str(InsectSprays)

# Tabla de frecuencias por spray
table(InsectSprays$spray)


# Resumen general
summary(InsectSprays)

# Media y desviación estándar por grupo
library(dplyr)

InsectSprays %>%
  group_by(spray) %>%
  summarise(
    n = n(),
    media = mean(count),
    sd = sd(count),
    min = min(count),
    max = max(count)
  )


library(ggplot2)

ggplot(InsectSprays, aes(x = spray, y = count)) +
  geom_boxplot() +
  labs(title = "Conteo de insectos por tipo de spray",
       x = "Tipo de Spray",
       y = "Número de insectos") 


# Ajustar modelo de ANOVA de un factor
#############################
# H0: no hay diferencia entre los tipos de spray
modelo <- aov(count ~ spray, data = InsectSprays)
summary(modelo)
# diferencias significativas entre al menos dos tipos de spray


# Pruebas post hoc (Tukey)
#############################
# Prueba de comparaciones múltiples de Tukey
TukeyHSD(modelo)
# Esto indica qué pares de sprays son significativamente diferentes.


# Diagnóstico de supuestos del modelo
#############################
#Residuos vs valores ajustados)
plot(modelo, which = 1)

#Normalidad de residuos (Q-Q plot)
plot(modelo, which = 2)

# Prueba de normalidad de residuos
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas (prueba de Levene)
library(car)
leveneTest(count ~ spray, data = InsectSprays)


#No Normalidad de residuos y Homogeneidad de varianzas
#########################
#1. ANOVA paramétrico (basado en F) puede producir resultados no confiables
#2. Si n por grupo son similares y suficientemente grandes (n ≥ 30),
# el ANOVA es robusto ante cierta desviación de normalidad (TLC).
# Pero si las varianzas son muy heterogéneas y
# los tamaños de muestra son muy desiguales, sí es grave.


InsectSprays$log_count <- log(InsectSprays$count + 1) # +1 si hay ceros
modelo2 <- aov(log_count ~ spray, data = InsectSprays)
summary(modelo2)
plot(modelo2, which = 1)
plot(modelo2, which = 2)
shapiro.test(residuals(modelo2))

#Homogeneidad de varianzas (prueba de Levene)
library(car)
leveneTest(log_count ~ spray, data = InsectSprays)
