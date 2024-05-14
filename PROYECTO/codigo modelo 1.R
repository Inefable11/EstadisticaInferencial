datos <- read.csv("D:/MAESTRÍA EN BIG DATA Y DATA ANALYTICS/Análisis e interpretación de datos 24-01-2023/ACTIVIDAD 3/Sleep_health_and_lifestyle_dataset.csv")
install.packages("readxl")
head(datos)

library(ggplot2)
install.packages("psych")
library(psych)
install.packages("BSDA")
summary(datos$Sleep.Duration)

ggplot(datos, aes(x = "", y = Sleep.Duration)) +
  geom_boxplot() +
  labs(title = "Distribución de Horas de Sueño", y = "Horas de Sueño por Noche") +
  theme_minimal()

describe(datos$Sleep.Duration)

ggplot(datos, aes(x = Sleep.Duration)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribución de Horas de Sueño", x = "Horas de Sueño por Noche", y = "Frecuencia") +
  theme_minimal()

hipotesis_media <- 7  # Media nacional
resultado_prueba <- t.test(datos$Sleep.Duration, mu = hipotesis_media)

# Visualización del resultado de la prueba
print(resultado_prueba)

# Interpretación
if (resultado_prueba$p.value < 0.05) {
  cat("La diferencia en las horas de sueño es estadísticamente significativa.\n")
} else {
  cat("No hay evidencia significativa para afirmar que la media de horas de sueño es diferente de la media nacional.\n")
}