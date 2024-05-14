datos <- read.csv("D:/MAESTRÍA EN BIG DATA Y DATA ANALYTICS/Análisis e interpretación de datos 24-01-2023/ACTIVIDAD 3/calificaciones.csv")
install.packages("readxl")
head(datos)

library(ggplot2)
install.packages("psych")
library(psych)
install.packages("BSDA")
library(gridExtra)
install.packages("gridExtra")
desc_stats_previo <- summary(datos$ExitoPrevio)
desc_stats_actual <- summary(datos$ExitoActual)

# Visualizamos las estadísticas descriptivas
print("Estadísticas Descriptivas para ExitoPrevio:")
print(desc_stats_previo)

print("Estadísticas Descriptivas para ExitoActual:")
print(desc_stats_actual)

# Agrupar por Escuela y Método y calcular las estadísticas descriptivas
stats_previo <- aggregate(ExitoPrevio ~ Escuela + Metodo, data = datos, summary)
stats_actual <- aggregate(ExitoActual ~ Escuela + Metodo, data = datos, summary)

# Imprimir las estadísticas descriptivas
print("Estadísticas Descriptivas para ExitoPrevio:")
print(stats_previo)

print("Estadísticas Descriptivas para ExitoActual:")
print(stats_actual)

promedios <- aggregate(cbind(ExitoPrevio, ExitoActual) ~ Metodo + Escuela, data = datos, FUN = mean)

# Crear un gráfico de barras
ggplot(promedios, aes(x = Metodo, y = ExitoPrevio, fill = Escuela)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_bar(aes(y = ExitoActual), stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7) +
  labs(title = "Comparación de Métodos de Enseñanza",
       x = "Método",
       y = "Promedio de Éxito") +
  scale_fill_manual(values = c("Mi pequeño gigante" = "blue", "Santa Rita" = "red")) +
  theme_minimal()


ggplot(datos, aes(x = Metodo, y = ExitoPrevio, fill = Escuela)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Comparación de Métodos de Enseñanza",
       x = "Método",
       y = "Promedio de Éxito Previo") +
  scale_fill_manual(values = c("Mi pequeño gigante" = "blue", "Santa Rita" = "red")) +
  theme_minimal()
library(ggplot2)

ggplot(datos, aes(x = Metodo, fill = Escuela)) +
  geom_bar(aes(y = ExitoPrevio), stat = "summary", fun = "mean", position = "dodge", color = "black") +
  geom_bar(aes(y = ExitoActual), stat = "summary", fun = "mean", position = "dodge", color = "white", fill = "grey") +
  labs(title = "Comparación de Métodos de Enseñanza",
       x = "Método",
       y = "Promedio de Éxito") +
  scale_fill_manual(values = c("Mi pequeño gigante" = "blue", "Santa Rita" = "red")) +
  theme_minimal()


grafico_previo <- ggplot(datos, aes(x = Metodo, y = ExitoPrevio, fill = Escuela)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  labs(title = "Comparación de Métodos de Enseñanza",
       x = "Método",
       y = "Promedio de Éxito Previo") +
  scale_fill_manual(values = c("Mi pequeño gigante" = "blue", "Santa Rita" = "red")) +
  theme_minimal()

# Crear gráfico de barras para Éxito Actual
grafico_actual <- ggplot(datos, aes(x = Metodo, y = ExitoActual, fill = Escuela)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  labs(x = "Método",
       y = "Promedio de Éxito Actual") +
  theme_minimal()

# Combinar los gráficos

grid.arrange(grafico_previo, grafico_actual, ncol = 2)


resultado_prueba <- t.test(ExitoPrevio ~ Metodo, data = datos)
print(resultado_prueba)

# Visualización del resultado de la prueba
boxplot(ExitoPrevio ~ Metodo, data = datos, main = "Comparación de Éxito Previo por Método")


resultado_prueba <- t.test(ExitoPrevio ~ Metodo, data = datos)
print(resultado_prueba)

resultado_prueba_previo <- t.test(ExitoPrevio ~ Metodo, data = datos)
print(resultado_prueba_previo)
datos$Mejora <- datos$ExitoActual - datos$ExitoPrevio

# Prueba t para la diferencia de medias en la mejora
resultado_prueba_mejora <- t.test(Mejora ~ Metodo, data = datos)
print(resultado_prueba_mejora)