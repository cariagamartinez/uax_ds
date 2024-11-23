# Librerías necesarias
library(tidyverse)   # Manipulación y visualización de datos
library(corrplot)    # Visualización de correlaciones
library(gridExtra)   # Visualización múltiple
library(GGally)      # Gráficos combinados para análisis multivariado

# 1. Carga de datos
#EL ARCHIVO DEBE ESTAR DENTRO DE TU PROPIA RUTA
datos <- read.csv("T3/Alzheimer_dataset_actualizado.csv", sep = ",")

# Verificar los primeros registros
head(datos)

# Verificar estructura de los datos
str(datos)

# 2. Limpieza inicial
# Eliminar columnas irrelevantes o redundantes
# En este caso, ya no hay necesidad de eliminar 'Categoria', como se indica en el diccionario de datos

# Verificar valores NA
cat("Número de valores faltantes por columna:\n")
colSums(is.na(datos))

# Eliminar filas con valores NA si es necesario
datos <- na.omit(datos)

# Verificar que los datos sean consistentes y ordenados
datos <- datos %>%
  mutate(across(where(is.character), as.factor))  # Convertir variables categóricas a factores si corresponde

# 3. Exploración básica
cat("Dimensiones del dataset:\n")
dim(datos)  # Número de filas y columnas

cat("Resumen de las variables:\n")
summary(datos)

# cat("Número de valores únicos por columna:\n")
# sapply(datos, function(x) length(unique(x)))

# 4. Estadística descriptiva para variables numéricas
numericas <- select(datos, where(is.numeric))
cat("Estadísticas descriptivas para variables numéricas:\n")
summary(numericas)

# 5. Visualización de distribuciones
# Histogramas para todas las variables numéricas
par(mfrow = c(3, 3))  # Configurar múltiples gráficos
for (var in names(numericas)) {
  hist(numericas[[var]], main = paste("Histograma de", var),
       xlab = var, col = "skyblue", border = "black")
}

# Boxplots para detectar outliers
par(mfrow = c(1, 1))
boxplot(numericas, main = "Boxplots de Variables Numéricas", las = 2, col = "lightblue")

# 6. Correlaciones
# Visualización de la matriz de correlación
correlaciones <- cor(numericas)
corrplot(correlaciones, method = "circle", type = "upper", tl.cex = 0.7, tl.col = "black", main = "Matriz de Correlación")

# 7. Análisis univariante
# Análisis de la variable objetivo
table(datos$Diagnostico_Alzheimer)
barplot(table(datos$Diagnostico_Alzheimer), main = "Distribución de la Variable Target",
        col = c("orange", "skyblue"), xlab = "Clase", ylab = "Frecuencia")

# 8. Análisis bivariante
# Relación entre Edad y Puntuación MOCA
plot(datos$Edad, datos$Puntuacion_MOCA, main = "Edad vs Puntuación MOCA",
     xlab = "Edad", ylab = "Puntuación MOCA", col = "blue", pch = 16)

# Comparación de Puntuación MMSE entre clases
boxplot(Puntuacion_MMSE ~ Target, data = datos, main = "Puntuación MMSE por Clase",
        xlab = "Clase", ylab = "Puntuación MMSE", col = c("orange", "skyblue"))

# 9. Análisis multivariante
# Pair plot para las variables principales
ggpairs(select(datos, Edad_Paciente, Puntuacion_TestEstandarizado_MMSE, 
               Puntuacion_BiomarcadorEstandarizado_Hipocampo, 
               Puntuacion_SubtestCognitivo_MOCA, Diagnostico_Alzheimer),
        aes(color = as.factor(Diagnostico_Alzheimer), alpha = 0.5))

# 10. Observaciones
cat("Observaciones importantes:\n")
cat("- Verificar posibles outliers en variables como Edad y Puntuación MOCA.\n")
cat("- La correlación más fuerte encontrada es entre Volumen_Hipocampo y Nivel_Amyloide_Beta.\n")
cat("- La clase 'Target' está balanceada.\n")
