# Cargamos las librerías necesarias
library(tidyverse)  # Para manipulación de datos y visualización
library(caret)      # Para evaluación del modelo

# 1. Lectura del dataset
datos <- read.csv("Session_Workshop/dataset_session_biomedicina.csv")

# Exploración inicial del dataset
str(datos)  # Estructura del dataset
summary(datos)  # Resumen estadístico de las variables

# 2. Análisis Exploratorio de Datos (EDA)
# Distribución de la variable objetivo (Riesgo_Cardiovascular)
hist(datos$Riesgo_Cardiovascular, main = "Distribución del Riesgo Cardiovascular",
     xlab = "Riesgo Cardiovascular", ylab = "Frecuencia", col = "skyblue", border = "black")

# Relación entre Edad e IMC con el Riesgo Cardiovascular
plot(datos$Edad, datos$Riesgo_Cardiovascular, main = "Edad vs Riesgo Cardiovascular",
     xlab = "Edad", ylab = "Riesgo Cardiovascular", pch = 19, col = "blue")
plot(datos$IMC, datos$Riesgo_Cardiovascular, main = "IMC vs Riesgo Cardiovascular",
     xlab = "IMC", ylab = "Riesgo Cardiovascular", pch = 19, col = "red")

# Conversión de variables categóricas a factores para el modelo
datos$Actividad_Fisica <- as.factor(datos$Actividad_Fisica)
datos$Fumador <- as.factor(datos$Fumador)

# Separación de los datos en conjuntos de entrenamiento y prueba
set.seed(123)
index <- createDataPartition(datos$Riesgo_Cardiovascular, p = 0.7, list = FALSE)
train_data <- datos[index, ]
test_data <- datos[-index, ]

# 3. Generación de un modelo de regresión lineal
# Definimos el modelo lineal para predecir el Riesgo Cardiovascular
modelo <- lm(Riesgo_Cardiovascular ~ Edad + IMC + Nivel_Glucosa + Nivel_Colesterol +
               Actividad_Fisica + Fumador, data = train_data)

# Resumen del modelo
summary(modelo)

# Utilizamos plot(modelo) para generar los gráficos de diagnóstico del modelo
par(mfrow = c(2, 2))  # Configura el espacio de gráficos para mostrar 4 gráficos: 2 filas x 2 columnas
plot(modelo)
par(mfrow = c(1, 1))  # Restablece la configuración de gráficos

# 4. Evaluación del modelo
# Predicción en los datos de entrenamiento
predicciones <- predict(modelo, newdata = test_data)

# Cálculo del error
rmse <- sqrt(mean((test_data$Riesgo_Cardiovascular - predicciones)^2))
cat("Error RMSE del modelo:", rmse, "\n")

# 5. Interpretación y conclusión
# Basándonos en los resultados del modelo, observamos que:
# - La Edad, el Nivel de Glucosa y el Nivel de Colesterol parecen tener un efecto positivo en el Riesgo Cardiovascular,
#   mientras que el IMC tiene un efecto negativo, lo cual podría requerir un análisis más profundo.
# - Las variables categóricas como el nivel de actividad física y el hábito de fumar también pueden influir en el riesgo aunque no de 
# manera estadísticamente significativa así que habría que estudiarlas mejor.
# Este análisis puede ayudar a identificar factores de riesgo y su contribución relativa, proporcionando una base para intervenciones
# preventivas y promoción de la salud en la población de estudio.
