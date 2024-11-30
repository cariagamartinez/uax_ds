# Cargar librerías necesarias
library(tidyverse)   # Para manipulación de datos
library(caret)       # Para modelos y validación cruzada
library(class)       # Para K-NN

# 1. Lectura del dataset
# Asegúrate de guardar el archivo como 'Alzheimer_dataset.csv' en el mismo directorio que el script.
datos <- read.csv("T2/Alzheimer_dataset.csv", sep = ";")

# Eliminar la columna 'Categoria' asignándole NULL
datos$Categoria <- NULL

# Exploración inicial
str(datos)  # Estructura del dataset
summary(datos)  # Resumen estadístico

# 2. Análisis exploratorio (EDA)
# Comprobar distribución de la variable objetivo (Target)
table(datos$Target) #TENEMOS QUE TRABAJAR CON TABLE PORQUE ES UNA VARIABLE CATEGÓRICA

#¿Están balanceadas las categorías?
barplot(table(datos$Target), main = "Distribución de la Variable Target",
        xlab = "Clase", ylab = "Frecuencia", col = c("skyblue", "orange"))

# Visualización de correlaciones entre variables numéricas
numericas <- select(datos, where(is.numeric))
correlaciones <- cor(numericas)
corrplot::corrplot(correlaciones, method = "circle", main = "Matriz de Correlación")

# Visualizar la distribución de algunas variables
# Edad por clases de Target
hist(datos$Edad[datos$Target == 0], breaks = 15, col = "lightblue", main = "Distribución de Edad (Clase 0)",
     xlab = "Edad", ylab = "Frecuencia")
hist(datos$Edad[datos$Target == 1], breaks = 15, col = "pink", main = "Distribución de Edad (Clase 1)",
     xlab = "Edad", ylab = "Frecuencia")

# Boxplot para una variable como ejemplo
boxplot(Edad ~ Target, data = datos, main = "Boxplot de Edad por Clase",
        xlab = "Clase", ylab = "Edad", col = c("lightblue", "pink"))

#Otras forma de hacer boxplots
boxplot(datos$Edad, main = "Boxplot de Edad",
        ylab = "Edad", col = "lightblue")

boxplot(datos$Edad[datos$Target == 0], datos$Edad[datos$Target == 1],
        main = "Boxplot de Edad por Clase",
        names = c("Clase 0", "Clase 1"),
        col = c("lightblue", "pink"),
        ylab = "Edad")

boxplot(datos[, c("Edad", "Tiempo_Reaccion_1", "Medicion_1")],
        main = "Boxplot de Variables",
        col = c("lightblue", "pink", "lightgreen"),
        names = c("Edad", "Tº reacción", "Medición 1"),
        ylab = "Valores")

#Visualización avanzada con la librería GGPLOT2
ggplot(datos, aes(x = Edad, fill = as.factor(Target))) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de Edad por Clase", x = "Edad", fill = "Clase")

# 3. Preparación de los datos
# Convertir la variable Target a factor (necesario para clasificación)
datos$Target <- as.factor(datos$Target)

# Dividir los datos en conjunto de entrenamiento (70%) y prueba (30%)
set.seed(123)
trainIndex <- createDataPartition(datos$Target, p = 0.7, list = FALSE)
train <- datos[trainIndex, ]
test <- datos[-trainIndex, ]

# Escalar las variables numéricas (recomendado para K-NN)
escala <- preProcess(train, method = c("center", "scale"))
train_scaled <- predict(escala, train)
test_scaled <- predict(escala, test)

# Eliminar filas con valores NA tras el escalado
train_scaled <- na.omit(train_scaled)
test_scaled <- na.omit(test_scaled)

# 4. Modelo de K-NN
# Entrenar un modelo K-NN con k=5
set.seed(123)
knn_pred <- knn(train = train_scaled[, -which(names(train_scaled) == "Target")],
                test = test_scaled[, -which(names(test_scaled) == "Target")],
                cl = train_scaled$Target, k = 5)

# Evaluar el modelo K-NN
confusion_knn <- confusionMatrix(knn_pred, test_scaled$Target)
print(confusion_knn)

# 5. Modelo de Regresión Logística
# Entrenar un modelo de regresión logística
logistico <- glm(Target ~ ., data = train, family = "binomial")

# Resumen del modelo
summary(logistico)

# Predicción con regresión logística
logistico_pred <- predict(logistico, newdata = test, type = "response")
logistico_clases <- ifelse(logistico_pred > 0.5, 1, 0)
logistico_clases <- as.factor(logistico_clases)

# Evaluar el modelo de regresión logística
confusion_log <- confusionMatrix(logistico_clases, test$Target)
print(confusion_log)

# 6. Conclusión
# Comparar métricas clave entre K-NN y regresión logística
cat("K-NN - Precisión:", confusion_knn$overall["Accuracy"], "\n")
cat("Regresión Logística - Precisión:", confusion_log$overall["Accuracy"], "\n")
