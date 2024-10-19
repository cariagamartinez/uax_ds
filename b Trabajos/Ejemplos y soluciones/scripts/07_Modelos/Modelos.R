# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(randomForest)

# Cargar los datasets
classification_data <- read.csv("data/classification_dataset.csv")
regression_data <- read.csv("data/regression_dataset.csv")

# Exploración de los datos (EDA) para el dataset de clasificación
cat("Resumen del dataset de clasificación:\n")
summary(classification_data)

cat("\nDistribución de la variable target (clasificación):\n")
table(classification_data$Target)

# Exploración de los datos (EDA) para el dataset de regresión
cat("\nResumen del dataset de regresión:\n")
summary(regression_data)

# Preprocesamiento de los datos
classification_data$Feature3 <- as.factor(classification_data$Feature3)
classification_data$Target <- as.factor(classification_data$Target)

# Dividir el dataset de clasificación en entrenamiento y prueba
set.seed(42)
trainIndex_class <- createDataPartition(classification_data$Target, p = .8, 
                                        list = FALSE, 
                                        times = 1)
train_class <- classification_data[ trainIndex_class,]
test_class  <- classification_data[-trainIndex_class,]

# Modelos de Clasificación

# Modelo 1: Regresión Logística
logistic_model <- train(Target ~ ., data = train_class, method = "glm", family = "binomial")
logistic_pred <- predict(logistic_model, test_class)
confusionMatrix(logistic_pred, test_class$Target)

# Modelo 2: Árbol de Decisión
tree_model <- train(Target ~ ., data = train_class, method = "rpart")
tree_pred <- predict(tree_model, test_class)
confusionMatrix(tree_pred, test_class$Target)

# Dividir el dataset de regresión en entrenamiento y prueba
set.seed(42)
trainIndex_reg <- createDataPartition(regression_data$Target, p = .8, 
                                      list = FALSE, 
                                      times = 1)
train_reg <- regression_data[ trainIndex_reg,]
test_reg  <- regression_data[-trainIndex_reg,]

# Modelos de Regresión

# Modelo 1: Regresión Lineal
lm_model <- train(Target ~ ., data = train_reg, method = "lm")
lm_pred <- predict(lm_model, test_reg)
postResample(lm_pred, test_reg$Target)

# Modelo 2: Random Forest
rf_model <- train(Target ~ ., data = train_reg, method = "rf")
rf_pred <- predict(rf_model, test_reg)
postResample(rf_pred, test_reg$Target)

# Fin del script
