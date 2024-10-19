# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(VIM) # Para kNN Imputation

# Crear un dataset de ejemplo con valores faltantes
set.seed(123)
data <- data.frame(
    var1 = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10),
    var2 = c(10, 9, 8, 7, NA, 5, 4, 3, 2, NA),
    var3 = c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10)
)

# Mostrar los datos originales con valores faltantes
print("Datos originales con valores faltantes")
print(data)

# 1. Identificación de datos faltantes
#print("Número de valores faltantes por columna:")
print(colSums(is.na(data)))

# 2. Eliminar observaciones con valores faltantes
data_no_missing <- na.omit(data)
#print("Eliminar filas con valores faltantes:")
print(data_no_missing)

# 3. Imputación por la media
data_mean_imputed <- data %>%
    mutate(var1 = ifelse(is.na(var1), mean(var1, na.rm = TRUE), var1),
           var2 = ifelse(is.na(var2), mean(var2, na.rm = TRUE), var2),
           var3 = ifelse(is.na(var3), mean(var3, na.rm = TRUE), var3))
print("Imputación por la media:")
print(data_mean_imputed)

# 4. Imputación por la mediana
data_median_imputed <- data %>%
    mutate(var1 = ifelse(is.na(var1), median(var1, na.rm = TRUE), var1),
           var2 = ifelse(is.na(var2), median(var2, na.rm = TRUE), var2),
           var3 = ifelse(is.na(var3), median(var3, na.rm = TRUE), var3))
print("Imputación por la mediana:")
print(data_median_imputed)

# 5. Imputación por kNN (usando la librería VIM)
data_knn_imputed <- kNN(data, k = 3)
print("Imputación por kNN:")
print(data_knn_imputed)
# TRUE: Significa que el valor en esa fila y columna ha sido imputado por el algoritmo kNN.
# FALSE: Significa que el valor en esa fila y columna es el valor original que estaba presente en el dataset y no fue imputado.

# 6. Imputación por regresión
# Se usa la librería mice para imputaciones por regresión
library(mice)

# Crear un nuevo dataframe sin colinealidad y sin variables constantes
data_new <- data.frame(
  var1 = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10),      # Variable con valores faltantes
  var2 = c(9, 7, 6, 5, NA, 3, 2, 1, 4, NA),       # Diferente de var1 para evitar colinealidad
  var3 = c(15, 14, 13, 12, 11, NA, 9, 8, 7, NA)   # Nueva distribución sin colinealidad
)
# Ver el dataset original con valores faltantes
print("Dataset original con valores faltantes:")
print(data_new)

#Usamos la función mice() para imputar los valores faltantes usando regresión lineal predictiva (method = "norm.predict").
data_regression_imputed <- mice(data_new, method = "norm.predict", m = 1, maxit = 5, seed = 123)

#Luego, extraemos los datos completados con la función complete().
data_regression_imputed <- complete(data_regression_imputed)
print("Imputación por regresión:")
print(data_regression_imputed)


