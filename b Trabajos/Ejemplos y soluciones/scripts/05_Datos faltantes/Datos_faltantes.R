# Instalar paquetes necesarios
install.packages("naniar")
install.packages("BaylorEdPsych")
install.packages("mice")

# Cargar las librerías
library(naniar)
library(BaylorEdPsych)
library(mice)

# Cargar el dataset
data <- read.csv("path_to_your_dataset.csv")

# 1. Prueba de Little's MCAR test
# Este test evalúa si los datos faltantes son completamente al azar (MCAR)
little_test <- LittleMCAR(data)
print(little_test)

# 2. Visualización de los patrones de datos faltantes
# Gráficos para observar posibles patrones y relaciones
gg_miss_var(data) + theme_minimal()
vis_miss(data, cluster = TRUE)  # Muestra una matriz de patrones de faltantes

# 3. Regresión logística para evaluar MAR o MNAR
# Creamos una nueva variable indicadora de si hay valores faltantes en una columna
data$missing_salario <- ifelse(is.na(data$Salario), 1, 0)

# Ajustamos un modelo de regresión logística para ver si la probabilidad de un valor faltante
# depende de otras variables observadas (MAR)
modelo_mar <- glm(missing_salario ~ Edad + Genero + Departamento, family = binomial, data = data)
summary(modelo_mar)

# Si el modelo muestra que las variables predictoras son significativas, es un indicativo de MAR.
# Si no, podría tratarse de MCAR o MNAR (aunque MCAR ya se habría descartado con el test de Little).


Explicación de los pasos:
#1.	Little's MCAR Test: Se usa el paquete BaylorEdPsych para realizar el test de Little, que evalúa si los valores faltantes son completamente al azar (MCAR). Un p-valor bajo (generalmente < 0.05) sugiere que los datos no son MCAR, es decir, que hay algún patrón en los faltantes.
#2.	Visualización con naniar: Generamos gráficos para observar los patrones de valores faltantes. gg_miss_var()muestra la cantidad de valores faltantes en cada variable, mientras que vis_miss() puede agrupar las columnas por patrones similares de faltantes. Esto ayuda a detectar visualmente si los valores faltantes dependen de otras variables observadas (MAR).
#3.	Regresión logística para MAR o MNAR: Creamos una variable binaria que indica si un valor está faltante o no en una columna específica (por ejemplo, "Salario"). Luego ajustamos un modelo de regresión logística para predecir si los valores faltantes en esa columna dependen de otras variables observadas (MAR). Si el modelo es significativo, se puede inferir que los datos son MAR. Si no hay relación aparente, podría ser MNAR o MCAR.
#Conclusiones:
#•	Si el test de Little sugiere que los datos no son MCAR, y los modelos de regresión logística indican que los faltantes están asociados con otras variables, los datos son MAR.
#•	Si no hay relación aparente con otras variables observadas, los faltantes podrían ser MNAR.
Este enfoque te permitirá categorizar los tipos de faltantes y aplicar técnicas de imputación más adecuadas para cada situación.

