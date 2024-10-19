# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape2)
library(psych)

# Cargar el dataset
df <- read.csv("data/dataset_intro.csv")

# 1. Revisión general de los datos
# Vemos la estructura de los datos: tipos de variables, primeras observaciones, y su dimensión.
str(df)  # Muestra el tipo de cada columna
head(df)  # Muestra las primeras filas del dataset
summary(df)  # Resumen estadístico de cada variable
psych::describe(df) #Paquete psych

# Explicación:
# 'str()' nos da una visión general sobre los tipos de datos de cada columna (numérica, categórica, etc.).
# 'head()' nos permite ver las primeras filas de los datos para entender cómo están estructurados.
# 'summary()' proporciona estadísticas clave como media, mediana, mínimo, máximo, etc., para columnas numéricas.
# Valores típicos que queremos obtener:
# Media (Mediana).
# Desviación estándar. 
# OJO: TÉTRADA DE ANSCOMBE.

#################################
# Cargar el dataset anscombe
data(anscombe)

# Ver los datos
head(anscombe)

# Graficar la tetrada de Anscombe
par(mfrow=c(2,2)) # Dividir el área gráfica en 2 filas y 2 columnas
for(i in 1:4) {
  plot(anscombe[, i], anscombe[, i+4], main=paste("Dataset", i),
       xlab="X", ylab="Y", pch=16, col="blue")
  abline(lm(anscombe[, i+4] ~ anscombe[, i]), col="red") # Agregar línea de regresión
}

######################################

# 2. Análisis de valores faltantes
# Comprobamos si hay valores faltantes en el dataset.
missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values  # Número de valores NA en cada columna

# Explicación:
# Aquí estamos verificando si el conjunto de datos tiene valores faltantes, lo cual es importante porque los NAs pueden afectar el análisis.

# 3. Análisis univariado: distribución de variables numéricas
# Visualizamos la distribución de las variables numéricas usando histogramas.
numeric_cols <- sapply(df, is.numeric)
numeric_df <- df[, numeric_cols]

# Crear histogramas para cada variable numérica
for (col in names(numeric_df)) {
  p <- ggplot(df, aes_string(col)) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribución de", col), x = col, y = "Frecuencia") +
    theme_minimal()
  
  print(p)  # Imprimir el gráfico dentro del bucle
}

# Explicación:
# Los histogramas nos ayudan a entender la distribución de cada variable numérica. Podemos identificar si una variable tiene una distribución normal, sesgada, etc.

# 4. Análisis univariado: distribución de variables categóricas
# Visualizamos la frecuencia de las variables categóricas.
ggplot(df, aes(var5)) +
  geom_bar(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Frecuencia de la variable categórica var5", x = "Categorías", y = "Frecuencia") +
  theme_minimal()

# Explicación:
# Para variables categóricas, los gráficos de barras nos permiten ver cuántas observaciones caen en cada categoría.
# ¿Es lo mismo que un histograma?

# 5. Análisis bivariado: relación entre variables numéricas y la variable Target
# Usamos boxplots para visualizar la relación entre variables numéricas y la variable Target.
for (col in names(numeric_df)) {
  p <- ggplot(df, aes_string(x = "Target", y = col)) +
    geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "por Target"), x = "Target", y = col) +
    theme_minimal()
  
  # Usar print para mostrar el gráfico dentro del bucle
  print(p)
}

#### Ver todos los boxplots a la vez ###########################################

# Cargar el paquete
library(patchwork)

# Crear una lista para almacenar los gráficos
plots <- list()

# Crear los gráficos y guardarlos en la lista
for (col in names(numeric_df)) {
  p <- ggplot(df, aes_string(x = "Target", y = col)) +
    geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "por Target"), x = "Target", y = col) +
    theme_minimal()
  
  plots[[col]] <- p  # Guardar el gráfico en la lista
}

# Usar patchwork para combinar todos los gráficos
final_plot <- Reduce(`+`, plots) + plot_layout(ncol = 2)  # Ajusta el número de columnas según lo necesario

# Mostrar el gráfico combinado
print(final_plot)

## #############################################################################


# Explicación:
# Los boxplots son útiles para comparar la distribución de una variable numérica entre los niveles de una variable categórica, en este caso, "Target".


#Scatter plots

# Cargar librerías necesarias
library(GGally)

# Crear un subconjunto de variables numéricas para el análisis
numeric_df <- df[, sapply(df, is.numeric)]

# Diagrama de dispersión para las combinaciones de variables numéricas
ggpairs(numeric_df, aes(color = as.factor(df$Target), alpha = 0.5))

# 6. Análisis de Correlación
# Calculamos la correlación entre las variables numéricas.
cor_matrix <- cor(numeric_df)

# Visualizamos la matriz de correlación
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black", addCoef.col = "black")

# Explicación:
# El gráfico de correlación nos muestra qué variables están fuertemente correlacionadas entre sí, lo cual puede ser útil para detectar multicolinealidad o relaciones lineales entre variables.

# 7. Análisis Multivariado: Relación entre múltiples variables numéricas y categóricas
# Crear una versión compacta del dataframe para visualizar con 'pairs'
compact_df <- df %>%
  select(var1, var2, var6, var7, Target)

pairs(compact_df, pch = 21, bg = c("red", "blue")[compact_df$Target + 1])

# Explicación:
# La función 'pairs' genera una matriz de gráficos de dispersión entre múltiples variables. 
#Nos permite visualizar relaciones bivariadas entre cada par de variables, codificando los niveles de la variable 'Target'.