# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)

# Cargar el dataset
df <- read.csv("data/simple_health_survey.csv")

# Paso 1: Inspección inicial del dataset
str(df)
summary(df)

# Paso 2: Manejo de valores faltantes
# Ver el número de valores faltantes por columna
colSums(is.na(df))

# Imputación de valores faltantes en 'income' usando la mediana
df <- df %>%
  mutate(income = ifelse(is.na(income), median(income, na.rm = TRUE), income),
         bmi = ifelse(is.na(bmi), mean(bmi, na.rm = TRUE), bmi),
         health_score = ifelse(is.na(health_score), median(health_score, na.rm = TRUE), health_score))

# Paso 3: Análisis de sesgos
# Visualizar la distribución de 'income' antes y después de la transformación logarítmica
ggplot(df, aes(x = income)) +
  geom_histogram(fill = "blue", bins = 10) +
  theme_minimal() +
  labs(title = "Distribución de ingresos")

# Aplicar transformación logarítmica
df$income_log <- log1p(df$income)

# Visualización de la transformación logarítmica
ggplot(df, aes(x = income_log)) +
  geom_histogram(fill = "green", bins = 10) +
  theme_minimal() +
  labs(title = "Distribución de ingresos (transformación logarítmica)")

# Paso 4: Análisis de correlaciones
cor_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")
print(cor_matrix)

# Visualizar correlaciones usando ggplot2
cor_melted <- as.data.frame(as.table(cor_matrix))
ggplot(cor_melted, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Matriz de correlación")
