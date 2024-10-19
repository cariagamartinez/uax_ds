# Instalar paquetes necesarios si no están instalados
if(!require(tidyverse)) install.packages("tidyverse")

# Cargar librerías
library(tidyverse)

# Simular un dataset
set.seed(123)
n <- 1000
df <- tibble(
  id = 1:n,
  age = rnorm(n, mean = 35, sd = 10),          # Edad simulada con distribución normal
  income = rnorm(n, mean = 50000, sd = 15000), # Ingreso simulada
  gender = sample(c("Male", "Female"), n, replace = TRUE),  # Variable categórica
  department = sample(c("HR", "IT", "Finance", "Sales"), n, replace = TRUE), # Categórica
  purchased = sample(c(0, 1), n, replace = TRUE) # Variable binaria (target)
)

# Revisar las primeras filas
glimpse(df)

# Resumen estadístico de las variables numéricas
summary(df)

# Ver la estructura del dataset
str(df)

# Verificar valores faltantes
sapply(df, function(x) sum(is.na(x)))

# Tablas de frecuencia para variables categóricas
table(df$gender)
table(df$department)
table(df$purchased)

# Análisis de correlación para variables numéricas
cor(df %>% select(age, income), use = "complete.obs")

# Gráficos: Distribuciones
ggplot(df, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Distribución de la edad")

ggplot(df, aes(x = income)) + 
  geom_histogram(binwidth = 5000, fill = "green", color = "white") +
  labs(title = "Distribución de ingresos")

# Gráficos: Relación entre variables numéricas
ggplot(df, aes(x = age, y = income)) +
  geom_point(color = "red") +
  labs(title = "Relación entre Edad e Ingreso")

# Gráficos: Variables categóricas
ggplot(df, aes(x = gender, fill = purchased)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Compras por Género")

ggplot(df, aes(x = department, fill = purchased)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Compras por Departamento")