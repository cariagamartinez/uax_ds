
# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(naniar) #Para ggmiss

# Leer el dataset modificado
data <- read.csv("T4/Modified_Alzheimer_dataset.csv")

# Inspeccionar las primeras filas del dataset
head(data)

# 1. Identificar valores faltantes
cat("Valores faltantes por columna:\n")
sapply(data, function(x) sum(is.na(x)))
gg_miss_var(data, show_pct = TRUE)

# Reemplazar valores faltantes con la media en Edad_Paciente
data$Edad_Paciente[is.na(data$Edad_Paciente)] <- mean(data$Edad_Paciente, na.rm = TRUE)
gg_miss_var(data, show_pct = TRUE)

# 2. Detectar outliers en Tiempo_Reaccion_TestCognitivo
boxplot(data$Tiempo_Reaccion_TestCognitivo, main="Outliers en Tiempo de Reacción")

# Remover outliers usando el rango intercuartil (IQR)
Q1 <- quantile(data$Tiempo_Reaccion_TestCognitivo, 0.25, na.rm = TRUE)
Q3 <- quantile(data$Tiempo_Reaccion_TestCognitivo, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR
data <- data %>% filter(Tiempo_Reaccion_TestCognitivo >= lim_inf & Tiempo_Reaccion_TestCognitivo <= lim_sup)

boxplot(data$Tiempo_Reaccion_TestCognitivo, main="Sin outliers en Tiempo de Reacción")

# 3. Corregir errores tipográficos en Estado_Clinico
table(data$Estado_Clinico)
data$Estado_Clinico <- ifelse(data$Estado_Clinico == "error", NA, data$Estado_Clinico)
data$Estado_Clinico[is.na(data$Estado_Clinico)] <- "a" # Asignar categoría más común

# 4. Analizar desbalance en Diagnostico_Alzheimer
table(data$Diagnostico_Alzheimer)
ggplot(data, aes(x = as.factor(Diagnostico_Alzheimer))) +
  geom_bar() +
  labs(title = "Distribución de Diagnóstico Alzheimer", x = "Diagnóstico", y = "Frecuencia")

# Guardar el dataset limpio
write.csv(data, "Cleaned_Alzheimer_dataset.csv", row.names = FALSE)
