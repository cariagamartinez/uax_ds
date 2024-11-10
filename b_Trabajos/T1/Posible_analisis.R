# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar el dataset
#data <- read.csv("ruta/dataset.csv")

data <- read.csv("T1/dataset.csv")

# Vista general de los datos
summary(data)
str(data)

# Gráficas de distribución para variables numéricas
# Histograma de 'var1'
ggplot(data, aes(x = var1)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribución de var1", x = "var1", y = "Frecuencia")

# Histograma de 'var2'
ggplot(data, aes(x = var2)) + 
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Distribución de var2", x = "var2", y = "Frecuencia")

# Boxplot para comparar 'var6' entre los niveles de 'Target'
ggplot(data, aes(x = factor(Target), y = var6)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot de var6 según Target", x = "Target", y = "var6")

# Gráfica de barras para variables categóricas
# Distribución de 'var3'
ggplot(data, aes(x = factor(var3))) + 
  geom_bar(fill = "purple") +
  labs(title = "Distribución de var3", x = "var3", y = "Frecuencia")

# Scatterplot de 'var1' vs 'var2' coloreado por 'Target'
ggplot(data, aes(x = var1, y = var2, color = factor(Target))) +
  geom_point() +
  labs(title = "Relación entre var1 y var2", x = "var1", y = "var2")

# Guardar las gráficas
ggsave("histograma_var1.png")
ggsave("histograma_var2.png")
ggsave("boxplot_var6_target.png")
ggsave("barras_var3.png")
ggsave("scatterplot_var1_var2.png")
