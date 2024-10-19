# Carga de librerías necesarias
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(jsonlite)
library(xml2)


# 1. Transformación de datos con dplyr y data.table

# Creamos dataframe en R
data <- data.frame(
  id = 1:5,
  category = c("A", "B", "A", "B", "C"),
  value = c(10, 20, 15, 25, 30)
)

# a) Pivotaje de datos
# Pivotar datos de ancho a largo (similar a 'melt' en pandas)
long_data <- pivot_longer(data, cols = value, names_to = "variable", values_to = "value_long")
print(long_data)
# El dataframe se reestructurará de tal manera que ahora habrá dos columnas nuevas:
# variable: indica qué columna original correspondía a cada fila.
# value_long: contiene el valor correspondiente a esa variable.

# Pivotar de largo a ancho
wide_data <- pivot_wider(long_data, names_from = variable, values_from = value_long)
print(wide_data)

wide_data <- pivot_wider(data, names_from = id, values_from = value)
print(wide_data)

# b) Agrupación y agregación
# Agrupar por categoría y calcular el promedio del valor
grouped_data <- data %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))
print(grouped_data)

# c) Operaciones con fechas y horas
# Crear una columna de fechas y realizar operaciones con lubridate
data$date <- as.Date(c('2022-01-01', '2022-02-01', '2022-03-01', '2022-04-01', '2022-05-01'))
data$month <- month(data$date)
print(data)

# d) Transformación de datos categóricos
# Convertir factores a variables dummy
dummy_data <- data %>%
  mutate(dummy_A = ifelse(category == "A", 1, 0))
print(dummy_data)

# e) Aplicación de funciones con mutate
# Aplicación de funciones a columnas específicas
data <- data %>%
  mutate(value_squared = value^2)
print(data)

# 2. Manejo de datos desbalanceados

# Simulamos un dataset desbalanceado
data_imbalanced <- data.frame(
  class = c(rep("A", 90), rep("B", 10)),
  value = rnorm(100)
)

# Visualización del desbalance
table(data_imbalanced$class)

# Sobremuestreo con la librería ROSE (combinación de sub/sobre muestreo)
library(ROSE)
balanced_data <- ROSE(class ~ ., data = data_imbalanced, seed = 1)$data
table(balanced_data$class)


# 3. Fusión y combinación de datos

# Simulación de dos dataframes
data1 <- data.frame(id = 1:3, value1 = c(10, 20, 30))
data2 <- data.frame(id = 2:4, value2 = c(40, 50, 60))

# Fusión de tablas usando left_join
merged_data <- left_join(data1, data2, by = "id")
print(merged_data)

# Anti join para ver diferencias entre tablas
anti_join_data <- anti_join(data1, data2, by = "id")
print(anti_join_data)




################################################################################
#Recordatorio (si necesario)

# 4. Manejo eficiente de datos grandes en R con data.table

# Convertimos el dataframe a data.table
data_dt <- as.data.table(data)

# Selección y filtrado eficientes en data.table
filtered_data_dt <- data_dt[category == "A"]
print(filtered_data_dt)

# Agrupación y agregación en data.table
grouped_data_dt <- data_dt[, .(mean_value = mean(value)), by = category]
print(grouped_data_dt)

# Operaciones con datos estructurados y semi-estructurados

# a) Manejo de JSON
json_data <- '{"id": 1, "name": "John", "age": 30, "city": "New York"}'
parsed_json <- fromJSON(json_data)
print(parsed_json)

# b) Manejo de XML
xml_data <- '<root><person><id>1</id><name>John</name></person></root>'
parsed_xml <- read_xml(xml_data)
print(xml_text(xml_find_first(parsed_xml, "//name")))

# c) Normalización de JSON anidado
complex_json <- '[{"id":1, "info":{"age":30, "city":"New York"}}, {"id":2, "info":{"age":25, "city":"Los Angeles"}}]'
normalized_data <- fromJSON(complex_json, flatten = TRUE)
print(normalized_data)

# Conexión con APIs (JSON desde una API pública)
# Ejemplo básico con la API de ejemplo JSONPlaceholder
api_data <- fromJSON("https://jsonplaceholder.typicode.com/posts")
print(head(api_data))