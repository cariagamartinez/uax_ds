
# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

# Cargar los datasets desde los archivos descargados
df1 <- read.csv("data/dataset_data_wrangling.csv")
df2 <- read.csv("data/dataset_for_joins_and_pivot.csv")

# -----------------------
# 1. Aplicar Joins
# -----------------------

# Left Join: Mantiene todas las filas del primer dataset y une las columnas del segundo dataset
left_joined <- left_join(df1, df2, by = "user_id")
print(head(left_joined))

# Inner Join: Solo mantiene las filas que coinciden en ambas tablas
inner_joined <- inner_join(df1, df2, by = "user_id")
print(head(inner_joined))

# Right Join: Mantiene todas las filas del segundo dataset y une las columnas del primero
right_joined <- right_join(df1, df2, by = "user_id")
print(head(right_joined))

# Full Join: Mantiene todas las filas de ambos datasets
full_joined <- full_join(df1, df2, by = "user_id")
print(head(full_joined))

# -----------------------
# 2. Pivot Longer
# -----------------------

# Para convertir el dataset de ancho a largo, seleccionamos algunas columnas (ej. income y bonus)
pivot_long <- pivot_longer(df2, cols = c(experience_years, bonus), names_to = "variable", values_to = "value")
print(head(pivot_long))

# -----------------------
# 3. Pivot Wider
# -----------------------

# Convertir el dataset largo nuevamente a ancho
pivot_wide <- pivot_wider(pivot_long, names_from = variable, values_from = value)
print(head(pivot_wide))
