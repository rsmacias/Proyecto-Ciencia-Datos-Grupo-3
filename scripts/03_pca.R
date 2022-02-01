
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)

# Pre-requisitos: 
# 1.- Ejecución de 02_outliers.R



df_general.withnames <- pretty_name_ratio_columns(df_normalizado, df_indicadores)

# Matriz correlacion - GENERAL
df_general.cor <- cor(df_general.withnames, method = 'pearson')
#corrplot::corrplot(df_general.cor)
df_general.cor <- round(df_general.cor, digits = 1)
corrplot::corrplot(df_general.cor)