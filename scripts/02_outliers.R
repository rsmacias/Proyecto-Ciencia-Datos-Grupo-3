
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)
library(mice)

# Pre-requisitos: 
# 1.- Ejecución de 01_consolidacion.R
# 2.- Ejecución de 00_soporte.R

# Análisis de outliers ---------------------------------------------------------

# ------------------------------------------------------------------------------
# ANTES 
# ------------------------------------------------------------------------------
# Visualizar los outliers en diagramas boxplots por cada indicador 
for (indicador in analisis.prefix_indicadores) {
  boxplot(data.analisis[[indicador]], horizontal = TRUE, main = indicador)
}

# Visualizar los outliers en diagramas boxplots en un único diagrama
data.analisis %>% 
  pivot_longer(cols = 6:22, names_to = "indicadores", values_to = "valores") %>% 
  ggplot2::ggplot(data = ., aes(x = indicadores, y = valores, fill = valores)) +
  geom_jitter(aes(color = indicadores), width=0.1,alpha=0.7) +
  geom_boxplot(aes(color = indicadores), alpha=0.5) + 
  xlab('Indices Financieros') + 
  ylab('Valor') +
  ggtitle('Ratios Financieros de IFIS') +
  theme_minimal()

# ------------------------------------------------------------------------------
# REMOVER OUTLIERS 
# ------------------------------------------------------------------------------

## Estrategia 1: Remover los registros de cada outliers encontrado 
outliers.v1 <- remove_outliers (data.analisis, analisis.prefix_indicadores, FALSE)

graficar_boxplots(outliers.v1, analisis.prefix_indicadores)
# Resumen: No quita todos los outliers ya que crea nuevos a medida que elimina observaciones.
# Además como desventaja se tiene que se pierde demasiados datos. De 198 observaciones pasamos 
# a 92 observaciones. 

## Estrategia 2: Reemplazo de outliers por la Media y Mediana 
outliers.v2 <- remove_outliers_v2 (data.analisis, analisis.prefix_indicadores, TRUE)

graficar_boxplots(outliers.v2, analisis.prefix_indicadores)
# Resumen: La ventaja es que no se pierden datos y se disminuyen considerablemente los 
# outliers, sin embargo al reemplazarlos por la media y mediana los datos se mueven de lugar. 
# Pierden su forma original.

## Estrategia 3: Reemplazar los outliers por NA e imputarlos  
outliers.v3 <- remove_outliers (data.analisis, analisis.prefix_indicadores, TRUE)

sum(is.na(outliers.v3)) # Total de outliers
colSums(is.na(outliers.v3)) # Total de outliers por variables / columna

graficar_boxplots(outliers.v3, analisis.prefix_indicadores)

### Estrategia 3.1: Imputación de los NA por la media 
outliers.v3.1.imputed_data <- mice(outliers.v3[,names(outliers.v3) %in% analisis.prefix_indicadores],
                     m = 1,
                     maxit = 1, 
                     method = "mean",
                     seed = 2018,
                     print=F)
outliers.v3.1 <- mice::complete(outliers.v3.1.imputed_data)
outliers.v3.1 %>% View()
graficar_boxplots(outliers.v3.1, analisis.prefix_indicadores, 1:17)
# Resumen: La desventaja es que al imputar con este método se crean nuevos outliers 

### Estrategia 3.2: Imputación de los NA por regresión 
outliers.v3.2.imputed_data  <- mice(outliers.v3[,names(outliers.v3) %in% analisis.prefix_indicadores],
                     m = 1,
                     maxit = 1, 
                     method = "norm.predict",
                     seed = 2018,
                     print=F)
outliers.v3.2 <- mice::complete(outliers.v3.2.imputed_data)
graficar_boxplots(outliers.v3.2, analisis.prefix_indicadores, 1:17)
# Resumen: Genera muy pocos outliers y conserva la forma original de los datos 

### Estrategia 3.3: Imputación de los NA por regresión estocástica
outliers.v3.3.imputed_data <- mice(outliers.v3[,names(outliers.v3) %in% analisis.prefix_indicadores],
                      m = 1,
                      maxit = 1, 
                      method = "norm.nob",
                      seed = 2018,
                      print=F)
outliers.v3.3 <- mice::complete(outliers.v3.3.imputed_data)
graficar_boxplots(outliers.v3.3, analisis.prefix_indicadores, 1:17)
# Resumen: Genera muy pocos outliers y conserva la forma original de los datos 


# ------------------------------------------------------------------------------
# NORMALIZACIÓN / ESCALAMIENTO DE VALORES
# ------------------------------------------------------------------------------
data.normalizada <- normalizar(outliers.v3.2, analisis.prefix_indicadores)

