
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)
library(psych)
library(FactoMineR)
library(factoextra)

# Pre-requisitos: 
# 1.- Ejecución de 02_outliers.R


# Matriz de Correlación -------------------------------------------------------- 

data.normalizada.pretty <- pretty_name_ratio_columns(data.normalizada, data.filtro)

# Matriz correlacion - GENERAL
data.normalizada.cor <- cor(data.normalizada.pretty, method = 'pearson')
data.normalizada.cor <- round(data.normalizada.cor, digits = 1)
corrplot::corrplot(data.normalizada.cor)

# Análisis Componentes Principales --------------------------------------------- 

## Análisis previo -------------------------------------------------------------
scree(data.normalizada,main ="Gráfico de Sedimentación")
#Nota: Según el gráfico de sedimentación lo óptimo son 5 componentes 

fa.parallel(data.normalizada,fa="pc") # Confirma lo expuesto en el gráfico anterior

## Cálculo PCA -----------------------------------------------------------------
data.normalizada.acp <- PCA(data.normalizada.pretty, graph = FALSE)
names(data.normalizada.acp)

data.normalizada.acp$eig

summary(data.normalizada.acp)
# se puede observar que con los primeros 5 componentes principales (eigenvalues) 
# se explican el 79.994 % de los datos. Esto a través de la varianza acumulativa.
# Con 5 componentes perderíamos un 21% de explicación de los datos.
# se pueden tomar hasta el componente 7 para sólo perder un 11% de explicación de los datos.


# Relación entre los PCA con las variables originales
data.normalizada.acp$svd$V

# Los valores de los PCA a unir en el dataframe original
data.normalizada.acp$ind$coord

data.normalizada.acp %>% biplot(cex = .5)

#pca %>% biplot(cex = .5)
var <- get_pca_var(data.normalizada.acp)
var
var$coord
var$cor
corrplot::corrplot(var$cor)

var$contrib

ind <- get_pca_ind(data.normalizada.acp)
ind
ind$coord
ind$cos2
ind$contrib
