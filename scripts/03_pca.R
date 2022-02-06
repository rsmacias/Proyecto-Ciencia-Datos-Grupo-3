
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
scree(data.normalizada, main ="Gráfico de Sedimentación")
#Nota: Según el gráfico de sedimentación lo óptimo son 5 componentes 

fa.parallel(data.normalizada,fa="pc") # Confirma lo expuesto en el gráfico anterior

## Cálculo PCA -----------------------------------------------------------------
data.normalizada.acp <- PCA(data.normalizada.pretty, graph = FALSE)
names(data.normalizada.acp)

data.normalizada.acp$eig # Eigen Values y su % de contribución

summary(data.normalizada.acp)

# Visualizar los eigenvalues
fviz_eig(data.normalizada.acp, addlabels = TRUE, ylim = c(0, 35), main="Visualización de los Eigen Values")
eig.val <- get_eigenvalue(data.normalizada.acp)

# se puede observar que con los primeros 5 componentes principales (eigenvalues) 
# se explican el 79.994 % de los datos. Esto a través de la varianza acumulativa.
# Con 5 componentes perderíamos un 21% de explicación de los datos.
# se pueden tomar hasta el componente 7 para sólo perder un 11% de explicación de los datos.


# Relación entre los PCA con las variables originales
data.normalizada.acp$svd$V

# Los valores de los PCA a unir en el dataframe original (Coordenadas)
data.normalizada.acp$ind$coord

#data.normalizada.acp %>% biplot(cex = .5)
#biplot(data.normalizada.acp, scale = 0)
#biplot(data.normalizada.acp)


plot(data.normalizada.acp, choix = 'ind')
plot(data.normalizada.acp, choix = 'var')

#plot(data.normalizada.acp.values, pch=25, col=rgb(0,0,0,0.5))

#pca %>% biplot(cex = .5)
var <- get_pca_var(data.normalizada.acp)
var
var$coord
var$cor
var$cos2
var$contrib
corrplot::corrplot(var$cor)

# Gráfico: Calidad de la Representación
corrplot::corrplot(var$cos2, is.corr=FALSE, main="Calidad de la Representación de las variables")

corrplot::corrplot(var$contrib, is.corr=FALSE)    

fviz_pca_var(data.normalizada.acp, col.var = "black")

fviz_cos2(data.normalizada.acp, choice = "var", axes = 1:5)

# Gráfico: Contribución
fviz_contrib(data.normalizada.acp, choice = "var", axes = 1:5, top = 17)

fviz_contrib(data.normalizada.acp, choice = "var", axes = 5, top = 5)

var$contrib

ind <- get_pca_ind(data.normalizada.acp)
ind
ind$coord
ind$cos2
ind$contrib




#PC-1     (Eficiencia en la gestión de colocaciones)
#=========
#Proporcion_Activos_Productivos_Netos  
#Cobertura_Cartera_Problematica (Contingencia de la cartera)
#Grado_de_Absorción  (No genera ingresos para los gastos operativos)
#ROA    (No genera ingresos para el patrimonio)

#PC-2     (Liquidez) 
#=========
#Activos_Liquidos_sobre_Pasivo_Exigible                   
#Fondos_Disponibles_sobre_Total_Depósitos_Corto_Plazo     
#Liquidez_1era_Linea                                      
#Liquidez_2da_Linea                                       

#PC-3
#=========
#Eficiencia_Operativa 

#PC-4     (Solvencia del Patrimonio)
#=========
#Cartera_Improductiva_sobre_Patrimonio                    
#Indice_de_Capitalización_Neto 
#Margen_Intermediación_Estimado_sobre_Patrimonio_Promedio 
#ROE 
#Relacion_Pasivos_con_Costo_con_Productividad_Generada  ****
  
#  PC-5     (Riesgo en las colocaciones)
#=========
#Morosidad_Cartera_Total 
#Eficiencia_Administrativa_de_Personal 
#Relacion_Pasivos_con_Costo_con_Productividad_Generada  ****
  
  
