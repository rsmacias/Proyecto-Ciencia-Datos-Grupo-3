

# Cargar paquetes --------------------------------------------------------------

library(tidyverse)
library(FactoMineR)
library(factoextra)


data.normalizada.cluster.jerarquico <- HCPC(data.normalizada.acp, nb.clust = 0, graph = FALSE)

fviz_dend(data.normalizada.cluster.jerarquico, 
          cex = 0.6,
          palette = "lancet", 
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "lancet",
          labels_track_height = 2)


fviz_cluster(data.normalizada.cluster.jerarquico, 
             repel = TRUE, 
             palette = "lancet",
             show.clust.cent = TRUE,
             ggtheme = theme_minimal(),
             main = "Clusters de Insituciones Financieras")

#Graficamos el dendograma sobre el plano del ACP
plot(data.normalizada.cluster.jerarquico, choice="3D.map")


#Le pedimos que nos despliegue las variables más significativas para cada cluster
data.normalizada.cluster.jerarquico$desc.var$quanti

#Guardamos el resultado desplegado en un archivo de texto
sink("variables_cluster_jerarquico.txt")
data.normalizada.cluster.jerarquico$desc.var$quanti
sink()


#Guardamos en memoria la información de los clusters asignados
data.analisis.cluster.jerarquico <- data.normalizada.cluster.jerarquico$data.clust


library(psych)
library(summarytools)  #install.packages('summarytools')
describeBy(data.analisis.cluster.jerarquico, group = 'clust')

descr(data.analisis.cluster.jerarquico,
      headings = FALSE, # remove headings
      stats = "common", # most common descriptive statistics
      transpose = TRUE
)

stby(
  data = data.analisis.cluster.jerarquico,
  INDICES = data.analisis.cluster.jerarquico$clust, # by Species
  FUN = descr, # descriptive statistics
  stats = "common", # most common descr. stats
  transpose = TRUE
)

library("ggpubr")
# Box plot colored by groups: Species
ggboxplot(data.analisis.cluster.jerarquico, x = "clust", y = "Activos_Liquidos_sobre_Pasivo_Exigible",
          color = "clust",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))

ggboxplot(data.analisis.cluster.jerarquico, x = "clust", y = "Liquidez_1era_Linea",
          color = "clust",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))

ggboxplot(data.analisis.cluster.jerarquico, x = "clust", y = "Cartera_Improductiva_sobre_Patrimonio",
          color = "clust",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))


#ctable(
#  x = data.analisis.cluster.jerarquico$Cartera_Improductiva_sobre_Patrimonio,
#  y = data.analisis.cluster.jerarquico$clust
#)
