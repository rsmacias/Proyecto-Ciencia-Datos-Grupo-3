
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)

# 1.- Se seleccionan los datos del PCA (coordenadas para cada observación)
data.normalizada.acp.coord <- data.normalizada.acp$ind$coord

# Partitioning Clustering ------------------------------------------------------

# Análisis de clusters ---------------------------------------------------------

# Nota: Sólo para pruebas se prueba kmeans sin embargo no se espera buenos resul-
#       tados por la presencia de outliers y como medida de distancia Manhattan.

# Clustering: Kmeans
# Distancia: Euclidean

# Elbow method - Euclidean distance
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "wss", 
             diss = get_dist(data.normalizada.acp.coord, method = "euclidean"), 
             k.max = 15) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Número óptimo de clústers")

# Silhouette method - Euclidean distance
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "silhouette", 
             diss = get_dist(data.normalizada.acp.coord, method = "euclidean"), 
             k.max = 15) +
  labs(subtitle = "Silhouette method - Número óptimo de clústers")

# Gap statistic - Euclidean distance
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             nstart = 50,  
             method = "gap_stat", 
             diss = get_dist(data.normalizada.acp.coord, method = "euclidean"), 
             nboot = 50, 
             k.max = 15) +
  labs(subtitle = "Gap statistic method - Número óptimo de clústers")


# Clustering: Kmeans
# Distancia: Manhattan

# Elbow method - Euclidean distance
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "wss", 
             diss = get_dist(data.normalizada.acp.coord, method = "manhattan"), 
             k.max = 15) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Número óptimo de clústers - Manhattan")

# Silhouette method - Euclidean distance
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "silhouette", 
             diss = get_dist(data.normalizada.acp.coord, method = "manhattan"), 
             k.max = 15) +
  labs(subtitle = "Silhouette method - Número óptimo de clústers - Manhattan")

# Gap statistic - Euclidean distance
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             nstart = 50,  
             method = "gap_stat", 
             diss = get_dist(data.normalizada.acp.coord, method = "manhattan"), 
             nboot = 50, 
             k.max = 15) +
  labs(subtitle = "Gap statistic method - Número óptimo de clústers - Manhattan")

# Resultado: Según los análisis anteriores dan como clústeres óptimos (1, 2, 4)
#            Como no tiene sentido común tener un único clúster, se evaluarán 2 y 4

# 2 CLUSTERS
set.seed(80) # fijar semilla
data.normalizada.cluster.2.kmeans <- kmeans(data.normalizada.acp.coord, 
                                            centers = 2,
                                            nstart = 50)

fviz_cluster(object = data.normalizada.cluster.2.kmeans, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "t",  #"euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering K-means - 2 clusters") +
  theme_bw() +
  theme(legend.position = "none")

# 3 CLUSTERS
set.seed(80) # fijar semilla
data.normalizada.cluster.3.kmeans <- kmeans(data.normalizada.acp.coord, 
                                            centers = 3,
                                            nstart = 50)

fviz_cluster(object = data.normalizada.cluster.3.kmeans, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "t",  #"euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering K-means - 3 clusters") +
  theme_bw() +
  theme(legend.position = "none")

# 5 CLUSTERS
set.seed(80) # fijar semilla
data.normalizada.cluster.5.kmeans <- kmeans(data.normalizada.acp.coord, 
                                            centers = 5,
                                            nstart = 50)

fviz_cluster(object = data.normalizada.cluster.5.kmeans, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "t",  #"euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering K-means - 5 clusters") +
  theme_bw() +
  theme(legend.position = "none")

# 9 CLUSTERS
set.seed(80) # fijar semilla
data.normalizada.cluster.9.kmeans <- kmeans(data.normalizada.acp.coord, 
                                            centers = 9,
                                            nstart = 50)

fviz_cluster(object = data.normalizada.cluster.9.kmeans, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "t",  #"euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering K-means - 9 clusters") +
  theme_bw() +
  theme(legend.position = "none")

# 4 CLUSTERS
#data.normalizada.cluster.4.kmeans <- kmeans(data.normalizada.acp.coord, 
#                                            centers = 4,
#                                            nstart = 50)

#fviz_cluster(object = data.normalizada.cluster.4.kmeans, 
#             data = data.normalizada.acp.coord, 
#             show.clust.cent = TRUE,
#             ellipse.type = "t", # "euclid", 
#             star.plot = TRUE, 
#             repel = TRUE) +
#  labs(title = "Resultados clustering K-means - 4 clusters") +
#  theme_bw() +
#  theme(legend.position = "none")







# Análisis de clusters ---------------------------------------------------------

# Nota: Por la presencia de outliers en los datos se selecciona Pam como método 
#       de agrupamietno y como medida de distancia Manhattan.

# Clustering: PAM (9, 6, 4, 2)
# Method: wss

fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = pam, 
             method = "wss", 
             k.max = 15,
             diss = dist(data.normalizada.acp.coord, 
                         method = "manhattan")) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Número óptimo de clústers - PAM")


# Silhouette method - Euclidean distance
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = pam, 
             method = "silhouette", 
             diss = get_dist(data.normalizada.acp.coord, method = "manhattan"), 
             k.max = 15) +
  labs(subtitle = "Silhouette method - Número óptimo de clústers - PAM")

# Gap statistic - Euclidean distance
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = pam, 
             method = "gap_stat", 
             diss = get_dist(data.normalizada.acp.coord, method = "manhattan"), 
             nboot = 50, 
             k.max = 15) +
  labs(subtitle = "Gap statistic method - Número óptimo de clústers - PAM")


# 2 CLUSTERS
set.seed(123)
data.normalizada.cluster.2.pam <- pam(x = data.normalizada.acp.coord, 
                                      k = 2, 
                                      metric = "manhattan")

data.normalizada.cluster.2.pam$medoids
data.normalizada.cluster.2.pam$clustering

fviz_cluster(object = data.normalizada.cluster.2.pam, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "euclid", #t, "euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering PAM - 2 Clusters") +
  theme_bw() +
  theme(legend.position = "none")

# 4 CLUSTERS
set.seed(123)
data.normalizada.cluster.4.pam <- pam(x = data.normalizada.acp.coord, 
                                      k = 4, 
                                      metric = "manhattan")

fviz_cluster(object = data.normalizada.cluster.4.pam, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "euclid", #t, "euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering PAM - 4 Clusters") +
  theme_bw() +
  theme(legend.position = "none")


# 6 CLUSTERS
set.seed(123)
data.normalizada.cluster.6.pam <- pam(x = data.normalizada.acp.coord, 
                                      k = 6, 
                                      metric = "manhattan")

fviz_cluster(object = data.normalizada.cluster.6.pam, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "euclid", #t, "euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering PAM - 6 Clusters") +
  theme_bw() +
  theme(legend.position = "none")


# 9 CLUSTERS
set.seed(123)
data.normalizada.cluster.9.pam <- pam(x = data.normalizada.acp.coord, 
                    k = 9, 
                    metric = "manhattan")

names(data.normalizada.cluster.9.pam)
data.normalizada.cluster.9.pam$medoids
data.normalizada.cluster.9.pam$clustering

fviz_cluster(object = data.normalizada.cluster.9.pam, 
             data = data.normalizada.acp.coord, 
             show.clust.cent = TRUE,
             ellipse.type = "euclid", #t, "euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering PAM - 9 Clusters") +
  theme_bw() +
  theme(legend.position = "none")


# Análisis de los resultados Kmeans --------------------------------------------

print('2 - kmeans')
data.normalizada.cluster.2.kmeans$totss # inercia total
data.normalizada.cluster.2.kmeans$betweenss # inercia ínter grupos
data.normalizada.cluster.2.kmeans$withinss # inercia intra grupos
data.normalizada.cluster.2.kmeans$tot.withinss # inercia intra grupos (total)
print('4 - kmeans')
data.normalizada.cluster.4.kmeans$totss # inercia total
data.normalizada.cluster.4.kmeans$betweenss # inercia ínter grupos
data.normalizada.cluster.4.kmeans$withinss # inercia intra grupos
data.normalizada.cluster.4.kmeans$tot.withinss # inercia intra grupos (total)
print('5 - kmeans')
data.normalizada.cluster.5.kmeans$totss # inercia total
data.normalizada.cluster.5.kmeans$betweenss # inercia ínter grupos
data.normalizada.cluster.5.kmeans$withinss # inercia intra grupos
data.normalizada.cluster.5.kmeans$tot.withinss # inercia intra grupos (total)
print('9 - kmeans')
data.normalizada.cluster.9.kmeans$totss # inercia total
data.normalizada.cluster.9.kmeans$betweenss # inercia ínter grupos
data.normalizada.cluster.9.kmeans$withinss # inercia intra grupos
data.normalizada.cluster.9.kmeans$tot.withinss # inercia intra grupos (total)


# Nota: La distancia inter cluster "betweenss" (distancia entre los centroides de cada cluster)
#       es mayor con 9 cluster.
#       La distancia intra cluster "tot.withinss" (distancias entre cada observación al centroide de cada cluster)
#       es menor con 9 cluster.




















fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "wss", 
             k.max = 15, 
             diss = get_dist(data.normalizada.acp.coord, method = "euclidean"), 
             nstart = 50)

fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "gap_stat", 
             k.max = 15, 
             diss = get_dist(data.normalizada.acp.coord, method = "euclidean"), 
             nstart = 50)

fviz_nbclust(x = data.normalizada.acp.coord, 
             FUNcluster = kmeans, 
             method = "silhouette", 
             k.max = 15, 
             diss = get_dist(data.normalizada.acp.coord, method = "euclidean"), 
             nstart = 50)



fviz_nbclust(data.normalizada.acp.coord, kmeans,
             method = "gap_stat")


set.seed(80) # fijar semilla
data.normalizada.kmeans <- kmeans(data.normalizada.acp$ind$coord, centers = 5)

data.normalizada.kmeans$cluster

fviz_cluster(object = data.normalizada.kmeans, 
             data = data.normalizada.acp$ind$coord, 
             show.clust.cent = TRUE,
             ellipse.type = "euclid", 
             star.plot = TRUE, 
             repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")










set.seed(80) # fijar semilla
df_general.km <- kmeans(df_general.orkmean, centers = 7) # Realizamos clustering

head(df_general.km$cluster) # asignación observaciones a clusters

df_general.km$totss # inercia total
df_general.km$betweenss # inercia ínter grupos
df_general.km$withinss # inercia intra grupos
df_general.km$tot.withinss # inercia intra grupos (total)

print(df_general.km)


df_general.withclusters <- cbind(df_general, cluster = df_general.km$cluster)
df_general.withclusters %>% View()


fviz_cluster(object = df_general.km, data = df_general.orkmean, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")


fviz_cluster(object = df_general.km, data = df_general.orkmean, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")


