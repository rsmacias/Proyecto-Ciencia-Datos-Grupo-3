
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)


# Cargar datos -----------------------------------------------------------------

setwd('D:/Cursos/Ciencia de Datos/FinDeSemana/Proyecto/Labs/01022022')

files_path <- c("data/Bancos.csv", "data/Mutualistas.csv", "data/Publicas.csv", "data/S1.csv", "data/S2.csv", "data/S3.csv")
data <- c("BANCOS", "MUTUALISTAS", "PUBLICAS", "COOPERATIVAS", "COOPERATIVAS", "COOPERATIVAS")
segmento <- c(NA, NA, NA, 1, 2, 3)
data.base <- data.frame(data, files_path, segmento)


for (archivo in data.base$files_path) {
  ambito <- as.character(data.base %>% filter( files_path == archivo ) %>% select( data ))
  segmento <- as.numeric(data.base %>% filter(files_path == archivo) %>% select(segmento))
  print(cat("Carga de datos: ", ambito, fill = TRUE))
  print(cat("Segmento: ", segmento, fill = TRUE))
  print(cat("Archivo: ", archivo, fill = TRUE))
  if(file.exists(archivo)) {
    temporal <- read.csv(file = archivo, sep = ";", dec = ".", header = TRUE)  
    temporal$tipo_ifi <- ambito
    temporal$segmento <- segmento
    
    if(exists("data.ifis")) {
      data.ifis <- rbind(data.ifis, temporal)
    } else {
      data.ifis = temporal[FALSE,]
      data.ifis <- rbind(data.ifis, temporal)
    }
    
    print(cat("Datos cargados: ", ambito, fill = TRUE))  
  }
}

data.ifis %>% filter( tipo_ifi == "COOPERATIVAS") %>% view()




