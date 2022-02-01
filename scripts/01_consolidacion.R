
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

# Transformación datos ---------------------------------------------------------

if(exists("data.ifis")){
  print("Transformacion General")
  temporal <- data.ifis %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "fecha_fin_mes",
      values_to = "valor",
      values_drop_na = FALSE
    ) %>% separate(
      fecha_fin_mes, 
      c("dia", "mes", "anio"), 
      remove = FALSE, 
      convert = TRUE
    ) %>% mutate( 
      dia = as.integer(str_sub(dia, -2)),
      fecha_fin_mes = as.Date(str_sub(fecha_fin_mes, -10), format = "%d.%m.%Y")
    ) %>% rename(
      "codigo_ifi" = "COD..IFI",
      "nombre_ifi" = "COD..INDICE",
      "nombre_ratio_financiero" = "CUENTA",
      "codigo_ratio_financiero" = "INDICE"
    ) %>% filter(
      !codigo_ifi %in% c(0, 1000) & !is.na(codigo_ifi)
    ) %>% filter(
      !codigo_ifi %in% c(1000, 300) & !is.na(codigo_ifi) 
    ) %>% filter(
      !codigo_ifi %in% c(1000, 500) & !is.na(codigo_ifi) 
    ) %>% filter(
      !codigo_ifi %in% c(1000, 10000, 10001) & !is.na(codigo_ifi) 
    ) %>% filter(
      !codigo_ifi %in% c(1000, 10002) & !is.na(codigo_ifi) 
    ) %>% filter(
      !codigo_ifi %in% c(1000, 10003) & !is.na(codigo_ifi) 
    )
  print("Datos transformados")
  glimpse(temporal)
}




