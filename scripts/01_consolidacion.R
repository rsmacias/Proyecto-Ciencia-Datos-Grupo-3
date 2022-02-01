
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)


# Cargar datos -----------------------------------------------------------------

setwd('D:/Cursos/Ciencia de Datos/FinDeSemana/Proyecto/Labs/01022022')

file_path_bancos <- "data/Bancos.csv"
file_path_mutualistas <- "data/Mutualistas.csv"
file_path_pulicas <- "data/Publicas.csv"
file_path_coops1 <- "data/S1.csv"
file_path_coops2 <- "data/S2.csv"

print("1.- Carga de datos: Banco")
if(file.exists(file_path_bancos)) {
  data.bancos <- read.csv(file = file_path_bancos, sep = ";", dec = ".", header = TRUE)
  print("Datos cargados: Bancos")
}