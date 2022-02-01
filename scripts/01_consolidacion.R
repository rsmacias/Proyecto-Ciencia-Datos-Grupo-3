
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)
library(fastDummies)

setwd('D:/Cursos/Ciencia de Datos/FinDeSemana/Proyecto/Labs/01022022')

## Definición de variables: ---------------------------------------------------- 
### Objetivo: Carga de datos
files_path <- c("data/Bancos.csv", "data/Mutualistas.csv", "data/Publicas.csv", "data/S1.csv", "data/S2.csv", "data/S3.csv")
data <- c("BANCOS", "MUTUALISTAS", "PUBLICAS", "COOPERATIVAS", "COOPERATIVAS", "COOPERATIVAS")
segmento <- c(NA, NA, NA, 1, 2, 3)
data.base <- data.frame(data, files_path, segmento)

### Objetivo: Filtro para análisis
analisis.anio <- 2021
analisis.mes <- 11     # Se seleccionó septiembre porque no hay datos de octubre 2021 para las cooperativas y mutualistas
analisis.indicadores <- c('C_04', 'C_03', 'C_08', 'MA_04', 'CA_13', 'CA_25', 'CA_11', 'CA_12', 'MA_02', 'MA_01', 'MA_03', 'RE_01', 'RE_03', 'L_04', 'LE_15', 'LE_24', 'L_03')
analisis.prefix_indicadores <- c('ratio_C_04', 'ratio_C_03', 'ratio_C_08', 'ratio_MA_04', 'ratio_CA_13', 'ratio_CA_25', 'ratio_CA_11', 'ratio_CA_12', 'ratio_MA_02', 'ratio_MA_01', 'ratio_MA_03', 'ratio_RE_01', 'ratio_RE_03', 'ratio_L_04', 'ratio_LE_15', 'ratio_LE_24', 'ratio_L_03')
analisis.nombre_indicadores <- c('Indice_de_Capitalización_Neto',
                        'Cartera_Improductiva_sobre_Patrimonio',
                        'Pat.Tec.Constituido_sobre_Act.Ponderados_por_Riesgos',
                        'Margen_Intermediación_Estimado_sobre_Patrimonio_Promedio',
                        'Morosidad_Cartera_Total',
                        'Cobertura_Cartera_Problematica',
                        'Activos_Productivos_sobre_Total_Activos',
                        'Activos_Productivos_sobre_Pasivos_con_Costo',
                        'Grado_de_Absorción',
                        'Gastos_Operacional_Estimados_sobre_Activo_Total_Promedio',
                        'Gastos_Personal_sobre_Activo_Total_Promedio',
                        'ROE',
                        'ROA',
                        'Fondos_Disponibles_sobre_Total_Depósitos_Corto_Plazo',
                        'Liquidez_1era_Linea',
                        'Liquidez_2da_Linea',
                        'Activos_Liquidos_sobre_Pasivo_Exigible')

data.filtro <- data.frame(analisis.indicadores, analisis.prefix_indicadores, analisis.nombre_indicadores)
colnames(data.filtro) <- c("indicadores", "prefijo", "nombre")

# Cargar datos -----------------------------------------------------------------

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
  data.ifis <- data.ifis %>%
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
  glimpse(data.ifis)
}

if(exists("data.ifis")){
  
  data.analisis <- data.ifis %>% 
    filter(
      anio == analisis.anio & mes == analisis.mes
    ) %>% filter(
      codigo_ratio_financiero %in% analisis.indicadores
    ) %>% pivot_wider( 
      names_from = 'codigo_ratio_financiero', 
      values_from = 'valor', 
      names_prefix = 'ratio_' , 
    ) %>% select(
      -c('nombre_ratio_financiero')
    ) %>% group_by(
      codigo_ifi, nombre_ifi, fecha_fin_mes, dia, mes, anio, tipo_ifi, segmento
    ) %>% summarise_all(sum, na.rm = TRUE)
  
  
  # Eliminación de IFIs cerradas (Sin información)
  temporal <- data.analisis %>% 
                filter( ratio_C_04 == 0 
                        & ratio_C_03 == 0 
                        & ratio_C_08 == 0 
                        & ratio_MA_04 == 0 
                        & ratio_CA_13 == 0 
                        & ratio_CA_25 == 0 
                        & ratio_CA_11 == 0 
                        & ratio_CA_12 == 0 
                        & ratio_MA_02 == 0 
                        & ratio_MA_01 == 0 
                        & ratio_MA_03 == 0 
                        & ratio_RE_01 == 0 
                        & ratio_RE_03 == 0 
                        & ratio_L_04 == 0 
                        & ratio_LE_15 == 0 
                        & ratio_LE_24 == 0 
                        & ratio_L_03 == 0
                )
  ifis_to_remove <- base::as_vector(temporal %>% select(codigo_ifi))
  data.analisis <- data.analisis %>% filter( !codigo_ifi %in% ifis_to_remove )
  
  # Creación de variables dummy para tipo de institución financiera y segmento de cooperativas
  data.analisis <- dummy_cols(data.analisis, select_columns = c("tipo_ifi", "segmento")
                              ) %>% 
                    select( -c("tipo_ifi", "segmento")) 
  
  # Reemplazar los NA por ceros en las variables dummy de los segmentos
  data.analisis$segmento_1[is.na(data.analisis$segmento_1)] <- 0
  data.analisis$segmento_2[is.na(data.analisis$segmento_2)] <- 0
  data.analisis$segmento_3[is.na(data.analisis$segmento_3)] <- 0
  # Eliminación de columna dummy "segmento_NA"
  data.analisis <- data.analisis %>% select( -('segmento_NA'))
  
  # Tratamiento 2: Pasamos el nombre de las IFI al nombre de cada fila
  data.analisis <- textshape::column_to_rownames(data.analisis, loc = 2)

}

