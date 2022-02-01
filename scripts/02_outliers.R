
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)
library(fastDummies)





# Funciones --------------------------------------------------------------------

## Objetivo: Remover outliers o reemplazarlo por NA
remove_outliers <- function(df, cols = names(df), replaceWithNa = FALSE) {
  df_temp <- df
  acum <- 0
  for (col in cols) {
    print(cat('Variable: ', col, fill = TRUE))
    repeat { 
      outliers <- boxplot.stats(df_temp[[col]])$out
      print(cat('Outliers >> ', outliers, fill = TRUE))
      total_outliers <- length(outliers)  
      acum <- acum + total_outliers  
      print(cat('TOTAL Outliers:', total_outliers, fill = TRUE ))
      if( total_outliers > 0 ) {
        
        if(replaceWithNa == TRUE) {
          # Reemplaza los outliers por NA
          df_temp[df_temp[[col]] %in% outliers, col] = NA 
        } else {
          # Remueve la fila (observación) con el outlier
          bigote_inferior <- boxplot.stats(df_temp[[col]])$stats[1]
          bigote_superior <- boxplot.stats(df_temp[[col]])$stats[5]
          
          outliers_altos <- outliers[outliers > bigote_superior]
          outliers_bajos <- outliers[outliers < bigote_inferior]
          
          if(length(outliers_altos) > 0) {
            # Hay outliers por encima del bigote superior
            limite_superior <- min(outliers_altos)
            print(cat('Outliers Altos: ', sort(outliers_altos, decreasing = FALSE), fill = TRUE))
            print(cat('LIMITE SUPERIOR: ', limite_superior, fill = TRUE))
            df_temp <- df_temp %>% filter( !!sym(col) < limite_superior)   
          }
          
          if(length(outliers_bajos) > 0) {
            # Hay outliers por debajo del bigote inferior
            print(cat('Outliers Bajos: ', sort(outliers_bajos, decreasing = FALSE), fill = TRUE))
            #print(sort(outliers_bajos, decreasing = FALSE))
            limite_inferior <- max(outliers_bajos)
            print(cat('LIMITE INFERIOR: ', limite_inferior, fill = TRUE))
            df_temp <- df_temp %>% filter( !!sym(col) > limite_inferior)   
          }  
        }
      } else {
        print(cat('TOTAL_ELIMINADOS:', acum, fill = TRUE))
        break
      }
    }
  }
  df_temp
}

## Objetivo: Esclar las columnas de un dataframe
normalizar <- function(df, cols = names(df)) {
  df_temp <- df
  for (col in cols) {
    df_temp[[col]] <- scale(x = df_temp[[col]], center = TRUE, scale = TRUE)
  }
  df_temp 
}
