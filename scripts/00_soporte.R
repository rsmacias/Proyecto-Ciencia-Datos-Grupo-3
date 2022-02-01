
# Cargar paquetes --------------------------------------------------------------

library(tidyverse)


# Funciones --------------------------------------------------------------------

## Objetivo: Graficar los diagramas Boxplots para cada indicador pasado por parámetro
graficar_boxplots <- function(data.analisis, analisis.prefix_indicadores, columnas = 6:22) {
  # Visualizar los outliers en diagramas boxplots por cada indicador 
  for (indicador in analisis.prefix_indicadores) {
    boxplot(data.analisis[[indicador]], horizontal = TRUE, main = indicador)
  }
  
  # Visualizar los outliers en diagramas boxplots en un único diagrama
  data.analisis %>% 
    pivot_longer(cols = columnas, names_to = "indicadores", values_to = "valores") %>% 
    ggplot2::ggplot(data = ., aes(x = indicadores, y = valores, fill = valores)) +
    geom_jitter(aes(color = indicadores), width=0.1,alpha=0.7) +
    geom_boxplot(aes(color = indicadores), alpha=0.5) + 
    xlab('Indices Financieros') + 
    ylab('Valor') +
    ggtitle('Ratios Financieros de IFIS') +
    theme_minimal()
}

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

## Objetivo: Remover outliers o imputarlos con la media y mediana
remove_outliers_v2 <- function(df, cols = names(df), replace = FALSE) {
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
      mediana <- median(df_temp[[col]])
      media <- mean(df_temp[[col]])
      print(cat('MEDIANA: ', mediana, fill = TRUE))
      print(cat('MEDIA: ', media, fill = TRUE))
      if( total_outliers > 0 ) {
        
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
          
          if(replace == TRUE) {
            # Reemplaza los outliers por la mediana
            df_temp[df_temp[[col]] >= limite_superior, col] = mediana
          } else {
            df_temp <- df_temp %>% filter( !!sym(col) < limite_superior)     
          }  
        }
        
        if(length(outliers_bajos) > 0) {
          # Hay outliers por debajo del bigote inferior
          print(cat('Outliers Bajos: ', sort(outliers_bajos, decreasing = FALSE), fill = TRUE))
          #print(sort(outliers_bajos, decreasing = FALSE))
          limite_inferior <- max(outliers_bajos)
          print(cat('LIMITE INFERIOR: ', limite_inferior, fill = TRUE))
          if(replace == TRUE) {
            # Reemplaza los outliers por la media
            df_temp[df_temp[[col]] <= limite_inferior, col] = media
          } else {
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

## Objetivo: Reemplazar nombre de columnas
pretty_name_ratio_columns <- function(df_original, df_nombres) {
  df_temp <- df_original
  
  for (i in 1:nrow(df_nombres)){
    names(df_temp)[names(df_temp) == df_nombres[i,]$indicadores] <- df_nombres[i,]$nombre_indicadores
  }
  
  df_temp
}