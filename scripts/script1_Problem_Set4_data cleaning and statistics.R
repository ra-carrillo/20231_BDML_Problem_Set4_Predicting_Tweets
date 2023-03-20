#-------------------------------------------------------------------------------
# Titulo: "Predicción de Tweets"
# Subtitulo: Problem Set 4
# Curso: "Big Data y Machine Learning"
#-------------------------------------------------------------------------------
# Autores:
# Diana Yarith Higuera Cogua
# Juan Sebastian Vallejo Triana
# Lizbeth Karina Hernandez Noriega
# Riky Andrés Carrillo Cadena

# Creado: 2023/03/12
# Última modificación: 2023/03/20
# Descripción: Este codigo permite realizar la limpieza y preparación de la base
#              de datos utilizada para la predicción de tweets.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Parte 1: Preparación de las bases de datos y estadísticas descriptivas
#-------------------------------------------------------------------------------

#--- Limpieza del entorno de trabajo

rm(list=ls())

# Eliminar todas las variables (objetos) previamente creadas y almacenadas 
# en la memoria del sistema, liberando espacio para nuevos objetos o para evitar
# confusiones entre objetos con el mismo nombre.

#--- Librerias a utilizar

require("pacman")
p_load(tm, # procesar,limpiar y analizar datos de texto
       tidyverse,stringi, tidytext, stopwords, wordcloud2, udpipe,
       ggcorrplot)

#--- Establecer el directorio de la carpeta de datos

setwd("C:/Users/andre/OneDrive/Github/Repositorios/20231_BDML_Problem_Set4_Predicting_Tweets/data/raw")


#--- Importar base de datos : train y test

  raw_train <- read.csv2("train.csv", 
                     sep = ",", 
                     dec = ".", 
                     fileEncoding = "UTF-8")
  
  raw_test <- read.csv2("test.csv", 
                        sep = ",", 
                        dec = ".", 
                        fileEncoding = "UTF-8")

#--- Visualizar las dimensiones de cada matriz de datos [train-test]

  dim(raw_train)  # Filas=9349 x Columnas=3
  dim(raw_test)   # Filas=1500 x Columnas=2
  
  
#--- Visualizar matriz de datos (En una pestaña) [db: raw_train]
  
  View(raw_train)
  
# La data:raw_train contiene tres variables: 
#  1) id: identificador del tweet
#  2) name: nombre del autor del tweet
#  3) text: contenido textual del tweet
  
  