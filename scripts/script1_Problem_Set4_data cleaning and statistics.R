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