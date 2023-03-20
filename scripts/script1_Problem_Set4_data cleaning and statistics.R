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
  
  
#--- Visualizar primera fila de la base de datos [db: raw_train]
  
  head(raw_train, 1)
  
#--- Visualizar última fila de la base de datos [db: raw_train]
  
  tail(raw_train, 1)
  
#--- Explorar el tipo de variable de la base de datos [db: raw_train]
  
  glimpse(raw_train)  
  
# Nota: Las tres varaibles incluidas en la db:raw_train son variables tipo "character"
  
#--- Preparando los datos 
  
#--- Renombrar varaibles
  raw_train <- rename(raw_train, c("autor" = "name"))
  raw_train <- rename(raw_train, c("tweet" = "text"))
  raw_test <- rename(raw_test, c("tweet" = "text"))
  
  # Antes de vectorizar vamos a limpiar y homogenizar el texto que aparece en 
  # la variable tweet.
 
#--- Limpieza de texto
  
#--- Número de tweets por autor
  
  table(raw_train$autor) / length(raw_train$autor) # proporción similar para cada autor
  
#--- Limpieza de la variable tweet 
  
  raw_train["tweet"] <- apply(raw_train["tweet"], 1, tolower)             # tweet en minúscula 
  raw_train["tweet"] <- apply(raw_train["tweet"], 1, removeNumbers)       # eliminar números
  raw_train["tweet"] <- apply(raw_train["tweet"], 1, removePunctuation)   # eliminar puntuación
  raw_train["tweet"] <- apply(raw_train["tweet"], 1, function(x)          # eliminar acentos
  stri_trans_general(str = x, id = "Latin-ASCII"))
  
  raw_train <- raw_train %>% 
    mutate(tweet = gsub('@|#', '', raw_train$tweet))
  
  raw_train <- raw_train %>% 
    mutate(tweet = gsub('!|¡', '', raw_train$tweet))
  
  raw_train <- raw_train %>% 
    mutate(tweet = gsub('¿|?', '', raw_train$tweet))
  
  raw_train <- raw_train %>% 
    mutate(tweet = gsub('"', '', raw_train$tweet))
  
  raw_train["tweet"] <- apply(raw_train["tweet"], 1, stripWhitespace)     # eliminar múltiples espacios en blanco

#---Se transforma el dataframe a nivel de palabras, previa creación de un id para cada tweet 
  raw_train <- raw_train %>%
    mutate(id2 = row_number())
  
  words <- raw_train %>%
    unnest_tokens(output = "word", input = "tweet")

#---Se eliminan las stopwords
  sw <- c()
  for (s in c("snowball", "stopwords-iso", "nltk")) {
    temp <- get_stopwords("spanish", source = s)$word
    sw <- c(sw, temp)
  }
  sw <- unique(sw)
  sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
  sw <- data.frame(word = sw)  
  
#--- Número de palabras antes de eliminar stopwords
  nrow(words)      # 290298 palabras
  
  words <- words %>%
    anti_join(sw, by = "word")
  
  ### Número de palabras después de eliminar stopwords
  nrow(words)     # 139097 palabras
  
