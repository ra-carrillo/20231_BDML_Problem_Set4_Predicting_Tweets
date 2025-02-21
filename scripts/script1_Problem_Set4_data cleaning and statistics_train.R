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
       ggcorrplot, janitor, RColorBrewer, wordcloud)

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
  

#---- Análisis descriptivo
  
#---Exploración de la base
  
  # Número de palabras utilizadas por cada autor
  words %>% group_by(autor) %>% summarise(n = n()) 
  
  words %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + 
    labs(x = "Autor", y = "Número de palabras") + 
    theme_bw() 
  
  
#--- Longitud media de los tweets por autor
  
  words %>% group_by(autor, id2) %>% summarise(longitud = n()) %>%                       
    group_by(autor) %>% summarise(media_longitud = mean(longitud),
                                 sd_longitud = sd(longitud))
  
  words %>% group_by(autor, id2) %>% summarise(longitud = n()) %>%                      
    group_by(autor) %>%
    summarise(media_longitud = mean(longitud),
              sd_longitud = sd(longitud)) %>%
    ggplot(aes(x = autor, y = media_longitud)) +
    geom_col() +
    geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                      ymax = media_longitud + sd_longitud)) +
    labs(x = "Autor", y = "Longitud media/Tweet") + 
    coord_flip() + theme_bw() 
  
  # Los tweets de Uribe y Petro son similares en longitud media y desviación, mostrando ésta última que suelen alternar entre tweets largos y cortos. 
  # Los tweets de López son más extensos y con menor desviación.   
  
  ### Gráfica de las frecuencia de palabras por autor
  words %>% group_by(autor, word) %>% count(word) %>% group_by(autor) %>%
    top_n(10, n) %>% arrange(autor, desc(n)) %>%
    ggplot(aes(x = reorder(word,n), y = n, fill = autor)) +
    geom_col() +
    theme_bw() +
    labs(y = "", x = "") +
    theme(legend.position = "none") +
    coord_flip() +
    facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)
  
  #----- Lematización y matriz TF-IDF
  
  ### Lematización
  m<-udpipe_download_model(language = "spanish")
  model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
  palabras_unicas <- words %>%
    distinct(word)
  udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
  udpipe_results <- as_tibble(udpipe_results)
  udpipe_results <- udpipe_results %>% 
    select(token, lemma) %>%
    rename("word" = "token")
  words <- words %>%
    left_join(udpipe_results, by = "word", multiple = "all")
  words[is.na(words$lemma), "lemma"] <- words[is.na(words$lemma), "word"]
  
  #--- Palabras menos comunes
  words %>%
    count(lemma) %>%
    arrange(desc(n)) %>%
    tail(100)
  
  #--- Palabras más comunes
  words %>%
    count(lemma) %>%
    arrange(desc(n)) %>%
    head()
  
  #--- Validación de palabras únicas
  length(unique(words$lemma))  # 17662
  
  ### Para reducir el tamaño de la matriz TF-IDF
  #se eliminan todas las palabras que estén menos de 10 veces
  palabras_eliminar <- words %>%
    count(lemma) %>%
    filter(n < 10)
  
  words <- words %>%
    anti_join(palabras_eliminar, by = "lemma") 
  
  ### Se ajusta el texto para volver al formato original (Tweet por fila)
  data_clean <- words %>%
    group_by(id2,autor,id) %>% 
    summarise(comentario = str_c(lemma, collapse = " ")) %>%
    ungroup()
  
  setdiff(raw_train$id, data_clean$id)
  raw_train[c(9287, 9349),]
  
  ### Se crea un corpus
  tm_corpus <- Corpus(VectorSource(x = data_clean$comentario))
  str(tm_corpus) # 9287 comentarios
  
  ### Se crea la matriz TF-IDF
  tf_idf <- TermDocumentMatrix(tm_corpus,
                               control = list(weighting = weightTfIdf))
  
  tf_idf <- tf_idf %>%
    as.matrix() %>%
    t() %>%
    as.data.frame()
  
  ### Validación del tamaño de la matriz
  dim(tf_idf) # 9287 x 2517
  
  
  #--- Establecer el directorio de la carpeta de datos
  
  setwd("C:/Users/andre/OneDrive/Github/Repositorios/20231_BDML_Problem_Set4_Predicting_Tweets/data/work")
  
  ### Se guarda la base final de train
  save(tf_idf, 
       file = "train_model.RData")
  
  