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

######################################## Set up ######################################################

###### Limpiar el espacio de trabajo

rm(list=ls())

##### Cargar paquetes necesarios

require(pacman)

p_load(ggplot2, highcharter,
       ggExtra, gridExtra, topicmodels,
       wordcloud,wordcloud2, textstem,
       tidyr, tm, stringr, stopwords,
       stringi, tidytext, tidyverse, 
       udpipe) 

##### Cargar funciones

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))

##### Cargar bases de datos #####

### Definir ubicación de los archivos sin comprimir (Cambia según PC)

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Raw")


Tweets_Train_Data <- read.csv2("train.csv", 
                               sep = ",", 
                               dec = ".", 
                               fileEncoding = "UTF-8")

Tweets_Test_Data <- read.csv2("test.csv", 
                              sep = ",", 
                              dec = ".", 
                              fileEncoding = "UTF-8")


######################################## Woodwork ######################################################

##### Capitalizar el nombre de las variables 

### Train

names(Tweets_Train_Data) <- proper(names(Tweets_Train_Data)) 

### Test

names(Tweets_Test_Data) <- proper(names(Tweets_Test_Data)) 

########## Crear nuevas variables ##########


##### Data entrenamiento #####

Tweets_Train_Data_Final <- Tweets_Train_Data %>%
  mutate(
    Emoji = case_when(
      stri_detect_regex(Text, "\\p{Emoji}") == TRUE ~ 1,
      TRUE ~ 0
    ),
    Excl.Mark = case_when(
      grepl("!", Text) == TRUE ~ 1,
      TRUE ~ 0
    ),
    No.Words = lengths(strsplit(Text, ' ')),
    No.Characters = str_length(Text),
    Users = as.character(str_extract_all(Text, "@\\w+")),
    No.Users = str_count(Users, "@"),
    Hash = as.character(str_extract_all(Text, "#\\w+")),
    No.Hash = str_count(Hash, "#")
  )

##### NAs Check

Tweets_Train_Data_Final %>% 
  map(is.na) %>% 
  map(sum) %>%
  map(~./nrow(Tweets_Train_Data_Final)*100) %>% 
  map(round) %>% 
  bind_cols()


##### Data evaluacion #####

Tweets_Test_Data_Final <- Tweets_Test_Data %>%
  mutate(
    Emoji = case_when(
      stri_detect_regex(Text, "\\p{Emoji}") == TRUE ~ 1,
      TRUE ~ 0
    ),
    Excl.Mark = case_when(
      grepl("!", Text) == TRUE ~ 1,
      TRUE ~ 0
    ),
    No.Words = lengths(strsplit(Text, ' ')),
    No.Characters = str_length(Text),
    Users = as.character(str_extract_all(Text, "@\\w+")),
    No.Users = str_count(Users, "@"),
    Hash = as.character(str_extract_all(Text, "#\\w+")),
    No.Hash = str_count(Hash, "#")
  )

summary(Tweets_Test_Data_Final)

##### NAs Check

Tweets_Test_Data_Final %>% 
  map(is.na) %>% 
  map(sum) %>%
  map(~./nrow(Tweets_Train_Data_Final)*100) %>% 
  map(round) %>% 
  bind_cols()



########## Limpieza variable de texto ##########

##### Data entrenamiento #####

Tweets_Train_Data_Final$Text.Clean <- Tweets_Train_Data_Final$Text %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub='') %>%
  removeWords(stopwords(language = "es", 
                        source = "snowball")) %>% 
  removeWords(stopwords(language = "es", 
                        source = "nltk")) %>% 
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace() %>% 
  tolower()
#removeWords(Empty_Words)

# Remove white spaces

Tweets_Train_Data_Final <- Tweets_Train_Data_Final[!(Tweets_Train_Data_Final$Text.Clean==""),]

Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>%
  mutate(
    Hash = case_when(
      Hash == "character(0)" ~ NA,
      TRUE ~ Hash
    ),
    Users = case_when(
      Users == "character(0)" ~ NA,
      TRUE ~ Users
    )
  )



##### Data evaluacion #####

Tweets_Test_Data_Final$Text.Clean <- Tweets_Test_Data_Final$Text %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub='') %>%
  removeWords(stopwords(language = "es", 
                        source = "snowball")) %>% 
  removeWords(stopwords(language = "es", 
                        source = "nltk")) %>% 
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace() %>% 
  tolower()
#removeWords(Empty_Words)


Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>%
  mutate(
    Hash = case_when(
      Hash == "character(0)" ~ NA,
      TRUE ~ Hash
    ),
    Users = case_when(
      Users == "character(0)" ~ NA,
      TRUE ~ Users
    )
  )




######################################### Analysis ######################################


##########  DE #########

##### Diferencias en el número de Tweets #####

Tweets_Train_Data_Final %>% 
  group_by(Name) %>% 
  tally()

# La muestra se encuentra levemente imbalanceada hacia Claudia.

##### Diferencia en extensión de los tweets #####

### Words

hcboxplot(
  var = Tweets_Train_Data_Final$Name,
  x = Tweets_Train_Data_Final$No.Words,
  outliers = FALSE,
  color = "#2980b9"
) %>% 
  hc_chart(type = "column")

# Claudia tiene tweets más largos en promedio y con menor variación.


### Characters

hcboxplot(
  var = Tweets_Train_Data_Final$Name,
  x = Tweets_Train_Data_Final$No.Characters,
  outliers = FALSE,
  color = "#2980b9"
) %>% 
  hc_chart(type = "column")

# Se alarga la diferencia y se reduce la variación para Claudia y Petro, pero aumenta para Uribe.

##### Número de hashtags utilizados en sus tweets #####

prop.table(table(Tweets_Train_Data_Final$Name,
                 Tweets_Train_Data_Final$No.Hash), 1)

hcboxplot(
  var = Tweets_Train_Data_Final$Name,
  x = Tweets_Train_Data_Final$No.Hash,
  outliers = FALSE,
  color = "#2980b9") %>% 
  hc_chart(type = "column",
           zoomType = "y") 


Tweets_Train_Data_Final %>% 
  group_by(Name) %>% 
  summarise(
    Min.Value = min(No.Hash),
    Mean.Value = mean(No.Hash),
    Max.Value = max(No.Hash)
  )

# Un mayor uso promedio de hashtags por parte de Claudia

##### Número de usuarios mencionados en sus tweets #####

prop.table(
  table(Tweets_Train_Data_Final$Name,
        Tweets_Train_Data_Final$No.Users), 1)

hcboxplot(
  var = Tweets_Train_Data_Final$Name,
  x = Tweets_Train_Data_Final$No.Users,
  outliers = FALSE,
  color = "#2980b9") %>% 
  hc_chart(type = "column",
           zoomType = "y") 



Tweets_Train_Data_Final %>% 
  group_by(Name) %>% 
  summarise(
    Min.Value = min(No.Users),
    Mean.Value = mean(No.Users),
    Max.Value = max(No.Users),
  )

# Un mayor número de menciones a usuarios por parte de Claudia, seguido de Uribe.



##########  Analisis de Sentimientos #####

Tweets_Tidy <- Tweets_Train_Data_Final %>% 
  unnest_tokens(Word, Text)

##### Limpieza de la variable de texto #####

Empty_Words <- c()

Tweets_Tidy$Word <- Tweets_Tidy$Word %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
  removeWords(stopwords("spanish")) %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>% 
  stripWhitespace()
#removeWords()

# Remove white spaces

Tweets_Tidy <- Tweets_Tidy[!(Tweets_Tidy$Word==""),]


##########  Analisis por monogramas #####

Word_Data <- Tweets_Train_Data_Final %>% 
  unnest_tokens(Word, Text)




##### Limpieza de la variable de texto #####

Empty_Words <- c()

Word_Data$Word <- Word_Data$Word %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
  removeWords(stopwords("spanish")) %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>% 
  stripWhitespace()
#removeWords()

# Remove white spaces

Word_Data <- Word_Data[!(Word_Data$Word==""),]

##### Palabras más frecuentes #####

### Palabras más frecuentes para los 3 políticos #####

Word_Data_General_Fq <- Word_Data %>%
  count(Word, sort = TRUE)

### Word Cloud

wordcloud2(Word_Data_General_Fq[c(1:100), ], 
           size = 0.5, 
           color='random-dark', 
           maxRotation = -pi/4)

### Bar plot

Word_Data_General_Fq %>% 
  hchart('column', 
         hcaes(x = Word, 
               y = n),
         name = "Frecuencia") %>% 
  hc_colors(c("#006600")) %>% 
  hc_title(text = "Top 20 palabras más repetidas",
           style = list(fontWeight = "bold",
                        fontSize = "18px"),
           align = "left") %>%
  hc_caption(text = "Fuente: Data Twitter. Cálculos propios.",
             style = list(fontSize = "10px"),
             align = "left") %>%
  hc_yAxis(title = list(text = "Frecuencia")) %>%
  hc_xAxis(title = list(text = " ")) %>%
  hc_exporting(enabled = TRUE)


### Palabras más frecuentes por político #####

Word_Data_User_Fq <- Word_Data %>%
  count(Name,  
        Word, 
        sort = TRUE)

# Relativo a la frecuencia general, la palabra Bogotá, USD, ciudad, vacunación y onza fuertes palabras de clasificación para creación de dummies
# La palabra hoy, jovenes, familia, salud y seguridad menos poder de clasificación pero pueden probarse

## Filtrar missings

Word_Data_User_Fq <- Word_Data_User_Fq %>% 
  filter(!is.na(Name))

### Top 5

Zoom <- Word_Data_User_Fq %>%
  group_by(Name) %>% 
  top_n(5, n) %>% 
  arrange(Name, n) %>% 
  ungroup()

### Palabras totales por politico

Total_Words <- Word_Data_User_Fq %>% 
  group_by(Name) %>% 
  summarize(Total = sum(n, 
                        na.rm = TRUE))


User_Words <- left_join(Word_Data_User_Fq, 
                        Total_Words)

User_Words <- User_Words %>% 
  mutate(
    Per = round(n/Total*100,1)
  )

### Grafica

ggplot(User_Words, aes(Per, 
                       fill = Name)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~Name, ncol = 2, scales = "free_y")

### Tf - Idf score #####

Tweets_Tf_Idf <- User_Words %>%
  bind_tf_idf(Word, 
              Name, n)


Zoom <- Tweets_Tf_Idf %>%
  group_by(Name) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() 


C <- Tweets_Tf_Idf %>% 
  filter(Name == "Lopez")

P <- Tweets_Tf_Idf %>% 
  filter(Name == "Petro")

U <- Tweets_Tf_Idf %>% 
  filter(Name == "Uribe")

hchart(
  density(C$n), 
  type = "area", 
  color = "#f9bc60", 
  name = "Claudia Lopez"
) %>%
  hc_add_series(
    density(P$n), 
    type = "area",
    color = "#abd1c6", 
    name = "Gustavo Petro"
  )%>%
  hc_add_series(
    density(U$n), type = "area",
    color = "#e16162", 
    name = "Alvaro Uribe"
  ) %>% 
  hc_chart(
    zoomType = "x") 


########## Analisis Hasgtags y Usuarios etiquetados #########


##### Hashtags #####

Hash_Data <- Tweets_Train_Data_Final %>% 
  unnest_tokens(Word, Hash)

### Limpieza de la variable de texto #####

Hash_Data$Word <- Hash_Data$Word %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
  removeWords(stopwords("spanish")) %>%
  removePunctuation() %>%
  tolower() %>% 
  stripWhitespace() %>% 
  removeWords("c")

# Remove white spaces

Hash_Data <- Hash_Data[!(Hash_Data$Word==""),]

Hash_Data <- Hash_Data %>% 
  filter(!is.na(Word))

### Hashtags más frecuentes para los 3 políticos #####

Hashtags_General_Fq <- Hash_Data %>%
  count(Word, sort = TRUE)

### Hashtags más frecuentes de cada uno de los 3 políticos #####

Hashtags_User_Fq <- Hash_Data %>%
  count(Name,Word, sort = TRUE)




##### Usuarios mencionados #####

Hash_Data <- Tweets_Train_Data_Final %>% 
  unnest_tokens(Word, Hash)


Users_Data <- Tweets_Train_Data_Final %>% 
  unnest_tokens(Word, Users)

### Limpieza de la variable de texto #####

Users_Data$Word <- Users_Data$Word %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
  removeWords(stopwords("spanish")) %>%
  removePunctuation() %>%
  tolower() %>% 
  stripWhitespace() %>% 
  removeWords("c")

# Remove white spaces

Users_Data <- Users_Data[!(Users_Data$Word==""),]

Users_Data <- Users_Data %>% 
  filter(!is.na(Word))

### Usuarios más frecuentes para los 3 políticos #####

Users_General_Fq <- Users_Data %>%
  count(Word, sort = TRUE)

### Usuarios más frecuentes de cada uno de los 3 políticos #####

Mentions_User_Fq <- Users_Data %>%
  count(Name,Word, sort = TRUE)




##########  LDA ##########


Lemma_Data <- Tweets_Train_Data_Final %>% 
  select(Name, Text.Clean)


Tweets = Corpus(VectorSource(Lemma_Data$Text.Clean))

Tweets_L <-tm_map(Tweets,
                  lemmatize_strings)

Dtm_Tweets <-DocumentTermMatrix(Tweets_L)

rowSum <- apply(Dtm_Tweets , 1, sum)
Dtm_Tweets <- Dtm_Tweets[rowSum> 0, ]


Lda_Tweets <- LDA(Dtm_Tweets, 
                  k = 3, 
                  control = 
                    list(seed = 2403))

Lda_Tweets


Tweets_Topics <- tidy(Lda_Tweets, 
                      matrix = "beta")


Tweets_Topics_Top_Terms <- Tweets_Topics %>%
  group_by(topic) %>%
  slice_max(beta, 
            n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

Tweets_Topics_Top_Terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


############################## Export data #######################################33333



########## Crear variables nuevas con base en el analisis anterior ##########################


####### Data entrenamiento

##### Generales #####

Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>% 
  mutate(
    Median.Words = median(No.Words, 
                          na.rm = TRUE),
    Disp.Words = No.Words - Median.Words,
    Median.Characters = median(No.Words, 
                               na.rm = TRUE),
    Disp.Characters = No.Words - Median.Characters
  )


##### Tf-IDF #####

### Se seleccionan las palabras con el tf idf mas alto que el 99% de las palabras
### Esto bajo el supuesto que esas palabras corresponden a palabras unicas del politico en cuestion

## Claudia

Claudia <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & tf_idf > quantile(tf_idf, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

## Petro

Petro <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & tf_idf > quantile(tf_idf, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

## Uribe

Uribe <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & tf_idf > quantile(tf_idf, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


### Se crea el clasificador con base en las tematicas unicas de cada politico

Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>% 
  mutate(
    Tf_Idf_Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Tf_Idf_Topic = as.factor(Tf_Idf_Topic)
  )


### Evaluar la clasificacion

prop.table(table(Tweets_Train_Data_Final$Tf_Idf_Topic,
                 Tweets_Train_Data_Final$Name), 2)

##### Frecuencia palabras #####

Claudia <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & Per > quantile(Per, 0.999)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

Petro <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & Per > quantile(Per, 0.999)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

Uribe <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & Per > quantile(Per, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>% 
  mutate(
    Freq_Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Freq_Topic = as.factor(Freq_Topic)
  )


### Check the classification

prop.table(table(Tweets_Train_Data_Final$Freq_Topic,
                 Tweets_Train_Data_Final$Name), 2)


##### Word Dummies

Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>% 
  mutate(
    Bogota = as.factor(case_when(
      grepl("bogota|ciudad", Text.Clean) ~ 1,
      TRUE ~ 0)),
    Macro = as.factor(case_when(
      grepl("usd|onza", Text.Clean) ~ 1,
      TRUE ~ 0)),
    Vacunacion = as.factor(case_when(
      grepl("vacunacion", Text.Clean) ~ 1,
      TRUE ~ 0)),
    Familia = as.factor(case_when(
      grepl("familia", Text.Clean) ~ 1,
      TRUE ~ 0))
  )

### Revisar clasificacion

## Bogota

prop.table(table(Tweets_Train_Data_Final$Name,
                 Tweets_Train_Data_Final$Bogota), 2)

## Macro 

prop.table(table(Tweets_Train_Data_Final$Name,
                 Tweets_Train_Data_Final$Macro), 2)


## Vacunacion

prop.table(table(Tweets_Train_Data_Final$Name,
                 Tweets_Train_Data_Final$Vacunacion), 2)

## Familia

prop.table(table(Tweets_Train_Data_Final$Name,
                 Tweets_Train_Data_Final$Familia), 2)

##### Hashtags ##### 

### Se seleccionan los hashtags que se repiten mas que el 95% de las hashtags
### Esto bajo el supuesto que estos hashtags son unicos para cada politico

## Claudia

Claudia <- Hashtags_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & n > quantile(n, 0.98)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

## Petro

Petro <- Hashtags_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & n > quantile(n, 0.95)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

## Uribe

Uribe <- Hashtags_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & n > quantile(n, 0.96)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


### Se crea el clasificador con base en las tematicas unicas de cada politico

Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>% 
  mutate(
    Hashtag.Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Hashtag.Topic = as.factor(Hashtag.Topic)
  )


### Evaluar la clasificacion

prop.table(table(Tweets_Train_Data_Final$Hashtag.Topic,
                 Tweets_Train_Data_Final$Name), 1)



##### Menciones Usuarios ##### 

### Se seleccionan las menciones que se repiten mas que el 95% de las menciones de cada politico
### Esto bajo el supuesto que estas menciones son unicas para cada politico

## Claudia

Claudia <- Mentions_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & n > quantile(n, 0.98)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

## Petro

Petro <- Mentions_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & n > quantile(n, 0.95)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

## Uribe

Uribe <- Mentions_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & n > quantile(n, 0.96)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


### Se crea el clasificador con base en las tematicas unicas de cada politico

Tweets_Train_Data_Final <- Tweets_Train_Data_Final %>% 
  mutate(
    Mentions.Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Mentions.Topic = as.factor(Mentions.Topic)
  )

### Evaluar la clasificacion

prop.table(table(Tweets_Train_Data_Final$Mentions.Topic,
                 Tweets_Train_Data_Final$Name), 2)

##### Texto Lematizado #####

### Modelo de lematizacion en español

Sp_Model <- udpipe_download_model(language = "spanish")

Sp_Model <- udpipe_load_model(Sp_Model$file_model)


### Funcion de lematizacion

Annotations <- udpipe_annotate(Sp_Model, 
                               x = Tweets_Train_Data_Final$Text.Clean)

# Convertir el producto de la lemmaztizacion en un data frame

Annotations_Df <- as.data.frame(Annotations)

# Unir los lemmas en la string original

Annotations_Df <- Annotations_Df %>% 
  group_by(doc_id) %>% 
  mutate(Lemmatized.Tweet = paste(lemma, collapse = " "))

### Crear variables con la identificacion de palabras (Upos)

Annotations_Df <- Annotations_Df %>% 
  mutate(Noun = ifelse(
    upos == "NOUN", 
    1,
    0),
    Adjetive = ifelse(
      upos == "ADJ", 
      1,
      0),
    Verb = ifelse(
      upos == "VERB", 
      1,
      0),
    Other = ifelse(
      !(upos %in% c("NOUN",'ADJ','VERB')), 
      1,
      0)
  )

## Suma de los POS tags por Tweet

Annotations_Df <- Annotations_Df %>% 
  group_by(doc_id) %>% 
  mutate(
    No.Noun = sum(Noun, 
                  na.rm = TRUE),
    No.Adjs = sum(Adjetive, 
                  na.rm = TRUE),
    No.Verbs = sum(Verb, 
                   na.rm = TRUE),
    No.Other = sum(Other, 
                   na.rm = TRUE)
  ) %>% 
  ungroup()

### Unir con la base original para exportar

Tweets_Train_Data_Final <- Tweets_Train_Data_Final[!(Tweets_Train_Data_Final$Text.Clean==""),]


Annotations_F <- Annotations_Df %>% 
  filter(token_id == 1) %>% 
  select("Text.Clean.L" = sentence, Lemmatized.Tweet,
         No.Noun:No.Other)

Model_Data_Export <- cbind(Tweets_Train_Data_Final, 
                           Annotations_F)




####### Data test ##########

##### Generales #####

Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>% 
  mutate(
    Median.Words = median(No.Words, 
                          na.rm = TRUE),
    Disp.Words = No.Words - Median.Words,
    Median.Characters = median(No.Words, 
                               na.rm = TRUE),
    Disp.Characters = No.Words - Median.Characters
  )

##### Tf-IDF #####

### Se seleccionan las palabras con el tf idf mas alto que el 99% de las palabras
### Esto bajo el supuesto que esas palabras corresponden a palabras unicas del politico en cuestion

## Claudia

Claudia <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & tf_idf > quantile(tf_idf, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

## Petro

Petro <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & tf_idf > quantile(tf_idf, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

## Uribe

Uribe <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & tf_idf > quantile(tf_idf, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


### Se crea el clasificador con base en las tematicas unicas de cada politico

Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>% 
  mutate(
    Tf_Idf_Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Tf_Idf_Topic = as.factor(Tf_Idf_Topic)
  )


##### Frecuencia palabras #####

Claudia <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & Per > quantile(Per, 0.999)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

Petro <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & Per > quantile(Per, 0.999)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

Uribe <- Tweets_Tf_Idf %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & Per > quantile(Per, 0.99)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>% 
  mutate(
    Freq_Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Freq_Topic = as.factor(Freq_Topic)
  )


##### Word Dummies

Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>% 
  mutate(
    Bogota = as.factor(case_when(
      grepl("bogota|ciudad", Text.Clean) ~ 1,
      TRUE ~ 0)),
    Macro = as.factor(case_when(
      grepl("usd|onza", Text.Clean) ~ 1,
      TRUE ~ 0)),
    Vacunacion = as.factor(case_when(
      grepl("vacunacion", Text.Clean) ~ 1,
      TRUE ~ 0)),
    Familia = as.factor(case_when(
      grepl("familia", Text.Clean) ~ 1,
      TRUE ~ 0))
  )


##### Hashtags ##### 

### Se seleccionan los hashtags que se repiten mas que el 95% de las hashtags
### Esto bajo el supuesto que estos hashtags son unicos para cada politico

## Claudia

Claudia <- Hashtags_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & n > quantile(n, 0.98)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

## Petro

Petro <- Hashtags_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & n > quantile(n, 0.95)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

## Uribe

Uribe <- Hashtags_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & n > quantile(n, 0.96)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


### Se crea el clasificador con base en las tematicas unicas de cada politico

Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>% 
  mutate(
    Hashtag.Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Hashtag.Topic = as.factor(Hashtag.Topic)
  )


##### Menciones Usuarios ##### 

### Se seleccionan las menciones que se repiten mas que el 95% de las menciones de cada politico
### Esto bajo el supuesto que estas menciones son unicas para cada politico

## Claudia

Claudia <- Mentions_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Lopez" & n > quantile(n, 0.98)) %>%
  ungroup() %>% 
  select(Word) 

Claudia <- as.character(Claudia[["Word"]])

## Petro

Petro <- Mentions_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Petro" & n > quantile(n, 0.95)) %>%
  ungroup() %>% 
  select(Word) 

Petro <- as.character(Petro[["Word"]])

## Uribe

Uribe <- Mentions_User_Fq %>% 
  group_by(Name) %>% 
  filter(Name == "Uribe" & n > quantile(n, 0.96)) %>%
  ungroup() %>% 
  select(Word) 

Uribe <- as.character(Uribe[["Word"]])


### Se crea el clasificador con base en las tematicas unicas de cada politico

Tweets_Test_Data_Final <- Tweets_Test_Data_Final %>% 
  mutate(
    Mentions.Topic = case_when(
      grepl(paste(Claudia, collapse="|"), Text.Clean) ~ "Claudia",
      grepl(paste(Petro, collapse="|"), Text.Clean) ~ "Petro",
      grepl(paste(Uribe, collapse="|"), Text.Clean) ~ "Uribe",
      TRUE ~ "Neutral"),
    Mentions.Topic = as.factor(Mentions.Topic)
  )


##### Texto Lematizado #####

### Funcion de lematizacion

Annotations <- udpipe_annotate(Sp_Model, 
                               x = Tweets_Test_Data_Final$Text.Clean)

# Convertir el producto de la lemmaztizacion en un data frame

Annotations_Df <- as.data.frame(Annotations)

# Unir los lemmas en la string original

Annotations_Df <- Annotations_Df %>% 
  group_by(doc_id) %>% 
  mutate(Lemmatized.Tweet = paste(lemma, collapse = " "))

### Crear variables con la identificacion de palabras (Upos)

Annotations_Df <- Annotations_Df %>% 
  mutate(Noun = ifelse(
    upos == "NOUN", 
    1,
    0),
    Adjetive = ifelse(
      upos == "ADJ", 
      1,
      0),
    Verb = ifelse(
      upos == "VERB", 
      1,
      0),
    Other = ifelse(
      !(upos %in% c("NOUN",'ADJ','VERB')), 
      1,
      0)
  )

## Suma de los POS tags por Tweet

Annotations_Df <- Annotations_Df %>% 
  group_by(doc_id) %>% 
  mutate(
    No.Noun = sum(Noun, 
                  na.rm = TRUE),
    No.Adjs = sum(Adjetive, 
                  na.rm = TRUE),
    No.Verbs = sum(Verb, 
                   na.rm = TRUE),
    No.Other = sum(Other, 
                   na.rm = TRUE)
  ) %>% 
  ungroup()

### Unir con la base original para exportar

Tweets_Test_Data_Final <- Tweets_Test_Data_Final[!(Tweets_Test_Data_Final$Text.Clean==" "),]

Annotations_F <- Annotations_Df %>% 
  filter(token_id == 1) %>% 
  select("Text.Clean.L" = sentence, Lemmatized.Tweet,
         No.Noun:No.Other)

Model_Test_Data_Export <- cbind(Tweets_Test_Data_Final, 
                                Annotations_F)



#################### Exportar la data para correr los modelos ########## 

######### Filtar variables innecesarias para el modelo ##########

##### Data entrenamiento #####

Model_Training_Data <- Model_Data_Export %>% 
  select(-c(Users, Hash, ))

##### Data test #####

Model_Test_Data <- Model_Test_Data_Export %>% 
  select(-c(Users, Hash))

##### Se exportan las bases de datos

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Final")

write.csv2(Model_Training_Data, 
           "Data Entrenamiento Modelo Lem.csv",
           row.names = FALSE )


write.csv2(Model_Test_Data, 
           "Data Test Modelo Lem.csv",
           row.names = FALSE )


##### Objetivo: Predecir la autoria de un pool de tweets entre 3 políticos colombianos

### Parte 2: Modelos de clasificación

# Autores:
# Andres Carrillo
# Diana Higuera
# Juan Sebastian Vallejo
# Lizbeth Hernandez


### Fecha creación : February 20th 2023

### Datos para correr el código: 

# Data Entrenamiento Modelo.cvs
# Data Test Modelo.cvs


###### Limpiar el espacio de trabajo

rm(list=ls())

##### Cargar paquetes necesarios

require(pacman)

p_load(tidyverse, ggplot2,
       ggExtra, tidymodels, themis,
       gridExtra, ranger, tidyr, vip,
       xgboost, finetune,
       yardstick) 

##### Cargar bases de datos #####

### Definir ubicación de los archivos sin comprimir (Cambia según PC)

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Final")


Model_Train_Data <- read.csv2("Data Entrenamiento Modelo Lem.csv", 
                              sep = ";", 
                              dec = ",", 
                              fileEncoding = "UTF-8")

Model_Test_Data <- read.csv2("Data Test Modelo Lem.csv", 
                             sep = ";", 
                             dec = ",", 
                             fileEncoding = "UTF-8")


summary(Model_Train_Data)

summary(Model_Test_Data)

##################################### Woodwork ######################################################

########## Training ##########

##### Definir vector de variables a convertir a string

To_Factor <- c("Bogota", "Macro", "Familia",
               "Vacunacion", "Emoji", "Excl.Mark")

Model_Train_Data[To_Factor] <- lapply(Model_Train_Data[To_Factor], as.character)

##### Convertir variables de texto a factor

Model_Data <- Model_Train_Data %>% 
  mutate_if(
    is.character, factor) 

str(Model_Data)

########## Test ##########

##### Definir vector de variables a convertir a string


Model_Test_Data[To_Factor] <- lapply(Model_Test_Data[To_Factor], as.character)


##### Convertir variables de texto a factor

Test_Final <- Model_Test_Data %>% 
  mutate_if(
    is.character, factor)




##################################### Modelling with Tidymodels ######################################################

########## 1. Definir base de entrenamiento y test, así como método de "resampleo" #####

set.seed(24)

Tweets_Split <- Model_Data %>% 
  initial_split(strata = Name) # Se hace por estratificación para mantener la proporción de la variable Pobre entre submuestras.

Tweets_Training <- training(Tweets_Split)

Tweets_Test <- testing(Tweets_Split) # 74% de la muestra de entrenamiento inicial 

set.seed(2403)

Tweets_Folds <- vfold_cv(Tweets_Training, 
                         strata = Name) # 10 submuestras por default

########## 2. Definir Recetas y forma funcional #####

Recipe_P <- 
  recipe(Name ~ No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Freq_Topic,
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Freq_Topic)

Recipe_1 <- 
  recipe(Name ~ No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic + 
           No.Noun + No.Adjs + No.Verbs + No.Other, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic)

Recipe_2 <- 
  recipe(Name ~ No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic + 
           No.Noun + No.Adjs + No.Verbs + No.Other, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_smote(Name)

Recipe_3 <- 
  recipe(Name ~ No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic + 
           No.Noun + No.Adjs + No.Verbs + No.Other, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_predictors(), 
           keep_original_cols = TRUE,
           threshold = .90) %>% 
  step_smote(Name)



##### Revisar si la base de entrenamiento se está preparando bien

Zoom <- juice(prep(Recipe_3))

summary(Zoom)

########## 3. Definir modelos y el workflow #####

Elastic_Multinom_Spec <- 
  multinom_reg(penalty = 0.0622,
               mixture = 0.0293) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

RF_Spec <-
  rand_forest(trees = 100) %>%
  set_engine("ranger",
             importance = "permutation") %>% 
  set_mode("classification") 

### Workflow

Tweets_Wflow <- workflow_set(
  list("Inicial" = Recipe_P,
       "Referencia" = Recipe_1, 
       "SMOTE - Normalizado" = Recipe_2, 
       "Rec 2 con PCA" = Recipe_3),
  list("Elastic Multinomial" = Elastic_Multinom_Spec, 
       "Random Forest" = RF_Spec))

######### 4. Se corren los modelos ######### 

Tweets_Metrics <- metric_set(accuracy, sensitivity, 
                             specificity, roc_auc)

doParallel::registerDoParallel()
set.seed(240394)

Tweets_RS_Basic <-
  workflow_map(
    Tweets_Wflow,
    "fit_resamples",
    resamples = Tweets_Folds,
    metrics = Tweets_Metrics
  )

########## 5. Seleccionar el mejor modelo con base en la métrica elegida (Accuracy) ########## 

collect_metrics(Tweets_RS_Basic)

Zoom <- rank_results(Tweets_RS_Basic, 
                     rank_metric = "accuracy")

Zoom$wflow_id[1]

write_excel_csv(Zoom, "Metrics Models K Fold Inicial.csv")

########## 6. Importancia predictores ########## 

Importance_Wf <- workflow(Recipe_1,
                          RF_Spec)

Importance_Fit <- fit(Importance_Wf,
                      Tweets_Training)

Importance_Plot_Rf <- Importance_Fit %>%
  extract_fit_parsnip() %>% 
  vip(num_features = 20L,
      geom = "point",
  )

########## 7. Hacer un último ajuste y predicción en entrenamiento y test #####

Best_Fit <-
  Tweets_Wflow %>% 
  extract_workflow(Zoom$wflow_id[1]) %>%
  last_fit(Tweets_Split)

Best_Fit

########## 8. Checkeo métricas y matriz de confusión #####

##### Matriz de confusión

collect_predictions(Best_Fit) %>%
  conf_mat(Name,
           .pred_class) %>% 
  autoplot()

###### Se calcula el accuracy y el ROC

collect_metrics(Best_Fit)

##### 9. Predicción Submission Test #####

Submission_Fit <-
  Tweets_Wflow %>% 
  extract_workflow(Zoom$wflow_id[1]) %>%
  fit(Tweets_Training)

Submission_Fit

Submission_Pred <- predict(
  Submission_Fit, 
  Test_Final)


Submission_Final <- bind_cols(Submission_Pred, 
                              Test_Final) %>% 
  select("id" = Id, "pobre" = .pred_class)

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-2-Predicting-Tweets/Data/Submissions/")

write_excel_csv(Submission_Final, "Sample Submission JSVT.csv")

##### Objetivo: Predecir la autoria de un pool de tweets entre 3 políticos colombianos

### Parte 2B: Modelos de clasificación de texto

# Autores:
# Andres Carrillo
# Diana Higuera
# Juan Sebastian Vallejo
# Lizbeth Hernandez


### Fecha creación : February 20th 2023

### Datos para correr el código: 

# Data Entrenamiento Modelo.csv
# Data Test Modelo.csv

######################################## Set up ######################################################

###### Limpiar el espacio de trabajo

rm(list=ls())

##### Cargar paquetes necesarios

require(pacman)

p_load(tidyverse, ggplot2, tm,
       ggExtra, tidymodels, themis,
       textstem, tidyr, stringr, stopwords,
       stringi, gridExtra, ranger, tidyr, vip,
       xgboost, finetune, brulee, 
       naivebayes, discrim, yardstick, 
       textrecipes, hardhat, tidytext) 

##### Cargar funciones

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))

##### Cargar bases de datos #####

### Definir ubicación de los archivos sin comprimir (Cambia según PC)


setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Final")


Model_Train_Data <- read.csv2("Data Entrenamiento Modelo Lem.csv", 
                              sep = ";", 
                              dec = ",", 
                              fileEncoding = "UTF-8")

Model_Test_Data <- read.csv2("Data Test Modelo Lem.csv", 
                             sep = ";", 
                             dec = ",", 
                             fileEncoding = "UTF-8")


summary(Model_Train_Data)

summary(Model_Test_Data)

######################################## Woodwork ######################################################


########## Training ##########

##### Definir vector de variables a convertir a string

To_Factor <- c("Bogota", "Macro", "Familia",
               "Vacunacion", "Emoji", "Excl.Mark")

Model_Train_Data[To_Factor] <- lapply(Model_Train_Data[To_Factor], as.character)

##### Convertir variables de texto a factor

Model_Data <- Model_Train_Data %>% 
  mutate_if(
    is.character, factor) 

str(Model_Data)

########## Test ##########

##### Definir vector de variables a convertir a string


Model_Test_Data[To_Factor] <- lapply(Model_Test_Data[To_Factor], as.character)


##### Convertir variables de texto a factor

Test_Final <- Model_Test_Data %>% 
  mutate_if(
    is.character, factor)




########## Limpiar los Tweets #####

Model_Train_Data$Text.Clean <- Model_Train_Data$Text %>%
  iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub='') %>%
  removeWords(stopwords(language = "es", 
                        source = "snowball")) %>% 
  removeWords(stopwords(language = "es", 
                        source = "nltk")) %>% 
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace() %>% 
  tolower()


##################################### Modelling with Tidymodels ######################################################

########## 1. Definir base de entrenamiento y test, así como método de "resampleo" #####

set.seed(24)

Tweets_Split <- Model_Train_Data %>% 
  initial_split(strata = Name) # Se hace por estratificación para mantener la proporción de la variable Pobre entre submuestras.

Tweets_Training <- training(Tweets_Split)

Tweets_Test <- testing(Tweets_Split) # 74% de la muestra de entrenamiento inicial 

set.seed(2403)

Tweets_Folds <- vfold_cv(Tweets_Training, 
                         strata = Name) # 10 submuestras por default

########## 2. Definir Recetas y forma funcional #####

Recipe_1 <- 
  recipe(Name ~ Text.Clean + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>% 
  step_tokenize(Text.Clean) %>%
  step_tokenfilter(Text.Clean, 
                   max_tokens = 2e3) %>% 
  step_tfidf(Text.Clean) 

Recipe_2 <- 
  recipe(Name ~ Text.Clean + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic, 
         Tweets_Training) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>% 
  step_tokenize(Text.Clean) %>%
  step_tokenfilter(Text.Clean, 
                   max_tokens = 2e3) %>% 
  step_tfidf(Text.Clean) 


Recipe_3 <- 
  recipe(Name ~ Text.Clean + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>% 
  step_tokenize(Text.Clean) %>%
  step_tokenfilter(Text.Clean, 
                   max_tokens = 2e3) %>% 
  step_tfidf(Text.Clean) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_smote(Name)

Recipe_4 <- 
  recipe(Name ~ Text.Clean + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>% 
  step_tokenize(Text.Clean.L) %>%
  step_tokenfilter(Text.Clean.L, 
                   max_tokens = 2e3) %>% 
  step_tfidf(Text.Clean) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_smote(Name)

##### Receta del PCA

Recipe_PCA <- 
  recipe(Name ~ Text.Clean + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic + 
           No.Noun + No.Adjs + No.Verbs + No.Other, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>%
  step_tokenize(Text.Clean) %>%
  step_tokenfilter(Text.Clean, 
                   max_tokens = 2e3) %>% 
  step_tfidf(Text.Clean) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_predictors(), 
           keep_original_cols = TRUE,
           threshold = .90
  ) %>% 
  step_smote(Name)



Recipe_PCA2 <- 
  recipe(Name ~ Text.Clean.L + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic + 
           No.Noun + No.Adjs + No.Verbs + No.Other, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>%
  step_tokenize(Text.Clean.L) %>%
  step_tokenfilter(Text.Clean.L, 
                   max_tokens = 2e3) %>% 
  step_tfidf(Text.Clean.L) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_predictors(), 
           keep_original_cols = TRUE,
           threshold = .90) %>% 
  step_smote(Name)


Recipe_PCA3 <- 
  recipe(Name ~ Text.Clean + No.Characters + No.Users + No.Hash +
           Disp.Characters + Emoji + Excl.Mark +
           Tf_Idf_Topic + Mentions.Topic + Bogota + Macro +
           Familia + Vacunacion + Freq_Topic + 
           No.Noun + No.Adjs + No.Verbs + No.Other, 
         Tweets_Training) %>%
  step_dummy(Emoji, Excl.Mark, Tf_Idf_Topic, Mentions.Topic,
             Bogota, Macro, Familia, Vacunacion, Freq_Topic) %>%
  step_tokenize(Text.Clean) %>%
  step_tokenfilter(Text.Clean, 
                   max_tokens = 3e3) %>% 
  step_tfidf(Text.Clean) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_predictors(), 
           keep_original_cols = TRUE,
           threshold = .90) %>% 
  step_smote(Name)


##### Revisar si la base de entrenamiento se está preparando bien

# Zoom <- juice(prep(Recipe_PCA))

#summary(Zoom)

##### Definir Blueprint

# Sparse_Tweets <- default_recipe_blueprint(composition = "dgCMatrix")

########## 3. Definir modelos y el workflow #####

Elastic_Multinom_Spec <- 
  multinom_reg(penalty = 0.01,
               mixture = 0.5) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")


Lasso_Multinom_Spec <- 
  multinom_reg(penalty = 0.01,
               mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")


Neural_Network_Spec <-
  mlp() %>%
  set_engine("brulee") %>% 
  set_mode("classification")


RF_Spec <-
  rand_forest(trees = 100) %>%
  set_engine("ranger") %>% 
  set_mode("classification") 

Xgb_Spec <-
  boost_tree(
    trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

### Workflow

Tweets_Wflow <- workflow_set(
  list("PCA" = Recipe_PCA, "PCA New Vars" = Recipe_PCA2, "PCA Tokens3" = Recipe_PCA3),
  list("Elastic Multinom" = Elastic_Multinom_Spec,
       "Lasso Multinom" = Lasso_Multinom_Spec,
       "Random Forest" = RF_Spec)
)

########## 4. Se corren los modelos ######### 

Tweets_Metrics <- metric_set(accuracy, sensitivity, 
                             specificity, roc_auc)

doParallel::registerDoParallel()
set.seed(240394)

Tweets_RS <-
  workflow_map(
    Tweets_Wflow,
    "fit_resamples",
    resamples = Tweets_Folds,
    metrics = Tweets_Metrics
  )

########## 4b. Modelo Alternativo ##########

Multi_Lasso_Wf <- workflow() %>%
  add_recipe(Recipe_1, 
             blueprint = Sparse_Tweets) %>%
  add_model(Multinom_Spec)

Multi_Lasso_Fit <- fit(Multi_Lasso_Wf,
                       Tweets_Training)


Multi_Lasso_Pred <- predict(
  Multi_Lasso_Fit, 
  Tweets_Test)

Multi_Lasso_Final <- bind_cols(Multi_Lasso_Pred, 
                               Tweets_Test)

accuracy(Multi_Lasso_Final, as.factor(Name), as.factor(.pred_class))


########## 5. Seleccionar el mejor modelo con base en la métrica elegida (Accuracy) ########## 

collect_metrics(Tweets_RS)

Zoom <- rank_results(Tweets_RS, 
                     rank_metric = "accuracy")

Zoom$wflow_id[1]

write_excel_csv(Zoom, "Metrics Models K Fold Final Last.csv")

########## 6. Importancia predictores ########## 

Importance_Wf <- workflow(Recipe_PCA3,
                          Elastic_Multinom_Spec)

Importance_Fit <- fit(Importance_Wf,
                      Tweets_Training)

Importance_Plot_Rf <- Importance_Fit %>%
  extract_fit_parsnip() %>% 
  vip(num_features = 20L,
      geom = "point",
  )

########## 7. Hacer un último ajuste y predicción en entrenamiento y test #####

Best_Fit <-
  Tweets_Wflow %>% 
  extract_workflow(Zoom$wflow_id[1]) %>%
  last_fit(Tweets_Split)

Best_Fit

########## 8. Checkeo métricas y matriz de confusión #####

##### Matriz de confusión

collect_predictions(Best_Fit) %>%
  conf_mat(Name,
           .pred_class) %>% 
  autoplot()

###### Se calcula el accuracy y el ROC

collect_metrics(Best_Fit)

##### 9. Predicción Submission Test #####

Submission_Fit <-
  Tweets_Wflow %>% 
  extract_workflow(Zoom$wflow_id[1]) %>%
  fit(Tweets_Training)

Submission_Fit

Submission_Pred <- predict(
  Submission_Fit, 
  Test_Final)


Submission_Final <- bind_cols(Submission_Pred, 
                              Test_Final) %>% 
  select("id" = Id, "name" = .pred_class)

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Submissions")

write_excel_csv(Submission_Final, "Elastic Multinomial PCA Tuned.csv")


########## 10. Tunear los 3  mejores modelos después del primer submission (M-EN) #####


##### Multinomial Elastic Net #####

### Definir los hiperparámetros que se van a optimizar #####

Multinomial_Tuned_Spec <-
  multinom_reg(penalty = tune(), 
               mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

### Definir la receta que mejor desempeño tuvo #####

Multinomial_Tuned_Wflow <- workflow(Recipe_3, 
                                    Multinomial_Tuned_Spec)

### Se define la grilla con los parametros que se van a probar en la optimización #####

set.seed(2023)

Multinomial_Grid <-
  grid_max_entropy(
    penalty(c(-3, 0)),
    mixture(c(0, 1)),
    size = 50
  )

Multinomial_Grid


### Se corre el proceso de optimización #####

doParallel::registerDoParallel()

set.seed(240394)

Multinomial_Rs <-
  tune_race_anova(
    Multinomial_Tuned_Wflow,
    Tweets_Folds,
    grid = Multinomial_Grid,
    metrics = metric_set(accuracy),
    control = control_race(verbose_elim = TRUE)
  )

Multinomial_Rs

### Se grafican los resultados y se elige la mejor combinación de parámetros #####

plot_race(Multinomial_Rs)

Tune <- Multinomial_Rs$.metrics

show_best(Multinomial_Rs)

### Se hace un último ajuste con la mejor especificación identificada #####

Multinomial_Last <-
  Multinomial_Tuned_Wflow %>%
  finalize_workflow(select_best(Multinomial_Rs, 
                                "accuracy")) %>%
  last_fit(Tweets_Split)

## Matriz de confusión

collect_predictions(Multinomial_Last) %>%
  conf_mat(Name,
           .pred_class)

## Se calcula el accuracy y el ROC

collect_predictions(Multinomial_Last) %>%
  accuracy(Name, .pred_class)

### Ajuste final para Kaggle #####

Multinomial_Kaggle_Spec <-
  multinom_reg(penalty = 0.0622, 
               mixture = 0.0293) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

Multinomial_Wf_Kaggle <- workflow(Recipe_PCA2,
                                  Multinomial_Kaggle_Spec)

Multinomial_Fit_Kaggle <- fit(Multinomial_Wf_Kaggle,
                              Tweets_Training)


Submission_Pred_Multinomial_Tuned <- predict(
  Multinomial_Fit_Kaggle, 
  Test_Final)

Submission_Multinomial_Tuned <- bind_cols(Submission_Pred_Multinomial_Tuned, 
                                          Test_Final) %>% 
  select("id" = Id, "name" = .pred_class)

Submission_Multinomial_Tuned <- Tweets_Test_Data %>% 
  merge(
    Submission_Multinomial_Tuned,
    by = "id",
    all.x = TRUE
  )

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Submissions")


write_excel_csv(Submission_Multinomial_Tuned, "Multinomial PCA Tuned Submission.csv")



##### Neural Network #####
### Definir los hiperparámetros que se van a optimizar #####

Neural_Tuned_Spec <-
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune(),
    learn_rate = tune(),
    
  ) %>% 
  set_engine("brulee") %>% 
  set_mode("classification") 

### Definir la receta que mejor desempeño tuvo #####

Neural_Wflow <- workflow(Recipe_3, 
                         Neural_Tuned_Spec)

### Se define la grilla con los parametros que se van a probar en la optimización #####

set.seed(2023)

Neural_Grid <-
  grid_max_entropy(
    hidden_units(c(1L, 10L)),
    penalty(c(-3, 0)),
    epochs(c(10L, 20L)),
    learn_rate(c(-3, -1)),
    size = 30
  )

Neural_Grid

### Se corre el proceso de optimización #####

doParallel::registerDoParallel()

set.seed(240394)

Neural_Network_Rs <-
  tune_race_anova(
    Neural_Wflow,
    Tweets_Folds,
    grid = Neural_Grid,
    metrics = metric_set(accuracy),
    control = control_race(verbose_elim = TRUE)
  )

Neural_Network_Rs

### Se grafican los resultados y se elige la mejor combinación de parámetros #####

plot_race(Neural_Network_Rs)

Tune <- Neural_Network_Rs$.metrics

show_best(Neural_Network_Rs)

### Se hace un último ajuste con la mejor especificación identificada #####

Xgb_Last <-
  Xgb_Wflow %>%
  finalize_workflow(select_best(Xgb_Rs, 
                                "accuracy")) %>%
  last_fit(Poverty_Split)

## Matriz de confusión

collect_predictions(Xgb_Last) %>%
  conf_mat(Pobre,
           .pred_class)

## Se calcula el accuracy y el ROC

collect_predictions(Xgb_Last) %>%
  accuracy(Pobre, .pred_class)

### Ajuste final para Kaggle #####

Xgb_Spec_Kaggle <-
  boost_tree(
    trees = 356,
    tree_depth = 7,
    min_n = 26,
    mtry = 8,
    sample_size = 0.344,
    learn_rate = 0.0894
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

Xgb_Wf_Kaggle <- workflow(Recipe_1,
                          Xgb_Spec_Kaggle)

Xgb_Fit_Kaggle <- fit(Xgb_Wf_Kaggle,
                      Poverty_Training)


Submission_Pred_Xgb_Tuned <- predict(
  Xgb_Fit_Kaggle, 
  Test_Final)

Submission_Xgb_Tuned <- bind_cols(Submission_Pred_Xgb_Tuned, 
                                  Test_Final) %>% 
  select("id" = Id, "name" = .pred_class)

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Submissions")

write_excel_csv(Submission_Xgb_Tuned, "Xgboost Tuned Submission.csv")










##### PCA Multinomial #####
### Definir los hiperparámetros que se van a optimizar #####

PCA_Tuned_Spec <-
  multinom_reg(penalty = 0.0622, 
               mixture = 0.0293) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

### Definir la receta que mejor desempeño tuvo #####

PCA_Wflow <- workflow(Recipe_PCA3, 
                      PCA_Tuned_Spec)

### Se define la grilla con los parametros que se van a probar en la optimización #####

set.seed(2023)

PCA_Grid <-
  grid_max_entropy(
    max_tokens(c(1e3, 3e3)),
    size = 30
  )

PCA_Grid

### Se corre el proceso de optimización #####

doParallel::registerDoParallel()

set.seed(240394)

PCA_Multinomial_Rs <-
  tune_race_anova(
    PCA_Wflow,
    Tweets_Folds,
    grid = PCA_Grid,
    metrics = metric_set(accuracy),
    control = control_race(verbose_elim = TRUE)
  )

PCA_Multinomial_Rs

### Se grafican los resultados y se elige la mejor combinación de parámetros #####

plot_race(PCA_Multinomial_Rs)

Tune <- PCA_Multinomial_Rs$.metrics

show_best(PCA_Multinomial_Rs)

### Se hace un último ajuste con la mejor especificación identificada #####

PCA_Multinomial_Last <-
  PCA_Wflow %>%
  finalize_workflow(select_best(PCA_Multinomial_Rs, 
                                "accuracy")) %>%
  last_fit(Tweets_Split)

## Matriz de confusión

collect_predictions(PCA_Multinomial_Last) %>%
  conf_mat(Name,
           .pred_class)

## Se calcula el accuracy y el ROC

collect_predictions(PCA_Multinomial_Last) %>%
  accuracy(Name, .pred_class)

### Ajuste final para Kaggle #####

Xgb_Spec_Kaggle <-
  boost_tree(
    trees = 356,
    tree_depth = 7,
    min_n = 26,
    mtry = 8,
    sample_size = 0.344,
    learn_rate = 0.0894
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

Xgb_Wf_Kaggle <- workflow(Recipe_1,
                          Xgb_Spec_Kaggle)

Xgb_Fit_Kaggle <- fit(Xgb_Wf_Kaggle,
                      Poverty_Training)


Submission_Pred_Xgb_Tuned <- predict(
  Xgb_Fit_Kaggle, 
  Test_Final)

Submission_Xgb_Tuned <- bind_cols(Submission_Pred_Xgb_Tuned, 
                                  Test_Final) %>% 
  select("id" = Id, "name" = .pred_class)

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Problem-Set-4--Predicting-Tweets/Data/Submissions")

write_excel_csv(Submission_Xgb_Tuned, "Xgboost Tuned Submission.csv")













