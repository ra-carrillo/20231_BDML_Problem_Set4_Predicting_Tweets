# Problem Set 4: predecir Tweets
# 2023/03/18

# Autores:
# Andres Carrillo
# Diana Higuera
# Juan Sebastian Vallejo
# Lizbeth Hernandez


### Parte 1: preparación de las bases de datos 

### Limpieza del entorno
rm(list = ls())

### Cargar los paquetes necesarios
library(pacman) 

p_load(tidyverse, tm, stringi, tidytext, stopwords, wordcloud2, udpipe,
       ggcorrplot) 

### Cargar las bases
setwd("/Users/Dhiguera/Desktop/MECA UNIANDES/2301/PROBLEM SET/PS4/DB")

test <- read.csv2("test.csv", 
                  sep = ",", 
                  dec = ".", 
                  fileEncoding = "UTF-8")

train <- read.csv2("train.csv", 
                   sep = ",", 
                   dec = ".", 
                   fileEncoding = "UTF-8")

head(train, 1)
glimpse(train)



#------------------------------------------------------------------------ Limpieza de texto

### Número de tweets por autor
table(train$name) / length(train$name) # proporción similar para cada autor

### Se limpia la variable text
train["text"] <- apply(train["text"], 1, tolower)             # texto en minúscula 
train["text"] <- apply(train["text"], 1, removeNumbers)       # eliminar números
train["text"] <- apply(train["text"], 1, removePunctuation)   # eliminar puntuación
train["text"] <- apply(train["text"], 1, function(x)          # eliminar acentos
  stri_trans_general(str = x, id = "Latin-ASCII"))

train <- train %>% 
  mutate(text = gsub('@|#', '', train$text))

train <- train %>% 
  mutate(text = gsub('!|¡', '', train$text))

train <- train %>% 
  mutate(text = gsub('¿|?', '', train$text))

train <- train %>% 
  mutate(text = gsub('"', '', train$text))

train["text"] <- apply(train["text"], 1, stripWhitespace)     # eliminar múltiples espacios en blanco


### Se transforma el dataframe a nivel de palabras, previa creación de un id para cada tweet 
train <- train %>%
  mutate(id2 = row_number())

words <- train %>%
  unnest_tokens(output = "word", input = "text")

### Se eliminan las stopwords
sw <- c()
for (s in c("snowball", "stopwords-iso", "nltk")) {
  temp <- get_stopwords("spanish", source = s)$word
  sw <- c(sw, temp)
}
sw <- unique(sw)
sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
sw <- data.frame(word = sw)

### Número de palabras antes de eliminar stopwords
nrow(words)      # 290298 palabras

words <- words %>%
  anti_join(sw, by = "word")

### Número de palabras después de eliminar stopwords
nrow(words)     # 139097 palabras



#-------------------------------------------------------------------- Exploración de la base

### Número de palabras utilizadas por cada autor
words %>% group_by(name) %>% summarise(n = n()) 

words %>%  ggplot(aes(x = name)) + geom_bar() + coord_flip() + 
  labs(x = "Autor", y = "Número de palabras") + 
  theme_bw() 


### Longitud media de los tweets por autor

words %>% group_by(name, id2) %>% summarise(longitud = n()) %>%                       
  group_by(name) %>% summarise(media_longitud = mean(longitud),
                                sd_longitud = sd(longitud))

words %>% group_by(name, id2) %>% summarise(longitud = n()) %>%                      
  group_by(name) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = name, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  labs(x = "Autor", y = "Longitud media/Tweet") + 
  coord_flip() + theme_bw()                                      
# Los tweets de Uribe y Petro son similares en longitud media y desviación, mostrando ésta última que suelen alternar entre tweets largos y cortos. 
# Los tweets de López son más extensos y con menor desviación.   

### Gráfica de las frecuencia de palabras por autor
words %>% group_by(name, word) %>% count(word) %>% group_by(name) %>%
  top_n(10, n) %>% arrange(name, desc(n)) %>%
  ggplot(aes(x = reorder(word,n), y = n, fill = name)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~name,scales = "free", ncol = 1, drop = TRUE)


#------------------------------------------------------------------- Lematización y matriz TF-IDF

### Lematización

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

### Palabras menos comunes
words %>%
  count(lemma) %>%
  arrange(desc(n)) %>%
  tail(100)
### Palabras más comunes
words %>%
  count(lemma) %>%
  arrange(desc(n)) %>%
  head()
### Validación de palabras únicas
length(unique(words$lemma))  # 17662

### Para reducir el tamaño de la matriz TF-IDF se eliminan todas las palabras que estén menos de 10 veces
palabras_eliminar <- words %>%
  count(lemma) %>%
  filter(n < 10)

words <- words %>%
  anti_join(palabras_eliminar, by = "lemma") 

### Se ajusta el texto para volver al formato original (Tweet por fila)
data_clean <- words %>%
  group_by(id2,name,id) %>% 
  summarise(comentario = str_c(lemma, collapse = " ")) %>%
  ungroup()

setdiff(train$id, data_clean$id)
train[c(9287, 9349),]

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

### Se guarda la base final de train
save(tf_idf, 
     file = "train_para_modelar.RData")

