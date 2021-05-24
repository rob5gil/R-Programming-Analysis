# Cargamos librerías
packages <- c("quanteda","tidytext","textdata","stringr","glue","reshape2","wordcloud","RWeka","ggmap","lubridate","ggplot2","dplyr","data.table","ggrepel","tidyverse","hrbrthemes")
new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)
library(dplyr)
library(ggplot2)
library(quanteda)
library(tidytext)
library(textdata)
library(stringr)
library(tidyverse)
library(glue)
library(stringr)
library(reshape2)
library(wordcloud)
library(RWeka)
library(hrbrthemes)
library(tm)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr, dplyr, magrittr)

# Partimos de dos archivos .csv
rm(list=ls())
trump <- read.csv("hashtag_donaldtrump.csv",stringsAsFactors = FALSE)
biden <- read.csv("hashtag_joebiden.csv",stringsAsFactors = FALSE)

# Unimos ambas bases de tweets
ambos <- rbind(biden,trump)

# Acotamos al periodo pre-elecciones (15 octubre - 2 noviembre)
ambos$created_at <- as.POSIXct(ambos$created_at, format="%Y-%m-%d %H:%M:%OS")
ambos <- ambos[(ambos$created_at>"2020-10-14" & ambos$created_at<"2020-11-03"), ]

# Comprobamos número de columnas de nuestro set
ncol(ambos)

# Eliminamos columnas innecesarias
ambos$tweet_id <- ambos$user_id <- ambos$user_name <- ambos$user_description <- ambos$user_location <- ambos$state_code <- ambos$collected_at <- NULL 
ncol(ambos)

# Convertimos la columna "likes" a numérica
ambos$likes <- as.numeric(ambos$likes)

# Convertimos la columna "user_followers_count" a numérica
ambos$user_followers_count <- as.numeric(ambos$user_followers_count)

# Eliminamos los NAs y blanks de la columna "likes"
ambos <- ambos[!(is.na(ambos$likes) | ambos$likes==""), ]

# Eliminamos los NAs y blanks de la columna "retweet_count"
ambos <- ambos[!(is.na(ambos$retweet_count) | ambos$retweet_count==""), ]

# Eliminamos los tweets que tengan 0 likes y 0 retweets
ambos <- ambos[!(ambos$likes=="0" & ambos$retweet_count=="0"), ]

# Si “city”, “country”, “continent” o “state” son NAs o blanks, ponemos “Unknown”
ambos$city[ambos$city==''] <- NA
ambos$country[ambos$country==''] <- NA
ambos$continent[ambos$continent==''] <- NA
ambos$state[ambos$state==''] <- NA

ambos$city <- as.character(ambos$city)
ambos$country <- as.character(ambos$country)
ambos$continent <- as.character(ambos$continent)
ambos$state <- as.character(ambos$state)

ambos$city[is.na(ambos$city)] <- "Unknown"
ambos$country[is.na(ambos$country)] <- "Unknown"
ambos$continent[is.na(ambos$continent)] <- "Unknown"
ambos$state[is.na(ambos$state)] <- "Unknown"

# Preparamos la columna "tweet" para el análisis eliminando caracteres extraños

# Eliminamos las mayúsculas
ambos$tweet = tolower(ambos$tweet)

# Eliminamos los links
ambos$tweet = gsub("http[a-zA-Z0-9:/.]*", "", ambos$tweet, perl = TRUE)
ambos$tweet <- gsub('https://t.co/[A-Za-z0-9]+', '', ambos$tweet )

# Transformamos los hashtags y menciones más importantes en palabras
ambos$tweet = gsub("@joebiden","joe biden",ambos$tweet)
ambos$tweet = gsub("@kamalaharris","kamala harris",ambos$tweet)
ambos$tweet = gsub("@realdonaldtrump","donald trump",ambos$tweet)
ambos$tweet = gsub("@potus","donald trump",ambos$tweet)
ambos$tweet = gsub("@vp","mike pence",ambos$tweet)
ambos$tweet = gsub("@mike_pence","mike pence",ambos$tweet)
ambos$tweet = gsub("#joe ","joe ",ambos$tweet)
ambos$tweet = gsub("#biden ","biden ",ambos$tweet)
ambos$tweet = gsub("#trump ","trump ",ambos$tweet)
ambos$tweet = gsub("#donald ","donald ",ambos$tweet)
ambos$tweet = gsub("#donaldtrump ","donald trump ",ambos$tweet)
ambos$tweet = gsub("#don ","don ",ambos$tweet)
ambos$tweet = gsub("#kamala ","kamala ",ambos$tweet)
ambos$tweet = gsub("#harris ","harris ",ambos$tweet)
ambos$tweet = gsub("#pence ","pence ",ambos$tweet)
ambos$tweet = gsub("#mikepence ","mike pence ",ambos$tweet)
ambos$tweet = gsub("#joebiden ","joe biden ",ambos$tweet)
ambos$tweet = gsub("#kamalaharris ","kamala harris ",ambos$tweet)
ambos$tweet = gsub("#realdonaldtrump ","donald trump ",ambos$tweet)
ambos$tweet = gsub("#potus ","donald trump ",ambos$tweet)
ambos$tweet = gsub("#vp ","mike pence ",ambos$tweet)
ambos$tweet = gsub("#mike_pence ","mike pence ",ambos$tweet)
ambos$tweet = gsub("joebiden ","joe biden ",ambos$tweet)
ambos$tweet = gsub("realdonaldtrump ","donald trump ",ambos$tweet)
ambos$tweet = gsub("donaldtrump ","donald trump ",ambos$tweet)
ambos$tweet = gsub("kamalaharris ","kamala harris ",ambos$tweet)
ambos$tweet = gsub("#bidenharris ","biden harris ",ambos$tweet)
ambos$tweet = gsub("#trumppence ","trump pence ",ambos$tweet)


# Eliminamos los símbolos extraños
ambos$tweet = gsub("#(\\d|\\w)+","",ambos$tweet)
ambos$tweet = gsub("@[0-9_A-Za-z]+","",ambos$tweet)
ambos$tweet <- gsub('&amp;', 'and', ambos$tweet )
ambos$tweet <- gsub('amp', 'and', ambos$tweet )
ambos$tweet = gsub("[^a-zA-Z Ã±Ã¡Ã©Ã?Ã³Ãº@#]","",ambos$tweet)

# Hacemos una limpieza de los tweets que se hayan quedado en blanco (es decir, que solo incluían caracteres extraños)
ambos <- ambos[!(is.na(ambos$tweet) | ambos$tweet==""), ]

# Eliminamos duplicados
ambos <- (ambos %>% distinct())

# Limpiamos con complete.cases
ambos <- ambos[complete.cases(ambos), ]


# Dividimos nuestro dataset en tweets que solo mencionan a uno de los candidatos, o a ambos
ambos$mencionan <- ifelse(!grepl("trump",ambos$tweet),"biden",ambos$tweet)
ambos$mencionan <- ifelse(!grepl("biden",ambos$mencionan),"trump",ambos$mencionan)

biden <- ambos[ambos$mencionan=="biden", ]
trump <- ambos[ambos$mencionan=="trump", ]
ambos <- ambos[ambos$mencionan!="biden" & ambos$mencionan!="trump", ]


# Preparamos el ranking de sentimiento para usarlo más adelante

trump_ranking <- trump[,2, drop=FALSE]
trump_ranking <- get_sentences(trump_ranking)
trump_ranking <- sentiment_by(trump_ranking)
trump_ranking <- trump_ranking[,4, drop=FALSE]
trump <- cbind(trump,trump_ranking)

biden_ranking <- biden[,2, drop=FALSE]
biden_ranking <- get_sentences(biden_ranking)
biden_ranking <- sentiment_by(biden_ranking)
biden_ranking <- biden_ranking[,4, drop=FALSE]
biden <- cbind(biden,biden_ranking)

ambos_ranking <- ambos[,2, drop=FALSE]
ambos_ranking <- get_sentences(ambos_ranking)
ambos_ranking <- sentiment_by(ambos_ranking)
ambos_ranking <- ambos_ranking[,4, drop=FALSE]
ambos <- cbind(ambos,ambos_ranking)

# Guardamos los csv limpios para poder trabajar con ellos de aquí en adelante sin tener que empezar el proceso desde cero
write.csv(trump, "trump_limpio.csv", row.names = FALSE)
write.csv(biden, "biden_limpio.csv", row.names = FALSE)
write.csv(ambos, "ambos_limpio.csv", row.names = FALSE)

rm(list=ls())
trump <- read.csv("trump_limpio.csv",stringsAsFactors = FALSE)
biden <- read.csv("biden_limpio.csv",stringsAsFactors = FALSE)
ambos <- read.csv("ambos_limpio.csv",stringsAsFactors = FALSE)

# Volvemos a indicar que las fechas deben estar en formato POSIXct
biden$created_at <- as.POSIXct(biden$created_at, format="%Y-%m-%d %H:%M:%OS")
trump$created_at <- as.POSIXct(trump$created_at, format="%Y-%m-%d %H:%M:%OS")
ambos$created_at <- as.POSIXct(ambos$created_at, format="%Y-%m-%d %H:%M:%OS")
biden$user_join_date <- as.POSIXct(biden$user_join_date, format="%Y-%m-%d %H:%M:%OS")
trump$user_join_date <- as.POSIXct(trump$user_join_date, format="%Y-%m-%d %H:%M:%OS")
ambos$user_join_date <- as.POSIXct(ambos$user_join_date, format="%Y-%m-%d %H:%M:%OS")

# Pre-análisis de likes y retweets
summary(biden$likes)
summary(biden$retweet_count)
summary(trump$likes)
summary(trump$retweet_count)
summary(ambos$likes)
summary(ambos$retweet_count)

plot(trump$user_followers_count,trump$likes)
plot(biden$user_followers_count,biden$likes)
plot(ambos$user_followers_count,ambos$likes)

# Análisis de correlación entre número de likes y retweets
p1_t <- ggplot(trump, aes(x=likes, y=retweet_count)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p1_t

p1_b <- ggplot(biden, aes(x=likes, y=retweet_count)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p1_b

p1_a <- ggplot(ambos, aes(x=likes, y=retweet_count)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p1_a

# Análisis de correlación entre número de likes y followers
p2_t <- ggplot(trump, aes(x=user_followers_count, y=likes)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p2_t

p2_b <- ggplot(biden, aes(x=user_followers_count, y=likes)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p2_b

p2_a <- ggplot(ambos, aes(x=user_followers_count, y=likes)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p2_a

# Análisis de correlación entre negatividad y likes
p3_t <- ggplot(trump, aes(x=ave_sentiment, y=likes)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p3_t

p3_b <- ggplot(biden, aes(x=ave_sentiment, y=likes)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p3_b

p3_a <- ggplot(ambos, aes(x=ave_sentiment, y=likes)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p3_a

# Análisis de correlación entre fecha de creación de la cuenta y número de seguidores
p4_t <- ggplot(trump, aes(x=user_join_date, y=user_followers_count)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p4_t

p4_b <- ggplot(biden, aes(x=user_join_date, y=user_followers_count)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p4_b

p4_a <- ggplot(ambos, aes(x=user_join_date, y=user_followers_count)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p4_a


# Análisis de correlación entre fecha de creación de la cuenta y sentimiento

p5_t <- ggplot(trump, aes(x=user_join_date, y=ave_sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p5_t

p5_b <- ggplot(biden, aes(x=user_join_date, y=ave_sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p5_b

p5_a <- ggplot(ambos, aes(x=user_join_date, y=ave_sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p5_a


# Análisis de correlación entre fecha de publicación del tweet y sentimiento

p6_t <- ggplot(trump, aes(x=created_at, y=ave_sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p6_t

p6_b <- ggplot(biden, aes(x=created_at, y=ave_sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p6_b

p6_a <- ggplot(ambos, aes(x=created_at, y=ave_sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p6_a


# Histograma de sentimientos

# Detallado
hist(trump$ave_sentiment, breaks=30,  xlim=c(-2,2), col=rgb(1,0,0,0.5), xlab="Sentimiento", 
     ylab="Número de tweets", main="Histograma de sentimiento Trump vs. Biden" )
hist(biden$ave_sentiment, breaks=30, xlim=c(-2,2), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Trump","Biden"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

# Menos fragmentado
hist(trump$ave_sentiment, breaks=10,  xlim=c(-2,2), col=rgb(1,0,0,0.5), xlab="Sentimiento", 
     ylab="Número de tweets", main="Histograma de sentimiento" )
hist(biden$ave_sentiment, breaks=10, xlim=c(-2,2), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Trump","Biden"), col=c(rgb(1,0,0,0.5), 
                                                    rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

# Densidad de seguidores
p5 <- ggplot(data = trump, aes(x=user_followers_count))+
  geom_density(fill=rgb(1,0,0,0.5)) +
  # Change the fill colour to differentiate it
  geom_density(data= biden, fill=rgb(0,0,1,0.5)) +
  labs(y="Densidad")+
  labs(x="Número de seguidores")+
  xlim(0, 7000)

p5




# Pre-análisis de ubicaciones y tweets
length(unique(trump$country))
length(unique(trump$user_screen_name))
(length(trump$tweet))/(length(unique(trump$user_screen_name))) # media de tweets por usuario

length(unique(biden$country))
length(unique(biden$user_screen_name))
(length(biden$tweet))/(length(unique(biden$user_screen_name))) # media de tweets por usuario

length(unique(ambos$country))
length(unique(ambos$user_screen_name))
(length(ambos$tweet))/(length(unique(ambos$user_screen_name))) # media de tweets por usuario

#Analizamos gráficamente en que países mencionan cada candidato, por separado:

trump %>%
  ggplot(aes(country)) +
  geom_bar() + coord_flip() +
  labs(x = "Ubicación",
       y = "Cantidad",
       title = "Ubicación de tweets que solo mencionan a Trump")

biden %>%
  ggplot(aes(country)) +
  geom_bar() + coord_flip() +
  labs(x = "Ubicación",
       y = "Cantidad",
       title = "Ubicación de tweets que solo mencionan a Biden")

ambos %>%
  ggplot(aes(country)) +
  geom_bar() + coord_flip() +
  labs(x = "Ubicación",
       y = "Cantidad",
       title = "Ubicación de tweets que mencionan a ambos")

# Filtramos los 20 principales

trump %>%
  count(country, sort = TRUE) %>%
  mutate(country = reorder(country, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "País",
       y = "Cantidad",
       title = "Ubicación de tweets que solo mencionan a Trump (top 20 países)")

biden %>%
  count(country, sort = TRUE) %>%
  mutate(country = reorder(country, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "País",
       y = "Cantidad",
       title = "Ubicación de tweets que solo mencionan a Biden (top 20 países)")

ambos %>%
  count(country, sort = TRUE) %>%
  mutate(country = reorder(country, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "País",
       y = "Cantidad",
       title = "Ubicación de tweets que mencionan a ambos (top 20 países)")

# Vemos que Estados Unidos está duplicado con dos nombres diferentes. Lo ajustamos:

trump$country[trump$country=="United States of America"]="United States"
biden$country[biden$country=="United States of America"]="United States"
ambos$country[ambos$country=="United States of America"]="United States"

trump %>%
  count(country, sort = TRUE) %>%
  mutate(country = reorder(country, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "País",
       y = "Cantidad",
       title = "Ubicación de tweets que solo mencionan a Trump (top 20 países)")

biden %>%
  count(country, sort = TRUE) %>%
  mutate(country = reorder(country, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "País",
       y = "Cantidad",
       title = "Ubicación de tweets que solo mencionan a Biden (top 20 países)")

ambos %>%
  count(country, sort = TRUE) %>%
  mutate(country = reorder(country, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "País",
       y = "Cantidad",
       title = "Ubicación de tweets que mencionan a ambos (top 20 países)")



# MAPA

col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"

# Convertimos las columnas de latitud y longitud a numéricas
trump$long <- as.numeric(trump$long)
trump$lat <- as.numeric(trump$lat)
biden$long <- as.numeric(biden$long)
biden$lat <- as.numeric(biden$lat)
ambos$long <- as.numeric(ambos$long)
ambos$lat <- as.numeric(ambos$lat)

ubi_trump <- trump[!(is.na(trump$long) | trump$long==""), ]
ubi_biden <- biden[!(is.na(biden$long) | biden$long==""), ]
ubi_ambos <- ambos[!(is.na(ambos$long) | ambos$long==""), ]

ubi_trump$long <- as.numeric(ubi_trump$long)
ubi_trump$lat <- as.numeric(ubi_trump$lat)
ubi_biden$long <- as.numeric(ubi_biden$long)
ubi_biden$lat <- as.numeric(ubi_biden$lat)
ubi_ambos$long <- as.numeric(ubi_ambos$long)
ubi_ambos$lat <- as.numeric(ubi_ambos$lat)

ubi_trump = ubi_trump[complete.cases(ubi_trump), ]
ubi_biden = ubi_biden[complete.cases(ubi_biden), ]
ubi_ambos = ubi_ambos[complete.cases(ubi_ambos), ]

# Nos registramos en google maps.
register_google(key = "AIzaSyBg1b3A__x9Ta9RmVKOGkNAsHpXANP_rzQ")

# Creamos los mapas principales
mapa_global <- ggmap(get_googlemap(center = c(lon = 0, lat = 0),
                         zoom = 1, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

mapa_usa <- ggmap(get_googlemap(center = c(lon = -96.1982277, lat = 38.1304954),
                                   zoom = 4, scale = 2,
                                   maptype ='terrain',
                                   color = 'color'))

mapa_europa <- ggmap(get_googlemap(center = c(lon = 13.494154, lat = 49.0113818),
                                zoom = 4, scale = 2,
                                maptype ='terrain',
                                color = 'color'))


# Mapa global Trump
mapa_global +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_trump, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa global Biden
mapa_global +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_biden, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa global ambos
mapa_global +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_ambos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa USA Trump
mapa_usa +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_trump, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa USA Biden
mapa_usa +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_biden, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa USA ambos
mapa_usa +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_ambos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa Europa Trump
mapa_europa +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_trump, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa Europa Biden
mapa_europa +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_biden, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa Europa ambos
mapa_europa +   geom_point(aes(x = long, y = lat), colour = col1, data = ubi_ambos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa densidad Global Trump
mapa_global + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_trump,
  geom = "polygon"
)

# Mapa densidad USA Trump
mapa_usa + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_trump,
  geom = "polygon"
)

# Mapa densidad Europa Trump
mapa_europa + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_trump,
  geom = "polygon"
)

# Mapa densidad Global Biden
mapa_global + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_biden,
  geom = "polygon"
)

# Mapa densidad USA Biden
mapa_usa + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_biden,
  geom = "polygon"
)

# Mapa densidad Europa Biden
mapa_europa + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_biden,
  geom = "polygon"
)

# Mapa densidad Global ambos
mapa_global + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_ambos,
  geom = "polygon"
)

# Mapa densidad USA ambos
mapa_usa + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_ambos,
  geom = "polygon"
)

# Mapa densidad Europa ambos
mapa_europa + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = ubi_ambos,
  geom = "polygon"
)


# Sentimiento preliminar en mapas
summary(trump$ave_sentiment)
summary(biden$ave_sentiment)
summary(ambos$ave_sentiment)

muyrojo = "#8a0000"
rojo = "#ff0000"
naranja = "#ffa500"
verde = "#008000"

ubi_trump_pos <- ubi_trump[ubi_trump$ave_sentiment>0,]
ubi_biden_pos <- ubi_biden[ubi_biden$ave_sentiment>0,]
ubi_ambos_pos <- ubi_ambos[ubi_ambos$ave_sentiment>0,]
ubi_trump_neut <- ubi_trump[ubi_trump$ave_sentiment>(-0.5) & ubi_trump$ave_sentiment<(0),]
ubi_biden_neut <- ubi_biden[ubi_biden$ave_sentiment>(-0.5) & ubi_biden$ave_sentiment<(0),]
ubi_ambos_neut <- ubi_ambos[ubi_ambos$ave_sentiment>(-0.5) & ubi_ambos$ave_sentiment<(0),]
ubi_trump_neg <- ubi_trump[ubi_trump$ave_sentiment>(-1) & ubi_trump$ave_sentiment<(-0.5),]
ubi_biden_neg <- ubi_biden[ubi_biden$ave_sentiment>(-1) & ubi_biden$ave_sentiment<(-0.5),]
ubi_ambos_neg <- ubi_ambos[ubi_ambos$ave_sentiment>(-1) & ubi_ambos$ave_sentiment<(-0.5),]
ubi_trump_muyneg <- ubi_trump[ubi_trump$ave_sentiment<(-1),]
ubi_biden_muyneg <- ubi_biden[ubi_biden$ave_sentiment<(-1),]
ubi_ambos_muyneg <- ubi_ambos[ubi_ambos$ave_sentiment<(-1),]


# Mapa global Trump
mapa_global +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_trump_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_trump_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_trump_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_trump_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa global Biden
mapa_global +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_biden_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_biden_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_biden_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_biden_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa global ambos
mapa_global +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_ambos_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_ambos_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_ambos_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_ambos_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa USA Trump
mapa_usa +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_trump_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_trump_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_trump_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_trump_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa USA Biden
mapa_usa +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_biden_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_biden_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_biden_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_biden_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa USA ambos
mapa_usa +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_ambos_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_ambos_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_ambos_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_ambos_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa Europa Trump
mapa_europa +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_trump_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_trump_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_trump_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_trump_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa Europa Biden
mapa_europa +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_biden_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_biden_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_biden_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_biden_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

# Mapa Europa ambos
mapa_europa +   geom_point(aes(x = long, y = lat), colour = verde, data = ubi_ambos_pos, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = naranja, data = ubi_ambos_neut, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = rojo, data = ubi_ambos_neg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none") +   geom_point(aes(x = long, y = lat), colour = muyrojo, data = ubi_ambos_muyneg, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")







# Análisis de sentimiento

twcorpus <- corpus(trump$tweet)
twdfm <- dfm(twcorpus, remove_punct=TRUE, remove_numbers=TRUE, remove=c(
  stopwords("english"), stopwords("spanish"), stopwords("french"), stopwords("italian")))  


bwcorpus <- corpus(biden$tweet)
bwdfm <- dfm(bwcorpus, remove_punct=TRUE, remove_numbers=TRUE, remove=c(
  stopwords("english"), stopwords("spanish"), stopwords("french"), stopwords("italian"))) 

awcorpus <- corpus(ambos$tweet)
awdfm <- dfm(awcorpus, remove_punct=TRUE, remove_numbers=TRUE, remove=c(
  stopwords("english"), stopwords("spanish"), stopwords("french"), stopwords("italian"))) 

textplot_wordcloud(twdfm, rot.per=0, scale=c(7.5, 1.5), max.words=50)
textplot_wordcloud(bwdfm, rot.per=0, scale=c(7.5, 1.5), max.words=50)
textplot_wordcloud(awdfm, rot.per=0, scale=c(7.5, 1.5), max.words=50)

textplot_wordcloud(twdfm, min_count = 1000, 
                   color = c(  'darkgreen', 'purple', 'orange', 'blue'))

textplot_wordcloud(twdfm)

trumpfrequency <- textstat_frequency(twdfm, n = 10)
trumpfrequency
bidenfrequency <- textstat_frequency(bwdfm, n = 10)
bidenfrequency
ambosfrequency <- textstat_frequency(awdfm, n = 10)
ambosfrequency

trumpfrequency$feature <- with(trumpfrequency, reorder(feature, -frequency))

ggplot(trumpfrequency, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#XXXXXXXXXXXXXX



trump_tokens <- tibble(text = trump$tweet) %>% unnest_tokens(word, text)
biden_tokens <- tibble(text = biden$tweet) %>% unnest_tokens(word, text)
ambos_tokens <- tibble(text = ambos$tweet) %>% unnest_tokens(word, text)

# Sentiment analysis with Bing library

bing <- get_sentiments("bing")

view(bing)

# custom stop words, to be removed from analysis
custom_stop_words <- tibble(word = c("trump", "trumps","critical","issues","issue","like","vote"))

trump_tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds


biden_tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

ambos_tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

# Sentiment analysis with NRC library

nrc <- get_sentiments("nrc")

trump_tokens %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) #%>% # made data wide rather than narrow

trump_sentiments <- trump_tokens %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) #%>% # made data wide rather than narrow

trump_sentiments <- t(trump_sentiments)

trump_sentiments <- as.data.frame(trump_sentiments)
library(data.table)
setDT(trump_sentiments, keep.rownames = TRUE)[]


ggplot(data = trump_sentiments, aes(x = rn , y = V1 )) + geom_bar(stat="identity", fill="steelblue")  +  scale_fill_brewer(palette="Blues") +
  ggtitle("Análisis de sentimiento de Trump en Twitter", subtitle = "Tweets Database") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")


biden_tokens %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) #%>% # made data wide rather than narrow
#mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

biden_sentiments <- biden_tokens %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words, by = "word") %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) #%>% # made data wide rather than narrow

biden_sentiments <- t(biden_sentiments)

biden_sentiments <- as.data.frame(biden_sentiments)
library(data.table)
setDT(biden_sentiments, keep.rownames = TRUE)[]


ggplot(data = biden_sentiments, aes(x = rn , y = V1 )) + geom_bar(stat="identity", fill="steelblue")  +  scale_fill_brewer(palette="Dark2") +
  ggtitle("Análisis de sentimiento de Biden en Twitter", subtitle = "Tweets Database") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")



#Two styles of comparison clouds

trump_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
biden_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

ambos_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

trump_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors =  brewer.pal( "fear", 'Paired'), scale = c(1,.5),
                   max.words = 500, title.size = 1.5)

biden_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors =  brewer.pal( "fear", 'Paired'), scale = c(1,.5),
                   max.words = 500, title.size = 1.5)




get_sentiments("nrc")

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

trump_tokens %>%
  inner_join(nrc_joy) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

trump_joy <- trump_tokens %>%
  inner_join(nrc_joy) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

ggplot(data = trump_joy[1:15,], aes(x = word , y = n )) + geom_bar(stat="identity", fill="steelblue")  +  scale_fill_brewer(palette="Dark2") +
  ggtitle("Análisis de sentimiento positivo de Trump en Twitter", subtitle = "Tweets Database") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")



trump_tokens %>%
  inner_join(nrc_anger) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

trump_anger <-trump_tokens %>%
  inner_join(nrc_anger) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)


ggplot(data = trump_anger[1:15,], aes(x = word , y = n )) + geom_bar(stat="identity", fill="steelblue")  +  scale_fill_brewer(palette="Dark2") +
  ggtitle("Análisis de sentimiento negativo de Trump en Twitter", subtitle = "Tweets Database") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")

#Hacemos lo mismo con Biden

biden_tokens %>%
  inner_join(nrc_joy) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

biden_joy <- biden_tokens %>%
  inner_join(nrc_joy) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

ggplot(data = biden_joy[1:15,], aes(x = word , y = n )) + geom_bar(stat="identity", fill="steelblue")  +  scale_fill_brewer(palette="Dark2") +
  ggtitle("Análisis de sentimiento positivo de Biden en Twitter", subtitle = "Tweets Database") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")



biden_tokens %>%
  inner_join(nrc_anger) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

biden_anger <- biden_tokens %>%
  inner_join(nrc_anger) %>%
  anti_join(custom_stop_words, by = "word") %>%
  count(word, sort = TRUE)

ggplot(data = biden_anger[1:15,], aes(x = word , y = n )) + geom_bar(stat="identity", fill="steelblue")  +  scale_fill_brewer(palette="Dark2") +
  ggtitle("Análisis de sentimiento negativo de Biden en Twitter", subtitle = "Tweets Database") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")


#Joy

barplot(trump_joy[1:20,]$n, las = 2, names.arg = trump_joy[1:20,]$word,
        col ="lightblue", horiz = TRUE, main ="Trump's Top 20 most frequent joy words")

barplot(biden_joy[1:20,]$n, las = 2, names.arg = biden_joy[1:20,]$word,
        col ="lightblue", horiz = TRUE, main ="Biden's Top 20 most frequent joy words")

#Anger

barplot(trump_anger[1:20,]$n, las = 2, names.arg = trump_anger[1:20,]$word,
        col ="lightblue", horiz = TRUE, main ="Trump's Top 20 most frequent extreme words")

barplot(biden_anger[1:20,]$n, las = 2, names.arg = biden_anger[1:20,]$word,
        col ="lightblue", horiz = TRUE, main ="Biden's Top 20 most frequent extreme words")



