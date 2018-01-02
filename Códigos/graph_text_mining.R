# Deletar todos os dados já carregados
rm(list=ls(all=TRUE))

# visualizar diretório atual
getwd()
setwd("INSIRA_AQUI_SUA_PASTA_DE_TRABALHO")


# A função ipak instala e carrega multiplos pacotes no R.
# Ela checa se os pacotes estão instalados, instalas os que não estão, depois carrega e informa o status de todos os pacotes


# criando a função ipak
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Implementando os pacotes que serão usados
packages <- c("rtweet", "dplyr", "ggplot2", "tidytext","tm", "igraph", "ggraph", "tidyr", "widyr")
ipak(packages)


# O nome que você deu para a sua aplicação
appname <- "TesteRJanderson"

## api key (examplo fictício abaixo)
key <- "0PPlbCn27tTGDR94GgZG2xDO8"

## api secret (examplo fictício abaixo)
secret <- "9v1HvT2dTpyH4S9UDX4vjsf42hp6S2Vcjzh32EUIcu2n9ixPHt"

# criando um token chamado "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)



#########################################
#Buscando por Tweets sobre a Anitta


search_anitta <- search_tweets(q = "anitta OR #anitta", retryonratelimit = TRUE, lang = "en", n = 10000, include_rts = FALSE)

# Verificando os dados adquiridos
head(search_anitta$text)


###############################################
#Limpando os dados

# Removendo os http manualmente
search_anitta$stripped_text <- gsub("http.*","",  search_anitta$text)
search_anitta$stripped_text <- gsub("https.*","", search_anitta$stripped_text)


# Removendo a pontuação, convertendo maisúculas em minúsculas, adicionando ID para cada tweet!
search_anitta_clean <- search_anitta %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# plotando as top 15 palavras.
search_anitta_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Contagem",
       y = "Palavras únicas",
       title = "Contagem de palavras únicas encontradas nos tweets")


#Carregando uma lista de stops words que vem do pacote tidytext.
data("stop_words")
# Vendo as 6 primeiras linhas da lista de stop words
head(stop_words)

#Verificando o número de linhas atual de search_anitta_clean.
nrow(search_anitta_clean)
## [1] 128597

# Revendo as stop words do nosso dataframe de análise.
cleaned_tweet_words <- search_anitta_clean %>%
  anti_join(stop_words)

# Dá pra observar que agora temos menos palavras nesse dataframe
nrow(cleaned_tweet_words)
## [1] 70697

# plotando as top 15 palavras.
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Contagem",
       x = "Palavras únicas",
       title = "Contagem de palavras únicas encontradas nos tweets",
       subtitle = "Stop words removidas dessa análise")


##############################################
# Explorando a rede de palavras


# Construção dos bigrama
search_anitta_paired_words <- search_anitta %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

search_anitta_paired_words %>%
  count(paired_words, sort = TRUE)

# separando o bigrama em duas colunas
search_anitta_separated_words <- search_anitta_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")


search_anitta_filtered <- search_anitta_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Nova contagem de bigramas
anitta_words_counts <- search_anitta_filtered %>%
  count(word1, word2, sort = TRUE)

head(anitta_words_counts)



# plotando a rede das palavras gerada pela busca por Anitta
anitta_words_counts %>%
  filter(n >= 30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(color = "red", aes(label = name), vjust = 1.8, size=3) +
  labs(title= "Grafo de Palavras: Anitta ",
       subtitle = "Mineração de dados Textuais de dados do Twitter usando o R ",
       x = "", y = "")

##############################################
# Onde se fala da Anitta?

geo_anitta <- lat_lng(search_anitta)
## plotando o mapa do mundo
par(mar = c(0,0,0,0))
maps::map("world",  lwd = 0.25)
with(geo_anitta, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

