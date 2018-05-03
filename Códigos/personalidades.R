
# Deletar todos os dados já carregados
rm(list=ls(all=TRUE))

# visualizar diretório atual e/ou ir para um novo diretório
getwd()
setwd("~/R/Teste/16personalidades")


# A função ipak instala e carrega multiplos pacotes no R.
# Ela checa se os pacotes estão instalados, instalas os que não estão, depois carrega e informa o status de todos os pacotes
# criando a função ipak:
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Implementando os pacotes que serão usados
packages <- c("rtweet", "textcat","cld2", "dplyr","tm", "SnowballC", 'wordcloud', 'topicmodels',"plotly")

#Executando a função
ipak(packages)

#################################################################

## whatever name you assigned to your created app
appname <- "TesteRJanderson"

## api key (example below is not a real key)
key <- "0PPlbCn27tTGDRgdfgd94GgZG2xDO8"

## api secret (example below is not a real key)
secret <- "9v1HvT2dTpyH4S9UDgdfgdfX4vjshgf42hp6S2Vcjzh32EUIcu2n9ixPHt"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)



#######################################################

# Buscando por usuários com a sigla ESTP
ESTP <- search_users(q = "ESTP", n = 1000)


# Todas as siglas pesquisadas
letters <- c("INTJ", "INTP","ENTJ","ENTP", "INFJ", "INFP","ENFJ", "ENFP", "ISTJ", "ISFJ", "ESTJ", "ESFJ", "ISTP", "ISFP", "ESTP", "ESFP")


# Busca única realidade para todas as siglas
personalidades <- Map(
  "search_users",
       letters,
    n = 1000
)


# Criando um subset apenas com os usuários que possuem texto na descrilão do perfil
ESTP <- users_data(subset(personalidades$ESTP, description != ""))

# Detectando a lingua usada na descrição do perfil usando 2 pacotes de identificação de língua
teste_lingua_ESTP <- cbind.data.frame(textcat(ESTP$description),tolower(detect_language(ESTP$description,lang_code = FALSE)),
                                      ESTP$description)

# Dando novos nomes as colunas do data.frame criado no passo acima.
colnames(teste_lingua_ESTP) <-c("textcat","cdl2","description")

# Criando um data.frame apenas com os perfis que passaram pelo duplo filtro de lingua. 
ESTP_EN <- filter(teste_lingua_ESTP, textcat == "english" & cdl2 == "english")


ESFP <- users_data(subset(personalidades$ESFP, description != ""))

teste_lingua_ESFP <- cbind.data.frame(textcat(ESFP$description),tolower(detect_language(ESFP$description,lang_code = FALSE)),
                                      ESFP$description)

colnames(teste_lingua_ESFP) <-c("textcat","cdl2","description")

ESFP_EN <- filter(teste_lingua_ESFP, textcat == "english" & cdl2 == "english")


INTJ <- users_data(subset(personalidades$INTJ, description != ""))

teste_lingua_INTJ <- cbind.data.frame(textcat(INTJ$description),tolower(detect_language(INTJ$description,lang_code = FALSE)),
                                      INTJ$description)

colnames(teste_lingua_INTJ) <-c("textcat","cdl2","description")

INTJ_EN <- filter(teste_lingua_INTJ, textcat == "english" & cdl2 == "english")


####

INTP <- users_data(subset(personalidades$INTP, description != ""))

teste_lingua_INTP <- cbind.data.frame(textcat(INTP$description),tolower(detect_language(INTP$description,lang_code = FALSE)),
                                      INTP$description, INTP$screen_name)

colnames(teste_lingua_INTP) <-c("textcat","cdl2","description", "screen_name")

INTP_EN <- filter(teste_lingua_INTP, textcat == "english" & cdl2 == "english")


####

ENTJ <- users_data(subset(personalidades$ENTJ, description != ""))

teste_lingua_ENTJ <- cbind.data.frame(textcat(ENTJ$description),tolower(detect_language(ENTJ$description,lang_code = FALSE)),
                                      ENTJ$description)

colnames(teste_lingua_ENTJ) <-c("textcat","cdl2","description")

ENTJ_EN <- filter(teste_lingua_ENTJ, textcat == "english" & cdl2 == "english")

#####

ENTP <- users_data(subset(personalidades$ENTP, description != ""))

teste_lingua_ENTP <- cbind.data.frame(textcat(ENTP$description),tolower(detect_language(ENTP$description,lang_code = FALSE)),
                                      ENTP$description)

colnames(teste_lingua_ENTP) <-c("textcat","cdl2","description")

ENTP_EN <- filter(teste_lingua_ENTP, textcat == "english" & cdl2 == "english")

#####

INFJ <- users_data(subset(personalidades$INFJ, description != ""))

teste_lingua_INFJ <- cbind.data.frame(textcat(INFJ$description),tolower(detect_language(INFJ$description,lang_code = FALSE)),
                                      INFJ$description)

colnames(teste_lingua_INFJ) <-c("textcat","cdl2","description")

INFJ_EN <- filter(teste_lingua_INFJ, textcat == "english" & cdl2 == "english")


#####

INFP <- users_data(subset(personalidades$INFP, description != ""))

teste_lingua_INFP <- cbind.data.frame(textcat(INFP$description),tolower(detect_language(INFP$description,lang_code = FALSE)),
                                      INFP$description)

colnames(teste_lingua_INFP) <-c("textcat","cdl2","description")

INFP_EN <- filter(teste_lingua_INFP, textcat == "english" & cdl2 == "english")



ENFJ <- users_data(subset(personalidades$ENFJ, description != ""))

teste_lingua_ENFJ <- cbind.data.frame(textcat(ENFJ$description),tolower(detect_language(ENFJ$description,lang_code = FALSE)),
                                      ENFJ$description)

colnames(teste_lingua_ENFJ) <-c("textcat","cdl2","description")

ENFJ_EN <- filter(teste_lingua_ENFJ, textcat == "english" & cdl2 == "english")


ENFP <- users_data(subset(personalidades$ENFP, description != ""))

teste_lingua_ENFP <- cbind.data.frame(textcat(ENFP$description),tolower(detect_language(ENFP$description,lang_code = FALSE)),
                                      ENFP$description)

colnames(teste_lingua_ENFP) <-c("textcat","cdl2","description")

ENFP_EN <- filter(teste_lingua_ENFP, textcat == "english" & cdl2 == "english")



ISTJ <- users_data(subset(personalidades$ISTJ, description != ""))

teste_lingua_ISTJ <- cbind.data.frame(textcat(ISTJ$description),tolower(detect_language(ISTJ$description,lang_code = FALSE)),
                                      ISTJ$description)

colnames(teste_lingua_ISTJ) <-c("textcat","cdl2","description")

ISTJ_EN <- filter(teste_lingua_ISTJ, textcat == "english" & cdl2 == "english")


ISFJ <- users_data(subset(personalidades$ISFJ, description != ""))

teste_lingua_ISFJ <- cbind.data.frame(textcat(ISFJ$description),tolower(detect_language(ISFJ$description,lang_code = FALSE)),
                                      ISFJ$description)

colnames(teste_lingua_ISFJ) <-c("textcat","cdl2","description")

ISFJ_EN <- filter(teste_lingua_ISFJ, textcat == "english" & cdl2 == "english")


ESTJ <- users_data(subset(personalidades$ESTJ, description != ""))

teste_lingua_ESTJ <- cbind.data.frame(textcat(ESTJ$description),tolower(detect_language(ESTJ$description,lang_code = FALSE)),
                                      ESTJ$description)

colnames(teste_lingua_ESTJ) <-c("textcat","cdl2","description")

ESTJ_EN <- filter(teste_lingua_ESTJ, textcat == "english" & cdl2 == "english")


ESFJ <- users_data(subset(personalidades$ESFJ, description != ""))

teste_lingua_ESFJ <- cbind.data.frame(textcat(ESFJ$description),tolower(detect_language(ESFJ$description,lang_code = FALSE)),
                                      ESFJ$description)

colnames(teste_lingua_ESFJ) <-c("textcat","cdl2","description")

ESFJ_EN <- filter(teste_lingua_ESFJ, textcat == "english" & cdl2 == "english")


ISTP <- users_data(subset(personalidades$ISTP, description != ""))

teste_lingua_ISTP <- cbind.data.frame(textcat(ISTP$description),tolower(detect_language(ISTP$description,lang_code = FALSE)),
                                      ISTP$description)

colnames(teste_lingua_ISTP) <-c("textcat","cdl2","description")

ISTP_EN <- filter(teste_lingua_ISTP, textcat == "english" & cdl2 == "english")


ISFP <- users_data(subset(personalidades$ISFP, description != ""))

teste_lingua_ISFP <- cbind.data.frame(textcat(ISFP$description),tolower(detect_language(ISFP$description,lang_code = FALSE)),
                                      ISFP$description)

colnames(teste_lingua_ISFP) <-c("textcat","cdl2","description")

ISFP_EN <- filter(teste_lingua_ISFP, textcat == "english" & cdl2 == "english")




###################
# Entrando com o texto a ser tratado
texto <-  ESTP_EN

texto$description  <- gsub("•" ,"",  texto$description)
texto$description  <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)","",  texto$description)
review_corpus = Corpus(VectorSource(texto$description))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", tolower(letters), stopwords("english")))
review_corpus =  tm_map(review_corpus, stripWhitespace)

inspect(review_corpus[1])


dtm <- TermDocumentMatrix(review_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

# Pltando em uma wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=30, random.order=FALSE, rot.per=0.20, 
          colors=brewer.pal(8, "Dark2"))


# Criando um data frame com os dados de tamanho de cada personalidade após a extração
n_rows <- as.data.frame.numeric(c(nrow(INTJ),nrow(INTP),nrow(ENTJ),nrow(ENTP),nrow(INFJ),nrow(INFP),nrow(ENFJ),
                                  nrow(ENFP),nrow(ISTJ),nrow(ISFJ),nrow(ESTJ),nrow(ESFJ),nrow(ISTP),nrow(ISFP),
                                  nrow(ESTP),nrow(ESFP)))

# Criando um data frame com os dados de tamanho de cada personalidade após o tratamento linguístico
n_rows_EN <- as.data.frame.numeric(c(nrow(INTJ_EN),nrow(INTP_EN),nrow(ENTJ_EN),nrow(ENTP_EN),nrow(INFJ_EN),nrow(INFP_EN),nrow(ENFJ_EN),
               nrow(ENFP_EN),nrow(ISTJ_EN),nrow(ISFJ_EN),nrow(ESTJ_EN),nrow(ESFJ_EN),nrow(ISTP_EN),nrow(ISFP_EN),
               nrow(ESTP_EN),nrow(ESFP_EN)))
               
               


# Criando um data.frame com os dados de tamanho das personalidades após extração, após o pré-processamento e uma relação entre esses valores;
quantidade <- cbind(as.factor(letters),n_rows,n_rows_EN,n_rows/n_rows_EN)

# Inserindo título nas colunas
colnames(quantidade) <- c("personalidade", "extraida","pre_processamento", "relacao")

# Observando a correlação dos dados (com o objetivo de analisar possíveis anormalidades)
cor(quantidade$pre_processamento, quantidade$extraida)

# plotando o gráfico
p_quantidade <- plot_ly(quantidade, x = ~extraida, y = ~pre_processamento, type = 'scatter', mode = 'markers', size = ~relacao, color = ~personalidade, colors = 'Paired',
                        sizes = c(10, 50),
                        marker = list(opacity = 0.5, sizemode = 'diameter'),
                        hoverinfo = 'text',
                        text = ~paste('Personalidade:', personalidade, '<br>Quantidade na Extração:', extraida,
                                      '<br>Quantidade no Pré-Processamento:', pre_processamento)) %>%
  layout(title = 'Gráfico de Eficiência em Detecção de Língua por Personalidade-MBTI',
         xaxis = list(title = "Quantidade na Extração", showgrid = FALSE),
         yaxis = list(title = "Quantidade no Pré-Processamento", showgrid = FALSE),
         showlegend = TRUE,
         legend = list(x = 100, y = 0.5)
  )

p_quantidade


# Criando um data.frame com a quantidade de dados após o pré-processamento linguístico
Per_pre_processamento <- (quantidade$pre_processamento/sum(quantidade$pre_processamento))*100

# Criando um data.frame com a quantidade de dados extraída na busca
Per_extracao <- (quantidade$extraida/sum(quantidade$extraida))*100

# # Criando um data.frame com os dados de percentual existente nos EUA de cada personalidade
Per_by_MBTI <- as.numeric(c(2.1,3.3,1.8, 3.2, 1.5, 4.4, 2.5, 8.1, 11.6,
                            13.8, 8.7, 12.3, 5.4, 8.8, 4.3, 8.5))

# Juntando os data.frames acima.
comparativo <- cbind.data.frame(letters,Per_pre_processamento,Per_extracao,Per_by_MBTI)
colnames(comparativo) <- c("Personalidades","Per_pre_processamento","Per_extracao","Per_by_MBTI")

# Exibindo os dados
comparativo

#   Personalidades Per_pre_processamento Per_extracao Per_by_MBTI
#1            INTJ          11.579980     9.473795         2.1
#2            INTP           9.911678     8.696565         3.3
#3            ENTJ           8.243376     8.181914         1.8
#4            ENTP           7.777233     7.436194         3.2
#5            INFJ          11.555447     9.620838         1.5
#6            INFP          10.917566     9.473795         4.4
#7            ENFJ           8.071639     8.286945         2.5
#8            ENFP           9.396467     8.843609         8.1
#9            ISTJ           4.661433     5.051990        11.6
#10           ISFJ           4.857704     5.461611        13.8
#11           ESTJ           2.600589     3.413507         8.7
#12           ESFJ           2.355250     3.129923        12.3
#13           ISTP           2.355250     3.823128         5.4
#14           ISFP           2.551521     3.424010         8.8
#15           ESTP           1.349362     2.499737         4.3
#16           ESFP           1.815505     3.182439         8.5


# plotando o gráfico
p_comparativo <- plot_ly(data = comparativo, color = I("gray80")) %>%
  add_segments(x = ~Per_extracao, xend = ~Per_pre_processamento, y = ~Personalidades, yend = ~Personalidades, name = "Diferença percentual",showlegend = TRUE) %>%
  add_markers(x = ~Per_extracao, y = ~Personalidades, name = "Percentagem na Extração", color = I("blue")) %>%
  add_markers(x = ~Per_by_MBTI, y = ~Personalidades, name = "Percentagem Oficial", color = I("red")) %>%
  add_markers(x = ~Per_pre_processamento, y = ~Personalidades, name = "Percentagem no Pré-Processamento", color = I("Green")) %>%
  
  layout(
    title = "Comparativo de Percentagem Entre as 16 Personalidades",
    xaxis = list(title = "Percentual(%)"),
    margin = list(l = 100),
    legend = list(x = 100, y = 0.5)
  )

p_comparativo

