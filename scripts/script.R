# Análise de Sentimentos no Twitter sobre COVID-19 - postagens mais recentes

# Pacotes necessários
library(rtweet)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(stringr)
library(lubridate)
library(reshape2)
library(data.table)

# Autenticação na API do Twitter
# Para ter acesso à API do Twitter é preciso ter uma conta de desenvolvedor
# para fazer isso é só seguir o tutorial:
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# Após aprovação da sua developer account, é necessário autenticar sua sessão no R para que você possa baixar os tweets. Todos os dados expostos abaixo são retirados da sua conta de desenvolvedor

appname <- ''
key <- ''
secret <- ''

twitter_token <- create_token(app = appname,
                              consumer_key = key,
                              consumer_secret = secret)

# Agora vamos baixar os tweets para análise
# Lendo os últimos 10.000 tweets em inglês que envolvam o termo covid19 sem incluir retweets
covid19_tweets <- search_tweets(q = 'covid19',
                                n = 10000,
                                lang = 'en',
                                include_rts = F)

# Para análise de sentimentos, existem algumas listas prontas (lexicons) do pacote tidytext que podem ser bem úteis na análise

# Analisando lexicon nrc
unique(get_sentiments('nrc')$sentiment)

# lexicon nrc desenvolvido por Saif Mohammad e Peter Turney
# Escolhido pois trata de diversos sentimentos associados às palvaras. São eles: positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust

# limpeza, organização e transformação dos dados

# gerando novo dataset de trabalho
# seleção de colunas: user_id (identificador único do tweet), data de criação, texto da publicação
# unnest para quebrar o texto em tokens individuais
# retirada de palavras que não têm significado na análise de sentimentos (stop words)
tidy_tweets <- covid19_tweets %>%
    select(user_id, created_at, text) %>%
    unnest_tokens(word, text, token = 'tweets') %>%
    anti_join(stop_words)

# limpando postagens
tidy_tweets$word <- tidy_tweets$word %>%
    # https://t.co/
    str_replace_all(pattern = 'https://t.co/\\w+', replacement = '') %>%
    # #
    str_replace_all(pattern = '#', replacement = '') %>%
    # @
    str_replace_all(pattern = '@', replacement = '') %>%
    # números
    str_replace_all(pattern = '[[:digit:]]', replacement = '')

# retirando postagens vazias do dataset
tidy_tweets <- tidy_tweets %>%
    filter(!word == '')

# analisando palavras mais recorentes nos tweets baixados
# utilizando a lexicon bing, podemos visualizar um mapa de palavras dividido em palavras negativas e positivas
# foi feito filtro de palavras que podiam poluir o mapa
tidy_tweets %>%
    inner_join(get_sentiments('bing')) %>%
    count(word, sentiment ,sort = T) %>%
    filter(!word %in% c('covid', 'pandemic', 'coronavirus', 'virus')) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"), max.words = 200)

# Podemos observar palavras negativas como mortes, infecções, risco, vulnerabilidade e sintomas.
# Já nas palavras positivas vemos segurança, suporte, aprovação, recuperação e saúde.
# Isso pode indicar o medo das pessoas de se infectarem pelo novo coronavírus bem como suas consequências para saúde, porém também pode apontar um certo sentimento de segurança sobre a recuperação nesta pandemia.

# analisando sentimentos que as palavras dos tweets baixados carregam
tidy_tweets %>%
    inner_join(get_sentiments('nrc')) %>%
    count(sentiment, sort = T) %>%
    mutate(sentiment = reorder(sentiment, n)) %>%
    ungroup() %>%
    ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
    geom_col(show.legend = F) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Sentimentos das postagens sobre covid-19',
         y = 'Quantidade', x = NULL)

# Os sentimentos mais observados nas psotagens se dividem entre positivos, negativos, de confiança e medo.
# Isso pode demosntrar que a população quer acreditar na superação da pandemia, no entanto, ainda há medo do que podemos enfrentar em um futuro próximo.

# quais foram as palavras que contribuiram para esses sentimentos?
# Selecionando apenas os 4 sentimentos mais relevantes observados nas últimas postagens e filtrando palavras que poderiam atrapalhar a análise, apresentamos aquelas palavras que mais contribuiram para cada sentimento.
tidy_tweets %>%
    inner_join(get_sentiments('nrc')) %>%
    count(word, sentiment, sort = T) %>%
    group_by(sentiment) %>%
    filter(sentiment %in% c('positive', 'negative', 'trust', 'fear')) %>%
    filter(!word %in% c('covid', 'pandemic', 'coronavirus', 'virus')) %>%
    mutate(word = reorder(word, n)) %>%
    slice_max(word, n = 5) %>%
    ungroup() %>%
    ggplot(aes(x = n, y = word, fill = sentiment)) +
    geom_col(show.legend = F) +
    facet_wrap(~ sentiment, ncol = 5, scales = 'free_y') +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Palavras que mais contribuem para cada sentimento',
         x = NULL, y = NULL)

# Do sentimento medo, a palavra que mais apareceu foi governo. Talvez demonstre certa insegurança da população sobre as medidas governamentais.
# Do sentimento negativo, observamos palavras como batalha e crise, demonstrando o cenário atual que enfretamos.
# Do sentimento positivo, vemos a palavra vacina, podendo indicar a grande esperança da população para esse ano novo.
# E, por fim, do sentimento de confiança, vemos palavras como sistema, economia e time, podendo indicar certa confiança da população na ação coletiva e na retomada da economia.


# Frequência das palavras ao longo das horas
# Como o termo covid-19 é muito falado no mundo inteiro, os 10.000 posts coletados foram todos realizados no dia 03/01/2020 entre às 7h e 13h.
# Nesse sentido, podemos analisar como a frequência das palavras mais comentadas mudou a cada hora

tidy_tweets %>%
    mutate(time = floor_date(created_at, unit = '1 hour')) %>%
    mutate(time = hour(time)) %>%
    count(word, time) %>%
    mutate(freq = n / sum(n)) %>%
    filter(!word %in% c('covid', 'pandemic', 'coronavirus', 'virus', 'amp', 'india', 'marr','borisjohnson', 'total')) %>%
    filter(freq > .0004) %>%
    group_by(time) %>%
    slice_max(freq, n = 4) %>%
    ungroup() %>%
    ggplot(aes(x = time, y = freq, color = word)) +
    geom_line(size = 1.3) +
    scale_x_continuous(limits = c(7, 13),
                       breaks = seq(7, 13, 1),
                       labels = paste0(seq(7, 13, 1), 'h')) +
    theme(legend.title = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Palavras mais frequentes ao longo das horas',
         y = 'Frequência',
         x = NULL)

# Das palavras mais frequntes, observamos que, diante das postagens coletadas, em nenhum momento deixou-se de falar sobre a vacina. De fato, ela pode ser considerada um dos desejos mais latentes da população mundial e a grande esperança para superação do covid-19 neste ano de 2021.
# Outras palavras como pessoas, mortes e lockdown foram observadas também na maior parte das horas, podendo ser consideradas como preocupeções da população neste cenário de pandemia.

# Conclusão
# Nesta análise rápida buscamos entender o que as pessoas que utilizam o twitter em inglês estavam postando sobre o covid-19 nas últimas horas. Por ser um tópico bastante falado no mundo inteiro, os 10.000 posts coletados referiram-se às  últimas seis horas apenas. Tentei entender quais as palavras foram mais recorrentes, quais eram os sentimentos mais relevantes das postagens e como a frequência das palavras mais postadas mudou ao longo das horas.

# Referências
# "Text Mining with R: A Tidy Approach" was written by Julia Silge and David Robinson. It was last built on 2020-11-10
# Formação Cientista de Dados - Data Science Academy