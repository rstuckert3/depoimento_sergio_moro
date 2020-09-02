# Autor: Rodrigo Stuckert
# Data: 2020-05-05


# Esse código foi feito para analisar o depoimento do ex-ministro Sérgio Moro
# à Polícia Federal, tornado público no dia 05 de maio de 2020.


# Link para o texto original (último acesso em 2020-05-05):
# https://www.cnnbrasil.com.br/politica/2020/05/05/exclusivo-leia-a-integra-do-depoimento-de-sergio-moro-a-pf


# Pacotes:
library(dplyr)
library(tidytext)
library(tm) # Text mining
library(wordcloud2)


# User functions:

# Remove, de um dataframe "x", os elementos constante em um vetor "y"
'%!in%' <- function(x,y)!('%in%'(x,y))

DFNgram <- function(df, num_ngram = 3){
  " Recebe um dataframe SEM filtro de stopwords e retorna uma tabela de frequências para um ngram.
  Recomenda-se o uso de uma ou de três palavras. No caso de num_ngram = 1, deve-se remover stopwords
  previamente.
  
  df: dataframe (OBS: não remover stopwords se num_ngram >= 2).
  ngram: número de palavras do ngram. Por default, temos ngram = 3 (sequências de 3 palavras).
  "
  new_df <- df
  
  if(num_ngram == 1){
    new_df_ngram <- unnest_tokens(new_df, palavra, texto) # Quebra o texto em palavras
  } else {
    new_df_ngram <- unnest_tokens(new_df, palavra, texto, token = "ngrams", n = num_ngram) # Quebra o texto em grupos de num_gram palavras.  
  }
  
  new_df_freq <- count(new_df_ngram, palavra, sort = TRUE) #frequência de palavras, ordenadas em ordem decrescente.
  return(new_df_freq)
}



# Arquivo usado
file <- "inputs/depoimento_original.txt"

# Lê o texto
depoimento_raw <- scan(file, what="character", sep="\n",encoding="UTF-8") # A função scan carrega os textos assim como uma leitor de CSV, mas funciona melhor para texto.
depoimento_raw[1:5] # Verificando os cinco primeiro parágrafos do texto
#View(depoimento_raw)

# Remove trechos do texto que não são estritamente pertencentes ao depoimento em si.
depoimento_moro <- depoimento_raw[-c(1, 32:37)]
depoimento_moro[153] <- gsub(";.*","",depoimento_moro[153])

# Escolhe apenas o texto em si, e remove as stopwords
depoimento_moro <- removeWords(depoimento_raw, c(stopwords("pt"), "que", "Que", "QUE")) # OBS: o método removeWords só funciona com vetores de strings e é caps sensitive.

# Análise exploratória: quantas vezes "Moro" e "Presidente" são citados
length(grep("Moro", depoimento_moro))
length(grep("Presidente", depoimento_moro))

# Cria um tibble com o texto, e depois o quebra em palavras.
df_depoimento <- tibble("linha"= 1:length(depoimento_moro),"texto"=depoimento_moro)
freq <- DFNgram(df_depoimento, num_ngram = 1)
head(df_depoimento)
head(freq)


# Gráfico. Note que, se você não salvar em um tamanho grande o suficiente,
# o termo "Presidente" não aparecerá no gráfico.
set.seed(20)
wordcloud2(freq, color = "random-light", 
           shape = "circle",
           backgroundColor = "white") # Salve preferencialmente em 1200x1200