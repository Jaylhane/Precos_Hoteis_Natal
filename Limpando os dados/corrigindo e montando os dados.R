
#criando um banco de dados unico para os dias descontinuados, remover dados duplicados, corrigir o preco, criando uma coluna NA para o nome do quarto (até 21/03), e incluir o dia da semana

#bibliotecas
library(readr) #ler o arquivo csv2
library(tidyverse) #inclui pacote dplyr para limpar dados duplicadose ordenar o banco de dados
library(lubridate) #incluir dia da semana

rm(list = ls())
#abrindo os dados

setwd("~/Projeto Assessoria e Consultoria/Pesquisa de Preços Natal RN/Banco de Dados")

dados1<-read.csv2("2020-03-19 -incom-noite.csv",header=T)
dados2 <- read.csv2("2020-03-19 noite.csv", header = T)
#dados3 <- read.csv2( "2020-03-05 .csv", header = T)

dados <- rbind(dados1,dados2)[,-1]

#removendo dados duplicados

dados <- unique(dados)

#corrigindo o preco
dados$precos_i<-strtrim(dados$precos_i,20)
dados$precos_i<- gsub("[.]", "", dados$precos_i)
dados$precos_i<-suppressWarnings(parse_number(dados$precos_i))

#criar coluna quarto_i vazia

dados$quarto_i <- NA

#criar coluna dia da semana
dados$diasemana <- wday(dados$diacheckin, label = T)

#ordenando o banco de dados

dados <- dados %>%
  select(inicio, nomes_i, quarto_i, precos_i, diacheckin, diasemana)

setwd("~/Projeto Assessoria e Consultoria/Pesquisa de Preços Natal RN/Banco de Dados/Dados limpos")

write.csv2(dados,"2020-03- .csv")

view(dados)
