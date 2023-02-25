#criando o banco de dados geral

library(readr) #ler o arquivo csv2
library(lubridate) #manipular datas

rm(list = ls())


dadosfinal <- NULL

dados <- read.csv2("2020-02-23.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-24.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-25.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-26.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-26.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-27.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-28.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-02-29.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-03-01.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-03-02.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-03-03.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-03-04.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-03-05.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)
dados <- read.csv2("2020-03-06.csv",header = TRUE)[,-1]
dadosfinal <- rbind(dadosfinal, dados)

datainicio <- as.Date("2020-03-07")
datafinal <- as.Date("2020-03-17")
dias <- as.numeric(datafinal-datainicio)

dia <- datainicio

for (i in 1:dias ){
  dados<- read.csv2( file=paste(dia,".csv"),header = TRUE)[,-1]
  
  dadosfinal <- rbind(dadosfinal, dados)
  
  dia <- datainicio + ddays(i)
  
}

datainicio <- as.Date("2020-03-19")
datafinal <- as.Date("2020-03-21")
dias <- as.numeric(datafinal-datainicio)

dia <- datainicio

for (i in 1:dias ){
  dados<- read.csv2( file=paste(dia,".csv"),header = TRUE)[,-1]
  
  dadosfinal <- rbind(dadosfinal, dados)
  
  dia <- datainicio + ddays(i)
  
}

write.csv2(dadosfinal,"bancogeral .csv")
view(dadosfinal)
