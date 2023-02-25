setwd("~/Projeto Assessoria e Consultoria/Pesquisa de Pre?os Natal RN/Banco de Dados")

#criando banco de dados 
#Dados de Natal, pesquisa de uma diaria para dois adultos - quarto de categoria de menor pre?o disponivel. Site: Booking.com
#Tipos de hospedagem: Hoteis, Pousadas, Albergues e Resorts - a categoria de menor pre?o dispon?vel
#Objetivo: analisar (evolucao) variacao dos precos de Natal e fazer uma previsao das futuras diarias. 

#bibliotecas
#install.packages("libcurl")
library(lubridate) #operacao com datas
library(rvest) #scraping 
library(devtools)
library(tidyverse) # %>%
library(rlang) #is_empty precisa dessa livraria, importante para verificar se ainda h? propriedades disponiveis
library(curl) #identificar-se ao servidor do site para evitar erro http 502
#library(htmltools) #tentando salvar a pagina em html pra testar o que acontece na pagina
#save_html(page, file ="page.html", background = "white", libdir = "lib")
#  Error in as.character(x) : 
#    cannot coerce type 'externalptr' to vector of type 'character'


rm(list = ls())
rm(banco_precos,banco_precos_dia,banco_precos_i,nomes_i,precos_i,quarto_i)
closeAllConnections()
# baixando a data
tempo.inicial <- Sys.time()

inicio <- today()
dias <- 0
banco_precos_dia <- c()
banco_precos_final <- c()
i <- 0

for (i in 0:731) {
 
# data do checkin 
  diacheckin <- inicio+ddays(i)
  diain <- as.numeric(day(diacheckin))
  mesin <- as.numeric(month(diacheckin))
  anoin <- as.numeric(year(diacheckin))
  
# data do check out
  diacheckout <- diacheckin+ddays(1)
  diaout <- as.numeric(day(diacheckout))
  mesout <- as.numeric(month(diacheckout))
  anoout <- as.numeric(year(diacheckout))
  
# a.scraping o numero de paginas - identificando na primeira busca quantas propriedades existem para podermos gerar o n?mero de paginas
  
  site <- curl(paste0('https://www.booking.com/searchresults.pt-br.html?aid=304142&label=gen173nr-1FCAEoggI46AdIM1gEaCCIAQGYAS24ARfIAQzYAQHoAQH4AQuIAgGoAgO4AvCp5vIFwAIB&sid=b0ea1003a80543236a20e94559c4ed28&tmpl=searchresults&checkin_month=',mesin,'&checkin_monthday=',diain,'&checkin_year=',anoin,'&checkout_month=',mesout,'&checkout_monthday=',diaout,'&checkout_year=',anoout,'&city=-656888&class_interval=1&dest_id=-656888&dest_type=city&from_sf=1&group_adults=2&group_children=0&label_click=undef&no_rooms=1&raw_dest_type=city&room1=A%2CA&sb_price_type=total&shw_aparth=1&slp_r_match=0&src=searchresults&srpvid=cb3ea243491000f8&ss=Natal&ssb=empty&ssne=Natal&ssne_untouched=Natal&top_ufis=1&nflt=ht_id%3D203%3Bht_id%3D216%3Bht_id%3D206%3Bht_id%3D204%3B&percent_htype_hotel=1&rsf='),"rb")
  
  pagina <- read_html(site)
 
#determinar o numero de propriedades   
  propriedades <- pagina %>%
    html_nodes("h1") %>%
    html_text()
  
  propriedades <- as.numeric(regmatches(propriedades,regexpr('[0-9]{2,4}', propriedades)))
  
  #caso nao tenha mais propriedade encerrar as conexoes, salvar o codigo e finalizar a execucao.
  if(is_empty(propriedades)==TRUE){
    #fechar as conex?es 
    closeAllConnections()
    #remover duplicidades - alguns hoteis aparecem em mais de uma pagina
    banco_precos_final <- unique(banco_precos_final)
    #limpar os precos
    banco_precos_final$precos_i<-strtrim(banco_precos_final$precos_i,25)
    banco_precos_final$precos_i<- gsub("[.]", "", banco_precos_final$precos_i)
    banco_precos_final$precos_i<-suppressWarnings(parse_number(banco_precos_final$precos_i))
    #incluir coluna do dia da semana
    banco_precos_final$diasemana <- wday(banco_precos_final$diacheckin, label = T)
    #salvar o banco de dados
    setwd("~/Projeto Assessoria e Consultoria/Pesquisa de Pre?os Natal RN/Banco de Dados/Dados limpos")
    write.csv2(banco_precos_final,file = paste(inicio,".csv"))
    #calcular o tempo de execu??o
    tempo.final <- Sys.time()
    print(tempo.final-tempo.inicial)
    #finalizar execu??o
    stop("N?o h? propriedades com tarifas dispon?veis")
  } else{
  #determinar o numero de paginas de acordo com o numero de propriedades
  paginas <- round(propriedades/25, digits = 0)
  
  #determinas a qtd do offset de acordo com o numero de propriedades
  
  ifelse(propriedades<26, qtd <-  0, qtd <- switch (paginas, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300) )  
  
  
  #scraping a pagina 
  
  banco_precos <- c()
  banco_precos_i <- c()
  
  for(j in seq(0,qtd,25)){
    url_number <- j
    
    #buscando a pagina
    
    url <- curl(paste0('https://www.booking.com/searchresults.pt-br.html?aid=304142&label=gen173nr-1FCAEoggI46AdIM1gEaCCIAQGYAS24ARfIAQzYAQHoAQH4AQuIAgGoAgO4AvCp5vIFwAIB&sid=b0ea1003a80543236a20e94559c4ed28&tmpl=searchresults&checkin_month=',mesin,'&checkin_monthday=',diain,'&checkin_year=',anoin,'&checkout_month=',mesout,'&checkout_monthday=',diaout,'&checkout_year=',anoout,'&city=-656888&class_interval=1&dest_id=-656888&dest_type=city&dtdisc=0&from_sf=1&group_adults=2&group_children=0&inac=0&index_postcard=0&label_click=undef&nflt=ht_id%3D203%3Bht_id%3D204%3Bht_id%3D206%3Bht_id%3D216%3B&no_rooms=1&postcard=0&room1=A%2CA&sb_price_type=total&shw_aparth=1&slp_r_match=0&src=searchresults&src_elem=sb&srpvid=3de2a5cdcd850113&ss=Natal&ss_all=0&ssb=empty&sshis=0&ssne=Natal&ssne_untouched=Natal&top_ufis=1&rows=25&offset=',url_number), "rb")
      
    #lendo a pagina
    page <- read_html(url)
    
#    quarto <- page %>%
#          html_nodes('.room_link') %>%
#          map_df(~list(quarto_i=html_nodes(.x,'.room_link ,  .sold_out_property,')%>%
#                    html_text()%>%
#                    {if(length(.) == 0) NA else .}
#                   ))
#   
#    View(cama) 
#    cama <-page %>%
#      html_nodes(".room_link") %>%
#      html_text()%>%
#      {if(length(.) == 0) NA else .}
    

    #nome dos hoteis
   nomes_i <-page %>%
      html_nodes(".sr-hotel__name") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .}

    #nome do quarto
  # if (diacheckin==as.Date("2020-09-10")) {quarto_i <- NA} else{
   #quarto_i <- page%>%
    # html_nodes(".room_link strong,  .sold_out_property") %>%
     #html_text()%>%
     #{if(length(.) == 0) NA else .}
   
   #.room_link strong
    quarto_i <- NA

    #precos
    precos_i <- page %>%
      html_nodes(".bui-price-display__value , .sold_out_property ") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .}
    
    #.prc-d-sr-wrapper, .prco-ltr-right-align-helper , .bui-alert__text

    #construindo o banco de dados de cada p?gina
    banco_precos_i <- data.frame(nomes_i, quarto_i, precos_i, stringsAsFactors = F)

    #alimentando o banco de dados de um dia
    banco_precos <- rbind(banco_precos ,banco_precos_i)
    
    #verificar a pagina em que esta
    print(j/25)
    
    #suspender execucao no R por 3 seg
    Sys.sleep(3)

banco_precos_dia <- cbind(inicio,banco_precos,diacheckin)

}
  
banco_precos_final <- rbind(banco_precos_final, banco_precos_dia)
  #Verificar o dia em que esta
  print(diacheckin)
  
  #fechar as conexoes a cada 90 dias, esperar 5 min e continuar
  dias <- dias+1
  print(dias)
  
  if (dias==90){
    dias <- 0
    closeAllConnections()
    Sys.sleep(300)
    next 
  } else {
    # ou descanse por 1 ou 2 segundos e prossiga
    Sys.sleep(sample(2,1))
  }
 #acompanhar quantos dias j? baixou
  dias.baixados <- as.numeric(diacheckin-inicio)+1
 print(dias.baixados)
}
}

View(banco_precos_final)
closeAllConnections()
write.csv2(banco_precos_final,file = "2020-06-26 -incom1.csv")

View(quarto_i)
#Observações dos dados baixados:
  
#1) deu erro de time out do site no dia 24/02/2020, os dados foram baixados apenas até dia 11/12/2020;

#2) a 00:35h o site vira o dia e o link perde conectividade (ocasionando o erro de time out), os dados do dia 25/02/2020 foram baixados apenas ate dia 10/05/2020; 

#3)coloquei os dados para baixar a partir de 20h, mas não conseguiu completar até 00h35, dia 26/02/2020 os dados foram baixados até 01/08/2020. está gastando muito tempo para baixar pois está verificando 10 páginas, sendo que em alguns dias o número de páginas é menor e ao não encontrar o offset dá erro. Necessário pensar em uma forma de otmizar esse processo e verificar o número de páginas. Removi a suspensao de 3 seg ao final do ciclo do dia para ver se otimiza o tempo e diminui para 2 dentro do ciclo da página, e nao resolveu, irei gerar um laço para verificar o número de páginas : .sr_pagination_link

#27/02 , baixou até 28/08/2020 - Error in read_connection_(x, n) : 
#Evaluation error: Failure when receiving data from the peer.
#mandei continuar baixando a partir de 28/08/2019 para tentar mesclar o banco de dados: 23h58, vamos ver até quando vai conseguir baixar. - não parou as 00h35 - baixou apenas ate 24/10/2020 na pg 8

#28/02, baixou até pg 3 11/08/2020 - Error in open.connection(x, "rb") : HTTP error 502. Inclui um laço para encerrar todas as conexões após 120 dias e esperar 5 minutos antes de continuar

#29/02, as alterações no codigo permitiram baixar 496 dias (ate 08/07/2021). apareceu um novo erro relacionado ao número de páginas: " Error in seq.default(0, qtd, 25) : 'to' deve ter comprimento 1" , acredito que por não haver informações na página ele não conseguiu prosseguir baixando. procurar uma forma de quando dar esse tipo de erro encerrar todas as conexões e salvar os dados. inclui um ifelse para caso a qtd de propriedades for menor de 26 estabelecer a qtd como 0 e se propriedades for igual a zero fechar conexões e salvar o arquivo. 

# 01/03: parou no dia 17/09/2020 Error in open.connection(x, "rb") : 
#Timeout was reached: Operation timed out after 10000 milliseconds with 0 out of 0 bytes received , investigar esse erro e descobrir como contornar

#parou em 13/04/2021 - salvei e mandei continuar baixando . o laço para verificar se chegou a 0 propriedades não funcionou, propriedades retornou como numeric (empty) e apareceu a seguinte mensagem: Error in if (propriedades == 0) { : argument is of length zero. baixou 502 dias com os incrementos no codigo. foi necessario intervir para continuar baixando, o erro de perda de conexão persiste. 

#02/03 - apareceu apenas o erro de propriedades == 0, sem falhas de conexão. 

#04/03 - computador desligou antes de baixar tudo, tentei continuar baixando os demais dias para continuar, talvez seja necessário fazer uma limpeza no dia 04/11/2020 deste dia

#05/03 - Error in open.connection(x, "rb") : 
#Timeout was reached: Operation timed out after 10006 milliseconds with 0 out of 0 bytes received retomei a baixar de 14/01/2021

#12/03 - Error in open.connection(x, "rb") : HTTP error 502.

#14/03 -  Error in open.connection(x, "rb") : 
# Timeout was reached: Connection timed out after 10004 milliseconds - baixou ate 12/03/2021

#18/03 - O R travou durante a execu??o e n?o foi poss?vel salvar ou resgatar os dados

#21/03 - finalmente incrementei o c?digo para corrigir o pre?o, deixando apenas o valor numerico. foi necess?rio inserir um comando de suppressWarnings() no parse number pq nos dias em que n?o h? pre?o ele aparece um warning informando que havia apenas caracteres e ser? gerado um NA. incluido tamb?m o dia da semana. 

#21/03 - passei a baixar tb o nome do quarto pois as vezes a mudan?a de tarifa ? em fun??o do esgotamento de uma categoria mais barata. 

# 23/03 - dia 10/09 estava dando algum erro com rela??o ao n?mero de quartos
# Error in data.frame(nomes_i, quarto_i, precos_i, stringsAsFactors = F) : 
# arguments imply differing number of rows: 16, 15

#24/03 - na pagina 5 est? dando erro ao baixar o nome do quarto novamente. 
#Error in data.frame(nomes_i, quarto_i, precos_i, stringsAsFactors = F) : 
#arguments imply differing number of rows: 18, 17

#25/03  dados baixados at? dia 30/09/2020 - Error in open.connection(x, "rb") : 
# Timeout was reached: Resolving timed out after 10000 milliseconds

#26/03 (Escrito por Fabr?cio) Dados foram baixados at? dia 11/06/2020 e o erro dado foi:
#Error in open.connection(x, "rb") : HTTP error 502.

# 28/03 - VERIFICAR AS DATAS ANTES DE UNIFICAR, ACREDITO QUE A PARTE DOI SERA A PARTE 1. PARTE DOI BAIXADA ATE 18/08/2020, PARTE 1 BAIXADA ATE 01/01/2021, NO ENTANTO ACHO QUE COMECA EM 03/07. 

#01/04 - Error in open.connection(x, "rb") : HTTP error 502. . BAIXOU APENAS ATE O DIA 22/09/2020

#02/04 - Error in open.connection(x, "rb") : HTTP error 502. . BAIXOU APENAS ATE 30/07/2020

#03/04 - tentativa de implementar o c?digo para voltar a baixar o nome do quarto e evitar o error http 502
    #library(libcurl) #nao disponivel para vers?o 3.6.1 - instalando a vers?o mais     recente
    #instalando ultima vers?o de libcurl - n?o foi necess?rio, library curl funciona
    #require(devtools)
    #libcurlVersion()
    #libcurl195 <- "https://cran.r-project.org/src/contrib/Archive/RCurl/RCurl_1.95-4   .12#.tar.gz"
    #install.packages(libcurl195,repos = NULL, type = "source")

# BANCO DE PRE?OS FINAL DO DIA 28/04 VEIO COM NA NA COLUNA PRE?O