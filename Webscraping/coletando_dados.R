setwd("~/Projeto Assessoria e Consultoria/Pesquisa de Preços Natal RN/Banco de Dados")

# Bibliotecas

library(tidyverse)
library(rvest)
library(lubridate)
library(curl)
library(stringr)

# rm(list = ls())
# closeAllConnections()

# baixando a data
tempo_inicial <- Sys.time()

data_coleta <- today()
dias <- 0
banco_precos_dia <- c()
banco_precos_final <- c()
contador_dia <- 0

for (contador_dia in 0:92){
  
  # data do checkin 
  data_checkin <- inicio+ddays(contador_dia)
  dia_in <- as.numeric(day(data_checkin))
  mes_in <- as.numeric(month(data_checkin))
  ano_in <- as.numeric(year(data_checkin))
  
  # data do check out
  data_checkout <- data_checkin+ddays(5)
  dia_out <- as.numeric(day(data_checkout))
  mes_out <- as.numeric(month(data_checkout))
  ano_out <- as.numeric(year(data_checkout))
  
  # Identificando quantas propriedades existem para podermos gerar o número de paginas
  
  site <- curl(paste0('https://www.booking.com/searchresults.pt-br.html?ss=Natal&ssne=Natal&ssne_untouched=Natal&label=gen173bo-1DCAEoggI46AdIM1gDaCCIAQGYATG4ARfIAQzYAQPoAQH4AQOIAgGYAgKoAgO4Aunx6Z8GwAIB0gIkNGNhYzQzY2YtMDc5Ny00ODg0LTg2ODMtMWY3OTIzNDM2YWZk2AIE4AIB&sid=f04ed5a00e80a505578f91129f0d1265&aid=304142&lang=pt-br&sb=1&src_elem=sb&src=searchresults&dest_id=-656888&dest_type=city&checkin=',
                      ano_in,'-',mes_in,'-',dia_in,
                      '&checkout=',
                      ano_out,'-',mes_out,'-',dia_out,
                      '&group_adults=2&no_rooms=1&group_children=0&sb_travel_purpose=leisure'),"rb")
  
  site <- read_html(site)

  propriedades <- site %>%
    html_nodes("h1") %>%
    html_text()
  
  propriedades <- as.numeric(regmatches(propriedades,regexpr('[0-9]{2,4}', propriedades)))
  
  paginas <- round(propriedades/25, digits = 0)
  
  #determinas a qtd do offset de acordo com o numero de propriedades
  
  ifelse(propriedades<26, qtd <-  0, qtd <- switch (paginas, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300) )  
  
  #scraping a pagina 
  
  banco_precos_auxiliar <- c()
  banco_precos_dia <- c()
  
  for(contador_pagina in seq(0,qtd,25)){
    contador_pagina <- 0#contador_pagina
    
    #buscando a pagina
    
    pagina_site <- curl(paste0('https://www.booking.com/searchresults.pt-br.html?label=gen173bo-1DCAEoggI46AdIM1gDaCCIAQGYATG4ARfIAQzYAQPoAQH4AQOIAgGYAgKoAgO4Aunx6Z8GwAIB0gIkNGNhYzQzY2YtMDc5Ny00ODg0LTg2ODMtMWY3OTIzNDM2YWZk2AIE4AIB&sid=f04ed5a00e80a505578f91129f0d1265&aid=304142&ss=Natal&ssne=Natal&ssne_untouched=Natal&lang=pt-br&sb=1&src_elem=sb&src=searchresults&dest_id=-656888&dest_type=city&checkin=',
      ano_in,'-',mes_in,'-',dia_in,'&checkout=',
      ano_out,'-',mes_out,'-',dia_out,'&group_adults=2&no_rooms=1&group_children=0&sb_travel_purpose=leisure&offset=',contador_pagina),"rb")
    
    #lendo a pagina
    pagina_site <- read_html(pagina_site)

    nome_hotel <-pagina_site %>%
      html_nodes(".a23c043802") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .}
    
    preco_hotel <- pagina_site %>%
      html_nodes(".fbd1d3018c") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .} %>% 
      data.frame()
    
    preco_hotel <- preco_hotel %>% 
      mutate(preco_hotel=parse_number(str_replace_all(.,"[.]",""))) %>% 
      select(-.)
    
    localizacao <- pagina_site %>%
      html_nodes(".b4273d69aa:nth-child(1)") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .} 
    
    localizacao <- str_split(localizacao,",",simplify = T)[,1]
    
    avaliacao <- pagina_site %>%
      html_nodes(".e46e88563a") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .} %>% 
      data.frame() %>% 
      rename('avaliacao'='.') %>% 
      mutate(avaliacao=as.factor(avaliacao))
    
    qtd_avaliacoes  <- pagina_site %>%
      html_nodes(".db63693c62") %>%
      html_text()%>%
      {if(length(.) == 0) NA else .} %>% 
      data.frame()
    

    
    
    #construindo o banco de dados de cada pagina
    banco_precos_pagina <- data.frame(nome_hotel, preco_hotel, localizacao, stringsAsFactors = F)
    
    #alimentando o banco de dados
    banco_precos_auxiliar <- rbind(banco_precos_auxiliar,banco_precos_pagina)
    
    print(contador_pagina/25)

    Sys.sleep(3)
    
    banco_precos_dia <- cbind(data_coleta,banco_precos_auxiliar,data_checkin)
    
  }
  
  banco_precos_final <- rbind(banco_precos_final, banco_precos_dia)
 
  print(data_checkin)
  #fechar as conexoes a cada 30 dias, esperar 2 min e continuar
  dias <- dias+1
  print(dias)
  if (dias==30){
    dias <- 0
    closeAllConnections()
    Sys.sleep(120)
    next 
  } else {
    # ou descanse por 1 ou 2 segundos e prossiga
    Sys.sleep(sample(2,1))
  }
  
  #acompanhar quantos dias baixou
  dias_baixados <- as.numeric(data_checkin-data_coleta)+1
  print(dias_baixados)
  
}
