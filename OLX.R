extrairAnuncios <- function(listaPaginas, info_adicional = TRUE) {
  
  library(magrittr) # não vivo sem esse pacote
  library(rvest) # principal pacote para web-scraping
  library(readr) # usado para extrair numeros de texto
  library(stringr) # usado para o data cleaning
  library(curl) # usado como suporte para o rvest
  library(tidyr) # data cleaning
  library(dplyr) # data cleaning
  
  
  base <- NULL
  
  for(i in 1:length(listaPaginas)){
  
  mycurl <- curl(listaPaginas[i], handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  mycurl <- read_html(mycurl)
  
  x <- mycurl %>% html_nodes(".OLXad-list-link")
  
  # extrair link do anuncio
  col_links <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("href")
  # extrair titulo do anuncio
  col_titles <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("title")
 
   # extrair preço
  precos <- lapply(x, . %>% html_nodes(".col-3"))
  precos %<>% lapply(html_text)
  precos %<>% unlist()
  precos <- str_extract(precos, pattern = "(([[:space:]]+))([0-9].)([[:punct:]]+)([0-9]{3})")
  precos <- str_trim(precos)
  precos <- str_replace(precos,"[[:punct:]]+","")
  precos %<>% as.numeric()
  col_precos <- precos
 
   # extrair Kilometragem
  km <- lapply(x, . %>% html_nodes(".col-2"))
  km %<>% lapply(html_text)
  km %<>% unlist()
  km <- str_extract(km, pattern = "(([[:space:]]+))([0-9]{1,2})([[:punct:]]+)([0-9]{3})")
  km %<>% str_replace_all("[\t]", "")
  km %<>% str_replace_all("[\n]", "")
  km %<>% str_replace_all("[[:punct:]]", "")
  km <- str_trim(km)
  km <- as.numeric(km)
  col_km <- km
  
   # extrair bairros
  bairros <- mycurl %>% html_nodes(".OLXad-list-line-2") %>% html_text()
  bairros %<>% str_replace_all("[\t]", "")
  bairros %<>% str_replace_all("[\n]", "")
  bairros %<>% str_replace_all("Anúncio Profissional", "")
  bairros %<>% str_replace("-", "")
  bairros %<>% str_trim()
  col_bairros <- bairros
  

  if (info_adicional) {
    adicional <- mycurl %>% html_nodes(".mt5px") %>% html_text()
    adicional %<>% str_replace_all("[\t]", "")
    adicional %<>% str_replace_all("[\n]", "")
    col_adicionais <- adicional
    
  }
  
  basex <- data.frame(link = col_links,
                      titulo = col_titles,
                      preco = col_precos,
                      km = col_km,
                      bairro = col_bairros,
                      infadicional = col_adicionais,
                      stringsAsFactors = FALSE)
  
  base <- rbind(base,basex)
  
}
  
  return(base)
}


url_pagina <- "http://pr.olx.com.br/regiao-de-curitiba-e-paranagua/veiculos-e-acessorios/carros"

number_pages <- 100

lista_urls <- paste0(url_pagina, "?o=", 1:number_pages)

Dados <- extrairAnuncios(lista_urls)


