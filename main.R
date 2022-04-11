
# Scrapping - Invest News -------------------------------------------------
retorna_altas_baixas <- function(){
  # Maiores altas -----------------------------------------------------------
  altas <- rvest::read_html("https://investnews.com.br/cotacao/") %>% 
    rvest::html_elements(".div-cot-altas") %>% 
    rvest::html_text() %>% 
    str_split("\n\n") %>% 
    purrr::pluck(1) %>% 
    stringr::str_squish() %>% 
    as_tibble() %>% 
    dplyr::filter(value != "") %>% 
    janitor::row_to_names(1) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(maiores_altas = stringr::str_replace(string = maiores_altas, pattern = "R\\$ ", replacement = "R$")) %>% 
    tidyr::separate(col = maiores_altas, into = c("Ticker", "Retorno", "Preco"), sep = " ")
  
  
  # Maiores baixas ----------------------------------------------------------
  baixas <- rvest::read_html("https://investnews.com.br/cotacao/") %>% 
    rvest::html_elements(".div-cot-baixas") %>% 
    rvest::html_text() %>% 
    str_split("\n\n") %>% 
    purrr::pluck(1) %>% 
    stringr::str_squish() %>% 
    as_tibble() %>% 
    dplyr::filter(value != "") %>% 
    janitor::row_to_names(1) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(maiores_baixas = stringr::str_replace(string = maiores_baixas, pattern = "R\\$ ", replacement = "R$")) %>% 
    tidyr::separate(col = maiores_baixas, into = c("Ticker", "Retorno", "Preco"), sep = " ")
  
  
  lista_dfs <- list(altas, baixas)
  
  
  # Renomear colunas --------------------------------------------------------
  renomear_colunas <- c("Maiores Altas", "Maiores Baixas")
  
  for (i in 1:2){
    names(lista_dfs)[i] <- renomear_colunas[i]
  }
  
  # Rertorna uma lista de dataframes. Sendo o primeiro maiores altas e o segundo maiores baixas --------------------------------------------------------------
  return(lista_dfs)
  
}

retorna_altas_baixas()
