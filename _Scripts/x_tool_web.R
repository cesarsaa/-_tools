## ------------------------------------------------------- #
# by: Carlos Gonzalez & Cesar Saavedra
# Ciat 
# Dic-2023
## ------------------------------------------------------- #
# R-Options
g = gc;rm(list=ls())
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr,tidyr,tidyverse,purrr,readxl,
                                knitr,plyr,sna,network,igraph,ggraph,
                                rvest,xml2,tidytext,stopwords,stringr,
                                broom,httr,GET,forcats))

## ------------------------------------------------------- #
root <- "D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/"
sheets <- readxl::excel_sheets(paste0(root,"_Inputs/WEB (Trial 1).xlsx"))
## ------------------------------------------------------- #
web_scrapp <- function(sheets) {
  tryCatch({
    # 
    x_files <- readxl::read_excel(paste0(root,"_Inputs/WEB (Trial 1).xlsx"), sheet = sheets)
    #
    nodes <- unique(x_files$Link)
    profile_web<- list()
    profile_bigram<- list()
    ## ------------------------------------------------------- #
    for( i in 1:length(nodes)){
      #
      cat(paste("inicia nodo: ", i, "\n", sheets, "\n", nodes[i],"\n",sep=""))
      categoria <- x_files %>% filter(Link==nodes[i]) %>% pull(Cat)
      #= -------------------------------------------------------
      web <- x_files %>% filter(Link==nodes[i])
      HtmlLink <- web$Link
      ASIN <- stringr::str_match(HtmlLink, "/dp/([A-Za-z0-9]+)/")[,2]
      review_title <- HtmlLink %>% httr::GET(., timeout(10000)) %>%
        xml2::read_html() %>% rvest::html_children() %>% rvest::html_text()
      #= -------------------------------------------------------
      source("D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/_Inputs/outwords.R",encoding="utf-8")
      fuera_palabras_english <- stopwords("english")
      stopwords_en <- stopwords("SMART")
      #= -------------------------------------------------------
      dn <- tibble::tibble(text=review_title)
      db <- dn %>% unnest_tokens(word, text)
      #= -------------------------------------------------------
      db <- as.data.frame(db)
      db_bigrams <- as.data.frame(dn)
      #= -------------------------------------------------------
      db_bigrams$cat <- categoria
      db$cat <- categoria
      #= -------------------------------------------------------
      db_bigrams <- db_bigrams %>%
        tidytext::unnest_tokens(bigram, text, token= "ngrams", n=2) %>%
        filter(!is.na(bigram))
      #= -------------------------------------------------------
      bigrams_separated <- db_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      #= -------------------------------------------------------
      out <- db[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", db$word),]
      out <- unique(out)
      #= -------------------------------------------------------
      cortas <- db[nchar(db$word)<3,] 
      largas <- db[nchar(db$word)>15,] 
      db <- db %>% filter(!word %in% cortas) %>% filter(!word %in% largas)
      #= -------------------------------------------------------
      db <- db  %>% dplyr::filter(!word %in% out ) %>%
        dplyr::filter(!word %in% fuera_palabras_english) %>%
        dplyr::filter(!word %in% out_words) %>% dplyr::filter(!word %in% stopwords_en)
      #= -------------------------------------------------------
      bigrams_filtered <- bigrams_separated %>%
        dplyr::filter(!word1 %in% stop_words$word) %>%
        dplyr::filter(!word2 %in% stop_words$word) %>%
        dplyr::filter(!word1 %in% stopwords_en) %>% 
        dplyr::filter(!word2 %in% stopwords_en) %>% 
        dplyr::filter(!word1 %in% out_words) %>%
        dplyr::filter(!word2 %in% out_words) %>%
        dplyr::filter(!word1 %in% out) %>%
        dplyr::filter(!word2 %in% out) %>%
        dplyr::filter(!word1 %in% cortas) %>%
        dplyr::filter(!word2 %in% cortas) %>%
        dplyr::filter(!word1 %in% largas) %>%
        dplyr::filter(!word2 %in% largas) %>%
        dplyr::filter(!word1 %in%  .$word1[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", .$word1)]) %>%
        dplyr::filter(!word2 %in%  .$word2[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", .$word2)]) %>%
        dplyr::filter(!word1 %in%  .[nchar(.$word1)<3,]) %>%
        dplyr::filter(!word1 %in%  .[nchar(.$word1)>14,]) %>%
        dplyr::filter(!word2 %in%  .[nchar(.$word2)<3,]) %>%
        dplyr::filter(!word2 %in%  .[nchar(.$word2)>13,])
      #= -------------------------------------------------------
      profile_web[[i]] <- db
      bigrams_filtered$cat<- categoria
      profile_bigram[[i]]<- bigrams_filtered
      cat(paste("termina nodo:","\n",nodes[i],"\n",sep=""))
    }
    #= -------------------------------------------------------
    #
    web_data<- do.call(rbind,profile_web)
    web_iki_bigram<- do.call(rbind,profile_bigram)
    #
    web_data <- web_data %>% 
      dplyr::group_by(., word, cat) %>% 
      dplyr::count(word, sort = TRUE)
    web_data$date <- Sys.Date()
    
    web_iki_bigram <- web_iki_bigram %>% 
      dplyr::group_by(cat) %>% 
      dplyr::count(word1, word2, sort = TRUE)
    web_iki_bigram$date <- Sys.Date()
    #
    write.csv(web_data, paste0("D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/_Results/webscrapping_",sheets,"_",Sys.Date(),".csv"))
    write.csv(web_iki_bigram, paste0("D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/_Results/bigram_web_",sheets,"_",Sys.Date(),".csv"),quote = T)
    
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    # Handle the error as per your requirement
  })
}
## ------------------------------------------------------- #
1:length(sheets) %>% purrr::map(.f=function(i){
  web_scrapp(sheets = sheets[i])
})
## ------------------------------------------------------- #