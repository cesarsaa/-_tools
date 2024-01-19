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
                                rvest,xml2,XML,tidytext,stopwords,stringr,
                                broom,httr,GET,forcats,pdftools))

## ------------------------------------------------------- #
root <- "D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/"
sheets <- readxl::excel_sheets(paste0(root,"_Inputs/PDF.xlsx")); sheets
## ------------------------------------------------------- #
PDF_scrapp <- function(sheets){
  tryCatch({
    ## ------------------------------------------------------- #
    webs <- readxl::read_excel(paste0(root,"_Inputs/PDF.xlsx"), sheet = sheets)
    links <- unique(webs$Link)
    #
    profile <- list()
    profile_bigram <- list()
    ## ------------------------------------------------------- #
    for(i in 1:length(links)){
      #
      cat(paste("inicia nodo ", i, ": ", links[i],"\n",sep=" ", "\n"))
      #  
      df <- webs %>% dplyr::filter(Link %in% links[i])
      #  
      r_html <-  rvest::read_html(df$Link)
      pdfs <- r_html %>% rvest::html_elements("a") %>% html_attr("href")
      # 
      pdfs <- pdfs[grep(".pdf",pdfs)]
      pth_pdf <- pdfs[!is.na(pdfs)]
      # 
      pdf_data <- lapply(pth_pdf, pdftools::pdf_text)
      pdf_data <- gsub("\n","_",pdf_data)
      pdf_data <- gsub("\n","_",pdf_data)
      # palabras a eliminar y stopwords
      out_words <- source(paste0(root,"_Inputs/outwords_2.R"),encoding="utf-8")
      stopwords_esp <- stopwords::stopwords("spanish")
      stopwords_eng <- stopwords::stopwords("english")
      stopwords_smart <- stopwords::stopwords("SMART")
      dn <- tibble::tibble(text=pdf_data) %>% 
        dplyr::mutate(cat=unique(df$Cat)) %>%
        dplyr::select(cat,text)
      
      dn$text<- as.character(dn$text)
      #
      db_bigrams <- as.data.frame(dn)
      db <- dn %>% tidytext::unnest_tokens(word, text)
      db <- as.data.frame(db)
      #
      db_bigrams <- db_bigrams %>%
        tidytext::unnest_tokens(bigram, text, token= "ngrams", n=2) %>%
        filter(!is.na(bigram))
      db_bigrams <- db_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      #
      out<- db[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", db$word),]
      out  <- unique(out)
      #
      p_cortas <- db[nchar(db$word)<=4,] 
      p_largas <- db[nchar(db$word)>=15,]
      #
      db <- db  %>% dplyr::filter(!word %in% out ) %>%
        dplyr::filter(!word %in% out_words$value) %>% 
        dplyr::filter(!word %in% stopwords_esp) %>% 
        dplyr::filter(!word %in% stopwords_eng) %>%
        dplyr::filter(!word %in% stopwords_smart) %>% 
        dplyr::filter(!word %in% p_cortas) %>% 
        dplyr::filter(!word %in% p_largas) %>% 
        filter(!word %in% c(.$word[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", .$word)]))
      #
      db_bigrams <- db_bigrams  %>%
        dplyr::filter(!word1 %in% out ) %>%
        dplyr::filter(!word2 %in% out ) %>%
        dplyr::filter(!word1 %in% out_words$value) %>%
        dplyr::filter(!word2 %in% out_words$value) %>%
        dplyr::filter(!word1 %in% stopwords_esp) %>%
        dplyr::filter(!word2 %in% stopwords_esp) %>%
        dplyr::filter(!word1 %in% stopwords_eng) %>%
        dplyr::filter(!word2 %in% stopwords_eng) %>%
        dplyr::filter(!word1 %in% stopwords_smart) %>%
        dplyr::filter(!word2 %in% stopwords_smart) %>%
        dplyr::filter(!word1 %in% p_cortas) %>%
        dplyr::filter(!word2 %in% p_cortas) %>%
        dplyr::filter(!word1 %in% p_largas) %>%
        dplyr::filter(!word2 %in% p_largas) %>%
        filter(!word1 %in% c(.$word1[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", .$word1)])) %>%
        filter(!word2 %in% c(.$word2[grep(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", .$word2)]))
      #
      db <- db %>% mutate(word=case_when(word %in% .$word[grep("ngender",.$word)]~ "gender",
                                         word %in% .$word[grep("nwhile",.$word)]~ "while",
                                         word %in% .$word[grep("nquestion",.$word)]~ "question",
                                         word %in% .$word[grep("nclimate",.$word)]~ "climate",
                                         word %in% .$word[grep("nkenya",.$word)]~ "kenya",
                                         word %in% .$word[grep("ninternational",.$word)]~ "international",
                                         word %in% .$word[grep("nenvironment",.$word)]~ "environment",
                                         word %in% .$word[grep("naccordance",.$word)]~ "accordance",
                                         word %in% .$word[grep("nfunding",.$word)]~ "funding",
                                         word %in% .$word[grep("npurpose",.$word)]~ "purpose",
                                         word %in% .$word[grep("neur",.$word)]~ "eur",
                                         word %in% .$word[grep("ntailored",.$word)]~ "tailored",
                                         word %in% .$word[grep("nsecond",.$word)]~ "second",
                                         word %in% .$word[grep("nserious",.$word)]~ "serious",
                                         word %in% .$word[grep("nstrengthened",.$word)]~ "strengthened",
                                         word %in% .$word[grep("nsubmissions",.$word)]~ "submissions",
                                         word %in% .$word[grep("ngrants",.$word)]~ "grants",
                                         word %in% .$word[grep("nstrengthening",.$word)]~ "strengthening",
                                         word %in% .$word[grep("nachieving",.$word)]~ "achieving",
                                         word %in% .$word[grep("napply",.$word)]~ "apply",
                                         word %in% .$word[grep("nbeing",.$word)]~ "being",
                                         word %in% .$word[grep("ncapabilities",.$word)]~ "capabilities",
                                         word %in% .$word[grep("nglobal",.$word)]~ "global",
                                         word %in% .$word[grep("nidentified",.$word)]~ "identified",
                                         word %in% .$word[grep("nneed",.$word)]~ "need",
                                         word %in% .$word[grep("ncooperatives",.$word)]~ "cooperatives",
                                         word %in% .$word[grep("nfigure",.$word)]~ "figure",
                                         TRUE ~ word))
      #
      db_bigrams <- db_bigrams %>%
        mutate(word1=case_when(word1 %in% .$word1[grep("ngender",.$word1)]~ "gender",
                               word1 %in% .$word1[grep("nwhile",.$word1)]~ "while",
                               word1 %in% .$word1[grep("nquestion",.$word1)]~ "question",
                               word1 %in% .$word1[grep("nclimate",.$word1)]~ "climate",
                               word1 %in% .$word1[grep("nkenya",.$word1)]~ "kenya",
                               word1 %in% .$word1[grep("ninternational",.$word1)]~ "international",
                               word1 %in% .$word1[grep("nenvironment",.$word1)]~ "environment",
                               word1 %in% .$word1[grep("naccordance",.$word1)]~ "accordance",
                               word1 %in% .$word1[grep("nfunding",.$word1)]~ "funding",
                               word1 %in% .$word1[grep("npurpose",.$word1)]~ "purpose",
                               word1 %in% .$word1[grep("neur",.$word1)]~ "eur",
                               word1 %in% .$word1[grep("ntailored",.$word1)]~ "tailored",
                               word1 %in% .$word1[grep("nsecond",.$word1)]~ "second",
                               word1 %in% .$word1[grep("nserious",.$word1)]~ "serious",
                               word1 %in% .$word1[grep("nstrengthened",.$word1)]~ "strengthened",
                               word1 %in% .$word1[grep("nsubmissions",.$word1)]~ "submissions",
                               word1 %in% .$word1[grep("ngrants",.$word1)]~ "grants",
                               word1 %in% .$word1[grep("nstrengthening",.$word1)]~ "strengthening",
                               word1 %in% .$word1[grep("nachieving",.$word1)]~ "achieving",
                               word1 %in% .$word1[grep("napply",.$word1)]~ "apply",
                               word1 %in% .$word1[grep("nbeing",.$word1)]~ "being",
                               word1 %in% .$word1[grep("ncapabilities",.$word1)]~ "capabilities",
                               word1 %in% .$word1[grep("nglobal",.$word1)]~ "global",
                               word1 %in% .$word1[grep("nidentified",.$word1)]~ "identified",
                               word1 %in% .$word1[grep("nneed",.$word1)]~ "need",
                               word1 %in% .$word1[grep("nplans",.$word1)]~ "plans",
                               word1 %in% .$word1[grep("nprojects",.$word1)]~ "projects",
                               word1 %in% .$word1[grep("nexclusively",.$word1)]~ "exclusively",
                               word1 %in% .$word1[grep("nsustainable",.$word1)]~ "sustainable",
                               word1 %in% .$word1[grep("nsociety",.$word1)]~ "society",
                               word1 %in% .$word1[grep("ncooperatives",.$word1)]~ "cooperatives",
                               TRUE~ word1)) %>%
        mutate(word2=case_when(word2 %in% .$word2[grep("ngender",.$word2)]~ "gender",
                               word2 %in% .$word2[grep("nwhile",.$word2)]~ "while",
                               word2 %in% .$word2[grep("nquestion",.$word2)]~ "question",
                               word2 %in% .$word2[grep("nclimate",.$word2)]~ "climate",
                               word2 %in% .$word2[grep("nkenya",.$word2)]~ "kenya",
                               word2 %in% .$word2[grep("ninternational",.$word2)]~ "international",
                               word2 %in% .$word2[grep("nenvironment",.$word2)]~ "environment",
                               word2 %in% .$word2[grep("naccordance",.$word2)]~ "accordance",
                               word2 %in% .$word2[grep("nfunding",.$word2)]~ "funding",
                               word2 %in% .$word2[grep("npurpose",.$word2)]~ "purpose",
                               word2 %in% .$word2[grep("neur",.$word2)]~ "eur",
                               word2 %in% .$word2[grep("ntailored",.$word2)]~ "tailored",
                               word2 %in% .$word2[grep("nsecond",.$word2)]~ "second",
                               word2 %in% .$word2[grep("nserious",.$word2)]~ "serious",
                               word2 %in% .$word2[grep("nstrengthened",.$word2)]~ "strengthened",
                               word2 %in% .$word2[grep("nsubmissions",.$word2)]~ "submissions",
                               word2 %in% .$word2[grep("ngrants",.$word2)]~ "grants",
                               word2 %in% .$word2[grep("nstrengthening",.$word2)]~ "strengthening",
                               word2 %in% .$word2[grep("nachieving",.$word2)]~ "achieving",
                               word2 %in% .$word2[grep("napply",.$word2)]~ "apply",
                               word2 %in% .$word2[grep("nbeing",.$word2)]~ "being",
                               word2 %in% .$word2[grep("ncapabilities",.$word2)]~ "capabilities",
                               word2 %in% .$word2[grep("nglobal",.$word2)]~ "global",
                               word2 %in% .$word2[grep("nidentified",.$word2)]~ "identified",
                               word2 %in% .$word2[grep("nneed",.$word2)]~ "need",
                               word2 %in% .$word2[grep("nplans",.$word2)]~ "plans",
                               word2 %in% .$word2[grep("nprojects",.$word2)]~ "projects",
                               word2 %in% .$word2[grep("nexclusively",.$word2)]~ "exclusively",
                               word2 %in% .$word2[grep("nsustainable",.$word2)]~ "sustainable",
                               word2 %in% .$word2[grep("nsociety",.$word2)]~ "society",
                               word2 %in% .$word2[grep("ncooperatives",.$word2)]~ "cooperatives",
                               TRUE~ word2))
      #
      profile[[i]] <- db
      profile_bigram[[i]] <- db_bigrams
      
      cat(paste("termina nodo ", i, ": ", links[i],"\n",sep=" ","\n"))
    }
    ## ------------------------------------------------------- #
    #
    PDF_scrapping <- do.call(rbind, profile)
    PDF_bigram <- do.call(rbind,profile_bigram)
    #
    PDF_scrapping <- PDF_scrapping %>% dplyr::count(cat, word, sort = TRUE)
    PDF_scrapping$date <- Sys.Date()
    PDF_scrapping$institution <- sheets
    
    PDF_bigram <- PDF_bigram %>% group_by(cat) %>%
      dplyr::count(word1, word2, sort = TRUE)
    PDF_bigram$date <- Sys.Date()
    PDF_bigram$institution <- sheets
    
    PDF_idf<- PDF_scrapping %>% 
      tidytext::bind_tf_idf(word, cat, n) %>%
      plyr::arrange(desc(tf_idf))
    PDF_idf$date <- Sys.Date()
    PDF_idf$institution <- sheets
    ## ------------------------------------------------------- #
    #
    write.csv(PDF_scrapping, paste0(root,"_Results/_PDF/PDF_scrapping_",sheets,"_",Sys.Date(),".csv"))
    write.csv(PDF_bigram, paste0(root,"_Results/_PDF/PDF_bigram_scrapping_",sheets,"_",Sys.Date(),".csv"))
    write.csv(PDF_idf, paste0(root,"_Results/_PDF/PDF_idf_scrapping_",sheets,"_",Sys.Date(),".csv"))
    
    ## ------------------------------------------------------- #
    # 
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    # Handle the error as per your requirement
  })  
  
}
## ------------------------------------------------------- #
#
1:length(sheets) %>% purrr::map(.f=function(i){
  PDF_scrapp(sheets = sheets[i])
})

## ------------------------------------------------------- #
