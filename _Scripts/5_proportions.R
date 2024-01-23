# Proporciones
g = gc;rm(list=ls())
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
#
root <- "D:/OneDrive - CGIAR/Documents/GitHub/XX_tool/"
#  
environment <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 1)  %>% mutate(word = tolower(WORD)) %>% pull()
climate <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 2)  %>% mutate(word = tolower(WORD)) %>% pull()
food_system <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 3)  %>% mutate(word = tolower(WORD)) %>% pull()
communities <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 4)  %>% mutate(word = tolower(WORD)) %>% pull()
region <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 5)  %>% mutate(word = tolower(WORD)) %>% pull()
education <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 6)  %>% mutate(word = tolower(WORD)) %>% pull()
research <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 7)  %>% mutate(word = tolower(WORD)) %>% pull()
economy <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 8) 
gender <- readxl::read_excel(paste0(root, "_Inputs/Glossary_english.xlsx"),sheet = 9) %>% mutate(word = tolower(WORD)) %>% pull()


iki<- read.csv(paste0(root,"_Results/scrapping.csv"))
iki$X<- NULL
iki
iki <- iki %>% mutate(word_cat =case_when(word %in% environment ~ "environment",
                                  word %in% climate ~ "climate",
                                  word %in% food_system ~ "food_system",
                                  word %in% communities ~ "communities",
                                  word %in% region ~ "region",
                                  word %in% education ~ "education",
                                  word %in% research ~ "research",
                                  word %in% economy ~ "economy",
                                  word %in% gender ~ "gender",
                                  TRUE ~ "others"))
head(iki)

iki$date <- lubridate::as_date(iki$date)

df <- iki %>% dplyr::group_by(word_cat,date,institution) %>% 
  # dplyr::mutate(Por = prop.table(n)*100)
  dplyr::summarize(total=sum(n)) %>% filter(word_cat!="others")

total <- df %>% dplyr::group_by(., institution,date) %>% 
  dplyr::mutate(Por = prop.table(total))
  dplyr::mutate(all=sum(total), prop=(total/all)*100) 

total$category <- NULL

total
write.csv(total,paste0(root,"_Results/propor_concordancia2.csv"))

df2 <- iki %>% group_by(word,word_cat,institution,date) %>% 
  dplyr::summarize(total=sum(n)) %>% filter(word_cat!="others")

write.csv(df2,paste0(root,"_Results/word_category.csv"))


 