

# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

table(paola$palavra)


# remove duplicate rows based on one column in r
# https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth

paola <- paola %>%
  arrange(text, -mentions) %>%
  filter(duplicated(text) == FALSE)


#--------------------------------------------------------------
# Top mentions
#--------------------------------------------------------------
# Here we tokenise the text of each tweet and use str_detect() from the stringr package to filter out words that start with an @ .
library(tidytext)
library(stringr)

top_mentions<-paola %>% 
  unnest_tokens(mentions_screen_name, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions_screen_name, "^@")) %>%  
  count(mentions_screen_name, sort = TRUE) %>%
  top_n(30)


#--------------------------------------------------------------
# QUEM MARCOU A PAOLA?
#--------------------------------------------------------------
keywords <-c('@PaolaCarosella')

library(purrr)
names(keywords)<-keywords
keywords<-keywords %>%
  map_dfc(~str_detect(paola$text,.x)) %>%
  add_column(paola$text,paola$screen_name,paola$status_id,paola$palavra)

names(keywords) <-c('PaolaCarosella','text','screen_name','status_id','palavra')

names(keywords) 

keywords <- keywords %>% filter(PaolaCarosella==T)
# 2567 pessoas marcaram a paola

keywords<-list(keywords)

#--------------------------------------------------------------
# QUEM PAOLA MARCOU?
#--------------------------------------------------------------
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')
top_mentions<-paola_oficial %>% 
  unnest_tokens(mentions_screen_name, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions_screen_name, "^@")) %>%  
  count(mentions_screen_name, sort = TRUE) %>%
  filter(mentions_screen_name!='PaolaCarosella') %>%
  top_n(16)

top_mentions<-top_mentions %>%  filter(mentions_screen_name!='@PaolaCarosella')

tabela_top_mentions <-
  top_mentions %>%
  gt()   %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - Pessoas que foram marcadas pela Paola Carosella"),
    subtitle = md("15 Pessoas marcadas no tweeter pela **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    mentions_screen_name = "Pessoa marcada",
    n = "Quantidade de marcações"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  )


tabela_top_mentions

gtsave(
  data = tabela_top_mentions,
  filename = paste0(getwd(),"/img/tabela_top_mentions_paola_oficial.png"))

remove(tabela_top_mentions,top_mentions)
beepr::beep(8)










#--------------------------------------------------------------
# Por palavra
#--------------------------------------------------------------
top_mentions<-paola %>%   group_by(palavra) %>%
  filter(palavra!='paola') %>%  
  unnest_tokens(mentions_screen_name, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions_screen_name, "^@")) %>%  
  count(mentions_screen_name, sort = TRUE) %>%
  top_n(10)
top_mentions <- top_mentions %>% arrange(palavra)


tabela_top_mentions <-
  top_mentions %>%
  gt()   %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - Pessoas mais marcadas"),
    subtitle = md("10 Pessoas mais marcadas no tweeter relacionadas à **paola_lacra_lucra** ou **paola_voltapraargentina**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    mentions_screen_name = "Pessoa marcada",
    n = "Quantidade de marcações"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  )   %>%
  tab_style(
    #style = cell_text(weight = "bold"),
    style = cell_fill(color = "#71797E"),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = everything()
    )
  )


tabela_top_mentions

gtsave(
  data = tabela_top_mentions,
  filename = paste0(getwd(),"/img/tabela_top_mentions_palavra.png"))

remove(tabela_top_mentions,top_mentions)
beepr::beep(8)

