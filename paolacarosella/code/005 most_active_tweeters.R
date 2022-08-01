# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')

table(paola$palavra)


library(tidytext)
library(stringr)
library(tidyr)
library(dplyr)

#---------------------------------------------------------
#Top tweeters
#---------------------------------------------------------
#To identify the most active tweeters we can use the 
#“screen_name” variable to tot up the number of tweets 
#by Twitter handle. We can then add back the @ symbol 
#using the paste0() function.

top_ten<-paola %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))

# Por palavra
top_ten_palavra<-paola %>% 
  group_by(palavra) %>%
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))

top_ten_palavra <-top_ten_palavra %>% arrange(palavra)
top_ten_palavra <-top_ten_palavra %>% filter(palavra!='paola')

#top_ten_wide<-top_ten_palavra %>%  pivot_wider(names_from = palavra, values_from = n)
#remove(top_ten_wide)


#Principais tweeters
#------------------------------------------------- --------

tabela_most_active_tweeters <-top_ten_palavra %>% gt() %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - Identificação das pessoas mais ativas do Twitter relacionadas com a **Paola Carosella**"),
    subtitle = md("As 10 pessoas mais ativas do Twitter relacionadas com *paola_lacra_lucra* e *paola_voltapraargentina*")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    palavra = "Palavra Associada",
    screen_name = "Nome",
    n = "Quantidade"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) 


tabela_most_active_tweeters

gtsave(
  data = tabela_most_active_tweeters,
  filename = paste0(getwd(),"/img/tabela_most_active_tweeters.png"))

remove(tabela_oficial,emoji_oficial)









#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# PAOLA OFICIAL (NAO FUNCIONOU)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

top_ten_oficial <-paola_oficial %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))