

library(dplyr)
options(stringsAsFactors = FALSE)
#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

table(paola$palavra)


#----------------------------------------------------------------
#Top emoji
#To identify the most frequently used emoji we can use the ji_extract_all() function from the emo package. This function extracts all the emojis from the text of each tweet. We can then use the unnest() function from the tidyr package to split out the emojis, count, sort in descending order and identify the top 10.
#----------------------------------------------------------------

library(emo)
library(tidytext)
library(tidyr)
library(dplyr)
library(reactable)
library(gt)

emoji<-paola %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10) 

tabela <-emoji %>% gt() %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - 10 EMOJIs"),
    subtitle = md("mais utilizados com as hashtags relacionadas à **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    emoji = "Emoji",
    n = "Quantidade"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) 


tabela

gtsave(
  data = tabela,
  filename = paste0(getwd(),"/img/tabela_emoji.png"))

remove(tabela,emoji)

#----------------------------------------------------------------
#Top emoji por palavra
#----------------------------------------------------------------

emoji_por_palavra<-paola %>%
  group_by(palavra) %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10)


tabela_por_palavra <- emoji_por_palavra %>%  filter(palavra!='paola') %>%  group_by(palavra) %>%
  gt() %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 2 - 10 EMOJIs"),
    subtitle = md("mais utilizados com as hashtags relacionadas à **paola_lacra_lucra** e **paola_voltapraargentina**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    emoji = "Emoji",
    n = "Quantidade"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) 


tabela_por_palavra

gtsave(
  data = tabela_por_palavra,
  filename = paste0(getwd(),"/img/tabela_emoji_por_palavra.png"))

remove(tabela_por_palavra,emoji_por_palavra)



#----------------------------------------------------------------
#Top emoji por palavra
#----------------------------------------------------------------

library(emojifont)
library(ggplot2)        
library(dplyr)
library(forcats)

emoji<-paola %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(15) 

g1_emoji<-emoji %>% #filter(n>4000) %>%
  arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(emoji, n, .desc = FALSE), n,colour =n,fill=n)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = emoji$emoji, labels = emoji$emoji) +
  theme( axis.text.y =element_text( size=30 ) ) +
  coord_flip()

g1_emoji+ scale_fill_continuous(type = "viridis")
g1_emoji + scale_fill_gradient(  low = "orange",high = "red")+
  scale_colour_gradient(  low = "orange",high = "red")+
  labs(title = "Quinze Emojis mais utilizados",subtitle = "no twitter sobre mobilidade urbana",caption ="Fonte:Giel/DATAUNIRIO" ,x="Emoji",y='')

g1_emoji + scale_fill_viridis_c()


remove(g1_emoji,emoji)
#------------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------

# PAOLA OFICIAL

paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

emoji_oficial<-paola_oficial %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10) 

tabela_oficial <-emoji_oficial %>% gt() %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - 10 EMOJIs"),
    subtitle = md("mais utilizados pela conta oficial da **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    emoji = "Emoji",
    n = "Quantidade"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) 


tabela_oficial

gtsave(
  data = tabela_oficial,
  filename = paste0(getwd(),"/img/tabela_emoji_oficial_paola.png"))

remove(tabela_oficial,emoji_oficial)


























