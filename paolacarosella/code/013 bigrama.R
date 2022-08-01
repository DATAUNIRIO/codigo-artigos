
#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola<- readRDS('dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')

table(paola$palavra)



#----------------------------------------------------------------
#                    BIGRAMA
#----------------------------------------------------------------
library(dplyr)
library(tidytext)
head(paola$text)
e<-paola$text
e<-tibble(e)
colnames(e)<-"txt"

# IMPORTANTE- tidytext nao funciona com NA (missing)
e <-na.omit(e)
#e %>% unnest_sentences(word, txt)

#frases<- e %>%
#  unnest_tokens(sentence, txt, token = "sentences")
#head(frases)


palavras_banidas<-c("e", "de", "o", "a", "com", "que", "do", "um", "para", "uma", 
                    "no", "em", "os", "da", "pelo", "ao", "mas", "nos", "na", "ser", 
                    "as", "por","tambem",":","alem","so","ate","pra",".","'",")", "(",
                    '(',' )','q','ta','ja','la')
                    
aspas<-'"'
virgula<-","
parenteses <- ")"
library(quanteda)
palavras_banidas2<-quanteda::stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,palavras_banidas2)
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
palavras_banidas<-append(palavras_banidas,parenteses)
remove(palavras_banidas2,aspas,virgula,parenteses)

palavras_banidas_DT<-tibble(palavras_banidas)
colnames(palavras_banidas_DT)<-"word"
remove(palavras_banidas)
#-----------------------------------------------------------------------
#  BIGRAMA, TRIGRAMA e PROXIMA PALAVRA (nextword)
#-----------------------------------------------------------------------
#e$txt<-gsub("muitas opcao","muita_opcao",e$txt)

# IMPORTANTE- tidytext nao funciona com NA (missing)
tidy_ngram<-e %>% unnest_tokens(ngram, txt, token = "ngrams", n = 2)

library(tidyr)
bigrama<-tidy_ngram %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, sort = TRUE)
bigrama <- na.omit(bigrama)

#write.csv2(bigrama,file="tweets_do_servidor_23_06_2021/BANCO_LIMPO/resultados/bigrama/bigrama.csv")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
library(gt)

tabela<-bigrama %>%  top_n(15) %>%
  gt()  %>% 
  cols_align(align = "left",
             columns = 1) %>%
  cols_align(align = "left",
             columns = 2) %>% 
  cols_align(align = "right",
             columns = 3) %>% 
  cols_width(
    everything() ~ px(250)
  )  %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("15 Bigramas"),
    subtitle = md("mais utilizados relacionadas à **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores")
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = everything()
    )
  )%>%
  cols_label(
    word1 = "Primeira Palavra",
    word2 = "Segunda Palavra",
    n = "Frequência")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(
    )
  )

gtsave(
  data = tabela,
  filename = "img/bigrama/tabela_bigrama.png")




# TRIGRAMA
tidy_ngram2<-e %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 3)
trigrama<-tidy_ngram2 %>%
  separate(ngram, c("word1", "word2","word3" ), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word,
         !word3 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, word3, sort = TRUE)
trigrama <- na.omit(trigrama)

library(gt)

tabela<-trigrama %>%  top_n(15) %>%
  gt()  %>% 
  cols_align(align = "left",
             columns = 1) %>%
  cols_align(align = "left",
             columns = 2) %>%
  cols_align(align = "left",
             columns = 3) %>%
  cols_align(align = "right",
             columns = 4) %>% 
  cols_width(
    everything() ~ px(250)
  )  %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("15 Trigramas"),
    subtitle = md("mais utilizados relacionadas à **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores")
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = everything()
    )
  )%>%
  cols_label(
    word1 = "Primeira Palavra",
    word2 = "Segunda Palavra",
    word3 = "Terceira Palavra",
    n = "Frequência")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(
    )
  )

gtsave(
  data = tabela,
  filename = "img/bigrama/tabela_trigrama.png")

#---------------------------------------------------------

previsao_paola <- bigrama %>%
  filter(grepl("^paola$", word1)) %>%
  arrange(desc(n)) %>%
  top_n(15)


tabela<-previsao_paola %>%  top_n(15) %>%
  gt()  %>% 
  cols_align(align = "left",
             columns = 1) %>%
  cols_align(align = "left",
             columns = 2) %>% 
  cols_align(align = "right",
             columns = 3) %>% 
  cols_width(
    everything() ~ px(250)
  )  %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Previsão da Próxima Palavra"),
    subtitle = md("relacionadas ao termo **Paola**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores")
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = everything()
    )
  )%>%
  cols_label(
    word1 = "Primeira Palavra",
    word2 = "Previsão Palavra",
    n = "Frequência")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(
    )
  )

gtsave(
  data = tabela,
  filename = "img/bigrama/tabela_previsao_palavra.png")

#------------------------------------------------------
# Visualizacao de bigramas
#------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(gutenbergr)

# https://www.tidytextmining.com/ngrams.html

bigrama %>%   filter(n > 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
ggsave("img/bigrama/grafico_bigrama.png",width = 4600, height = 2500, units = "px")
