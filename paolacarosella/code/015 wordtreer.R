
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


previsao_paola <- bigrama %>%
  filter(grepl("^paola$", word1)) %>%
  arrange(desc(n)) %>%
  top_n(15)

paola_wordtree <- paste0(previsao_paola$word1," ",previsao_paola$word2)
paola_wordtree <- paola_wordtree[paola_wordtree!='paola paola'] 

library(wordtreer)

wordtree(text=paola_wordtree,targetWord = "paola",direction="suffix",Number_words = 10,fileName="word_tree_paola.html")
browseURL("word_tree_paola.html")

#wordtree(text=paola_wordtree,targetWord = "paola",direction="double",Number_words=3,fileName="thingie.html")
#browseURL("thingie.html")
