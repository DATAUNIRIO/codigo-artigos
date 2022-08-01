

library(tidytext)
library(stringr)
library(tidyr)
library(dplyr)

#--------------------------------------------------------------
#Top words
#--------------------------------------------------------------
#To extract the words from the text of each tweet we need to use several functions from the tidytext package. First we remove ampersand, greater-than and less-than characters, URLs and emoji from the text, then we tokenise the text into a row per word format, filter out stop words such as “the”, “of”, and “to”, remove any numbers and filter out hashtags and mentions of usernames. Then we select the variables of interest, count the frequency of each word and sort in descending order.

words <- paola %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  #unnest_tokens(word, text, token = "tweets") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

# tem que fazer a limpeza de dados antes
head(words)
#Then we use the wordcloud package to create a visualisation of the word frequencies.

library(wordcloud) 
words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#F29545"))
library(ggplot2)
#ggsave('tweets_do_servidor_12_03_2021/resultados/wordcloud_29_03_2021_v2.png', width = 10, height = 6)

