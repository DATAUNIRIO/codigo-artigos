# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

table(paola$palavra)

#Top hashtags
#To pull out the hashtags from the text of each tweet we first need to convert the text into a one word per row format using the unnest_tokens() function from the tidytext package. We then select only those terms that have a hashtag, count them, sort in descending order and pick the top 10.
library(stringr)
library(tidyr)
library(tidytext)
library(dplyr)
options(stringsAsFactors = FALSE)

top_hashtags<-paola %>% select(text,palavra) %>%
  filter(str_detect(text, "^#")) %>%
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(50)


#writexl::write_xlsx(top_hashtags,path = "top_hashtags.xlsx")

#rm(list=ls())
#gc(reset = TRUE)

# esta erto mas, ficou pouco analitico - acho que nao vou usar
top_hashtags_palavra <-paola %>%  filter(palavra!='paola') %>% select(text,palavra) %>%
  group_by(palavra) %>%  filter(str_detect(text, "^#")) %>% 
  count(text, sort = TRUE)  %>%
  top_n(15)
writexl::write_xlsx(top_hashtags_palavra,path = paste0(getwd(),"/img/top_hashtags_palavra.xlsx"))


top_hashtags_palavra$text <-top_hashtags_palavra$text %>% tolower()
  
top_hashtags_palavra <-top_hashtags_palavra %>%
  unnest_tokens(hashtag,text,token = "tweets", to_lower = FALSE)  %>% 
  count(hashtag, sort = TRUE)  %>%
  top_n(15)


#------------------------------------------------------------------------------
# SENTIMENTO POSITIVO
#-------------------------------------------------------------------------------
library(tibble)
library(purrr)
keywords <-c('#paolatemrazao',"#paolatemrazão","#paolacarosellatemrazao")

names(keywords)<-keywords

keywords<-keywords %>%
  map_dfc(~str_detect(paola$text,.x)) %>%
  add_column(paola$text,paola$screen_name,paola$status_id,paola$palavra)
names(keywords)<-c("paolatemrazao","paolatemrazao2","paolacarosellatemrazao",
                   "text","screen_name","status_id","palavra")


keywords<-keywords %>% filter(paolatemrazao==TRUE|paolatemrazao2==TRUE|paolacarosellatemrazao==TRUE)
table(keywords$palavra)

#devtools::install_github("gadenbuie/tweetrmd")
library(tweetrmd)
tweet_screenshot(tweet_url("etel1935", "1533197179099176962"),
   file = paste0(getwd(),"/img/paola_lacra_e_lucra/",keywords$palavra[i],'_',i,'.png'))

for(i in 1:163){
  Sys.sleep(sample(1:10,1))
  try(
    tweet_screenshot(tweet_url(keywords$screen_name[i], keywords$status_id[i]),
                     file = paste0(getwd(),"/img/paola_lacra_e_lucra/",keywords$palavra[i],'_',i,'.png'))
  )
  cat("\n \r", i, "de", 163)
}



#------------------------------------------------------------------------------
# SENTIMENTO NEGATIVO
#-------------------------------------------------------------------------------

keywords <-c("#voltaparaargentinapaolacarosella","#voltapraargentina","voltapraargentinacozinheira","quemlacranãolucra","#voltapraargentinaescrota")

names(keywords)<-keywords

keywords<-keywords %>%
  map_dfc(~str_detect(paola$text,.x)) %>%
  add_column(paola$text,paola$screen_name,paola$status_id,paola$palavra)

names(keywords)<-c("voltaparaargentinapaolacarosella","voltapraargentina","voltapraargentinacozinheira","quemlacranãolucra","voltapraargentinaescrota",
                   "text","screen_name","status_id","palavra")


keywords<-keywords %>% filter(voltaparaargentinapaolacarosella==TRUE|voltapraargentina==TRUE|voltapraargentinacozinheira==TRUE|quemlacranãolucra==TRUE|voltapraargentinaescrota==TRUE)
table(keywords$palavra)

for(i in 1:50){
  Sys.sleep(sample(1:10,1))
  try(
    tweet_screenshot(tweet_url(keywords$screen_name[i], keywords$status_id[i]),
                     file = paste0(getwd(),"/img/paola_voltapraargentina/",keywords$palavra[i],'_',i,'.png'))
  )
  cat("\n \r", i, "de", 50)
}




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# PAOLA OFICIAL
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')


library(stringr)
library(tidyr)
library(tidytext)
library(dplyr)
options(stringsAsFactors = FALSE)

top_hashtags_oficial<-paola_oficial %>% select(text) %>%
  filter(str_detect(text, "^#")) %>%
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(50)

writexl::write_xlsx(top_hashtags_oficial,path = paste0(getwd(),"/img/top_hashtags_oficial.xlsx"))