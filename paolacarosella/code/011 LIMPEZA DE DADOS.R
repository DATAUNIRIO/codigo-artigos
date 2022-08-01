

# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
#paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

table(paola$palavra)

#----------------------------------------------------------------
# Retirar os caracteres especiais
#----------------------------------------------------------------

paola$text<-chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
                   "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc",paola$text)

paola_oficial$text<-chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
                   "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc",paola$paola_oficial)
head(paola$text)


#----------------------------------------------------------------
# Limpeza do banco de dados
#----------------------------------------------------------------

library(textclean)
paola$text <-replace_url(paola$text)      # remove todas as urls do banco de dados 
paola$text<-replace_html(paola$text)     # remove todos os possiveis codigos na linguagem HTML
paola$text<-replace_tag(paola$text)      # remove todas as tags do banco de dados

head(paola$text)

paola$text<-tolower(paola$text) 	       # trasnforma todas as letras em minúsculo
paola$text<- gsub("\\!","", paola$text)
paola$text<- gsub("\\?","", paola$text)
paola$text<- gsub("\n","", paola$text)
paola$text<- gsub("\\#","", paola$text)
#paola$text<- replace_hash(paola$text)

head(paola$text)


#------------------------------------------------------------------------------
# Não tem nenhum post da PaolaCarosella Oficial na base de dados paola
#------------------------------------------------------------------------------
paola %>% filter(screen_name=='PaolaCarosella') 
paola %>% filter(user_id=='135743827')
paola[paola$user_id==135743827,]
paola[paola$user_id=='135743827',]
paola[paola$screen_name=='PaolaCarosella',]


#------------------------------------------------------------------------------
paola$marcoupaola<-ifelse(grepl("@PaolaCarosella", paola$text, ignore.case = T), "Sim","Nao")
table(paola$marcoupaola)
library(dplyr)
paola %>% filter(marcoupaola=="Sim") %>% select(text) %>% head(10)

#----------------------------------------------------------------
# eliminando os tweet pequenos demais
#----------------------------------------------------------------
pequeno<- paola %>% filter(nchar(text)<10)
pequeno %>% pull(text) %>% head(30)
pequeno2<- paola %>% filter(nchar(text)<15)
pequeno2 %>% pull(text)
remove(pequeno,pequeno2)

dim(paola)
paola<- paola %>% filter(nchar(text)>9)
dim(paola)
#----------------------------------------------------------------
# olhando o autor do tweet e bots
#----------------------------------------------------------------
autor_do_tweet<-paola %>% pull(screen_name) %>% table()
autor_do_tweet<-data.frame(autor_do_tweet)
autor_do_tweet<- autor_do_tweet %>% arrange(desc(Freq))
summary(autor_do_tweet$Freq)
head(autor_do_tweet)

possivel_robo<-autor_do_tweet[autor_do_tweet$Freq>100,]

autor_do_tweet<-autor_do_tweet %>% rename(autor_do_tweet, autor = .)
possivel_robo <-possivel_robo %>% rename(possivel_robo, autor = .)
names(autor_do_tweet)

possivel_robo
paola %>% filter(screen_name=='Lulliseteseis') %>% select(text)
robo <- paola %>% filter(screen_name=='joelmor67738760')  %>% select(text) %>% head(100)
robo2 <- paola %>% filter(screen_name=='Henry_Mayer')  %>% select(text) %>% head(100)
paola %>% filter(screen_name=='carloslanes')  %>% select(text) %>% head(100)
paola %>% filter(screen_name=='net0_paul01O')  %>% select(text)
paola %>% filter(screen_name=='martin_lutherj')  %>% select(text)

#----------------------------------------------------------------
# removendo robots
#----------------------------------------------------------------
banidos <- c('joelmor67738760','Henry_Mayer')

dim(paola)
`%!in%` = Negate(`%in%`)

paola<-paola %>% filter(paola$screen_name %!in% banidos)
dim(paola)

#----------------------------------------------------------------
# antes de remover fazer uma analise do EMOJI
#----------------------------------------------------------------
#----------------------------------------------------------------
library(dplyr)
paola %>% pull(text) %>% head(15)
limpeza_de_dados<-paola %>% pull(text) %>% head(50)
limpeza_de_dados<-data.frame(limpeza_de_dados)
colnames(limpeza_de_dados)<-'original'

paola$text<-emo::ji_replace_all(paola$text," ")
paola %>% pull(text) %>% head(15)
limpeza_de_dados$sem_emoji<-paola %>% pull(text) %>% head(50)

library(textclean)
paola$text <- gsub("'", "", paola$text)  # remove apostrophes
paola$text <- gsub("[[:punct:]]", " ", paola$text)  # replace punctuation with space
paola$text <- gsub("[[:cntrl:]]", " ", paola$text)  # replace control characters with space
paola$text <- gsub("^[[:space:]]+", "", paola$text) # remove whitespace at beginning of documents
paola$text <- gsub("[[:space:]]+$", "", paola$text) # remove whitespace at end of documents
paola$text <- gsub("\\:","", paola$text) # remove todos os dois pontos do banco de dados
paola$text <- gsub("\\#","", paola$text) # remove todos os hashtags do banco de dados
paola$text <- gsub("kkkk","", paola$text) # remove todos os kkkk do banco de dados
paola$text <- gsub("\\!","", paola$text)
paola$text <- gsub("\\?","", paola$text)
paola$text <- gsub("\\.","", paola$text)

limpeza_de_dados$sem_url_e_sem_outros_simbolos<-paola %>% pull(text) %>% head(50)
writexl::write_xlsx(limpeza_de_dados,path = 'img/limpeza/exemplo_efeito_do_tratamento_50_twitter.xlsx')
remove(limpeza_de_dados)

#----------------------------------------------------------------
#
# LEMMA e STEM
#
#----------------------------------------------------------------
#remotes::install_github("DATAUNIRIO/lemmar")
#library(lemmar)
#paola$text_lemma<-lemmar::lemmatize_pt(paola$text)
#problemas identificados
#head(paola$text_lemma,50)
# bedelho = bedelhar ?

remove(autor_do_tweet,possivel_robo,robo,robo2,banidos)

#------------------------------------------------------------------------------
max(paola$created_at)
min(paola$created_at)

#------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
# limpeza e melhoria do texto paola lacra y lucra
#----------------------------------------------------------------------------------------
paola$text<-gsub("lacra y lucra","paola_lacra_y_lucra", paola$text, ignore.case = T)
paola$text<-gsub("lacra lucra","paola_lacra_y_lucra", paola$text, ignore.case = T)
paola$text<-gsub("burros","burro", paola$text, ignore.case = T)
paola$text<-gsub("vc","voce", paola$text, ignore.case = T)
paola$text<-gsub("escrotos","escroto", paola$text, ignore.case = T)

saveRDS(paola,'dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')

