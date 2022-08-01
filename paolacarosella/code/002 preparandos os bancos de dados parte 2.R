

library(dplyr)
options(stringsAsFactors = FALSE)
#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

table(paola$palavra)

paola %>% filter(palavra=='PaolaChefNotaDez') %>% select(text) %>% head() 

paola$palavra <- ifelse(paola$palavra=='PaolaChefNotaDez','paola_lacra_lucra',paola$palavra)
table(paola$palavra)

paola %>% filter(palavra=='paola_FC') %>% select(text) %>% head(50) 

paola$palavra <- ifelse(paola$palavra=='paola_FC','paola_voltapraargentina',paola$palavra)
table(paola$palavra)


'paola_lacra_lucra' = 'PaolaChefNotaDez'.'PaolaTemRazao','paola_lacra_lucra'
'paola_voltapraargentina'  = 'paola_FC','paola_voltapraargentina' 
paola$palavra <- ifelse(paola$palavra=='PaolaTemRazao','paola_lacra_lucra',paola$palavra)
table(paola$palavra)


saveRDS(paola,file='dados_trabalhados/paola_vf_27_07_2022.Rds')
