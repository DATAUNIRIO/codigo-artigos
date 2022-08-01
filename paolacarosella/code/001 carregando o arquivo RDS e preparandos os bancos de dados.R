library(dplyr)

banco_de_dados_paola <- readRDS('dados/banco_de_dados_paola_27_07_2022.Rds')
paolacarosella_oficial <- readRDS('dados/paolacarosella_oficial_27_07_2022.Rds')
#dim(banco_de_dados_paola)
#[1] 33128    91

paola <- banco_de_dados_paola %>% filter(palavra=="paola")
banco_de_dados_paola <- banco_de_dados_paola %>% filter(palavra!="paola")

#paola$text <- tolower(paola$text)

paola$carosella <- ifelse(grepl("carosella", paola$text, ignore.case = T), "carosella", "Nao")
table(paola$carosella)
paola <- paola %>% filter(carosella=="carosella")
table(paola$carosella)
paola <- paola %>% select(-carosella)


#dim(banco_de_dados_paola)
#[1] 12213    91

banco_de_dados_paola <- banco_de_dados_paola %>% add_row(paola)
#dim(banco_de_dados_paola)
remove(paola)

table(banco_de_dados_paola$palavra)


saveRDS(banco_de_dados_paola,file='dados_trabalhados/paola_vf_27_07_2022.Rds')
saveRDS(paolacarosella_oficial,file='dados_trabalhados/paolacarosella_oficial_vf.Rds')