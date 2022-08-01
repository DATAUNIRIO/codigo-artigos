
# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')

table(paola$palavra)

#--------------------------------------------------------------
# LOCAL
#--------------------------------------------------------------
local<-paola %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(30)
# LOCAL por palavra
local_palavra<-paola %>% 
  group_by(palavra) %>%
  filter(!is.na(place_full_name)) %>% 
  filter(palavra!='paola') %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(100)
local_palavra<-local_palavra %>% arrange(palavra)

local<-list(local,local_palavra)
writexl::write_xlsx(local,path = paste0(getwd(),"/img/local.xlsx"))
