library(dplyr)

list.dirs("/home/steven/tweet-steven/")

list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")
list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[1]
list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[8]


# criando o banco
banco_de_dados_paola <-tibble::tibble(user_id=NA,status_id=NA,created_at=NA,screen_name=NA,text=NA,source=NA,display_text_width=NA, reply_to_status_id=NA,reply_to_user_id=NA, reply_to_screen_name=NA,is_quote=NA, is_retweet=NA,favorite_count=NA, retweet_count=NA,quote_count=NA, reply_count=NA,hashtags=NA, symbols=NA,urls_url=NA, urls_t.co=NA,urls_expanded_url=NA, media_url=NA,media_t.co=NA, media_expanded_url=NA,media_type=NA, ext_media_url=NA,ext_media_t.co=NA, ext_media_expanded_url=NA,ext_media_type=NA, mentions_user_id=NA,mentions_screen_name=NA, lang=NA,quoted_status_id=NA, quoted_text=NA,quoted_created_at=NA, quoted_source=NA,quoted_favorite_count=NA, quoted_retweet_count=NA,quoted_user_id=NA, quoted_screen_name=NA,quoted_name=NA, quoted_followers_count=NA, quoted_friends_count=NA, quoted_statuses_count=NA,quoted_location=NA, quoted_description=NA,quoted_verified=NA, retweet_status_id=NA,retweet_text=NA, retweet_created_at=NA,retweet_source=NA, retweet_favorite_count=NA,retweet_retweet_count=NA, retweet_user_id=NA,retweet_screen_name=NA, retweet_name=NA,retweet_followers_count=NA, retweet_friends_count=NA,retweet_statuses_count =NA, retweet_location=NA,retweet_description=NA, retweet_verified=NA,place_url=NA, place_name=NA,place_full_name=NA, place_type=NA,country=NA, country_code=NA,geo_coords=NA, coords_coords=NA,bbox_coords=NA, status_url=NA,name=NA,location=NA,description=NA, url=NA,protected=NA, followers_count=NA,friends_count=NA, listed_count=NA,statuses_count=NA, favourites_count=NA,account_created_at=NA,verified=NA,profile_url=NA, profile_expanded_url=NA,account_lang=NA, profile_banner_url=NA, profile_background_url =NA, profile_image_url=NA, palavra=NA)
bd_paola_gernerico <-tibble::tibble(user_id=NA,status_id=NA,created_at=NA,screen_name=NA,text=NA,source=NA,display_text_width=NA, reply_to_status_id=NA,reply_to_user_id=NA, reply_to_screen_name=NA,is_quote=NA, is_retweet=NA,favorite_count=NA, retweet_count=NA,quote_count=NA, reply_count=NA,hashtags=NA, symbols=NA,urls_url=NA, urls_t.co=NA,urls_expanded_url=NA, media_url=NA,media_t.co=NA, media_expanded_url=NA,media_type=NA, ext_media_url=NA,ext_media_t.co=NA, ext_media_expanded_url=NA,ext_media_type=NA, mentions_user_id=NA,mentions_screen_name=NA, lang=NA,quoted_status_id=NA, quoted_text=NA,quoted_created_at=NA, quoted_source=NA,quoted_favorite_count=NA, quoted_retweet_count=NA,quoted_user_id=NA, quoted_screen_name=NA,quoted_name=NA, quoted_followers_count=NA, quoted_friends_count=NA, quoted_statuses_count=NA,quoted_location=NA, quoted_description=NA,quoted_verified=NA, retweet_status_id=NA,retweet_text=NA, retweet_created_at=NA,retweet_source=NA, retweet_favorite_count=NA,retweet_retweet_count=NA, retweet_user_id=NA,retweet_screen_name=NA, retweet_name=NA,retweet_followers_count=NA, retweet_friends_count=NA,retweet_statuses_count =NA, retweet_location=NA,retweet_description=NA, retweet_verified=NA,place_url=NA, place_name=NA,place_full_name=NA, place_type=NA,country=NA, country_code=NA,geo_coords=NA, coords_coords=NA,bbox_coords=NA, status_url=NA,name=NA,location=NA,description=NA, url=NA,protected=NA, followers_count=NA,friends_count=NA, listed_count=NA,statuses_count=NA, favourites_count=NA,account_created_at=NA,verified=NA,profile_url=NA, profile_expanded_url=NA,account_lang=NA, profile_banner_url=NA, profile_background_url =NA, profile_image_url=NA, palavra=NA)

# carregar os dois primeiros
load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[1]))
load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[2]))

paola_voltapraargentina$palavra <- "paola_voltapraargentina"
paola_voltapraargentina2$palavra <-"paola_voltapraargentina"

banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina2)
remove(paola_voltapraargentina,paola_voltapraargentina2)

load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[6]))

paola$palavra <- "paola"                   
paola_FC$palavra <- "paola_FC"                
paola_lacra_lucra$palavra <- "paola_lacra_lucra"       
paola_voltapraargentina$palavra <- "paola_voltapraargentina" 
paola_voltapraargentina2$palavra <- "paola_voltapraargentina"
PaolaCarosellaTemRazao$palavra <- "PaolaTemRazao"
PaolaChefNotaDez$palavra <- "PaolaChefNotaDez"        
PaolaTemRazao$palavra <- "PaolaTemRazao"   

banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_FC)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_lacra_lucra)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina2)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaCarosellaTemRazao)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaChefNotaDez)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaTemRazao)

remove(paola,paola_FC,paola_lacra_lucra,paola_voltapraargentina,paola_voltapraargentina2,PaolaCarosellaTemRazao,PaolaChefNotaDez,PaolaTemRazao)

load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[7]))

paola$palavra <- "paola"                   
paola_FC$palavra <- "paola_FC"                
paola_lacra_lucra$palavra <- "paola_lacra_lucra"       
paola_voltapraargentina$palavra <- "paola_voltapraargentina" 
paola_voltapraargentina2$palavra <- "paola_voltapraargentina"
PaolaCarosellaTemRazao$palavra <- "PaolaTemRazao"
PaolaChefNotaDez$palavra <- "PaolaChefNotaDez"        
PaolaTemRazao$palavra <- "PaolaTemRazao"   

banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_FC)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_lacra_lucra)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina2)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaCarosellaTemRazao)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaChefNotaDez)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaTemRazao)
remove(paola,paola_FC,paola_lacra_lucra,paola_voltapraargentina,paola_voltapraargentina2,PaolaCarosellaTemRazao,PaolaChefNotaDez,PaolaTemRazao)

load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[8]))

paola$palavra <- "paola"                   
paola_FC$palavra <- "paola_FC"                
paola_lacra_lucra$palavra <- "paola_lacra_lucra"       
paola_voltapraargentina$palavra <- "paola_voltapraargentina" 
paola_voltapraargentina2$palavra <- "paola_voltapraargentina"
PaolaCarosellaTemRazao$palavra <- "PaolaTemRazao"
PaolaChefNotaDez$palavra <- "PaolaChefNotaDez"        
PaolaTemRazao$palavra <- "PaolaTemRazao"   

banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_FC)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_lacra_lucra)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina2)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaCarosellaTemRazao)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaChefNotaDez)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaTemRazao)
remove(paola,paola_FC,paola_lacra_lucra,paola_voltapraargentina,paola_voltapraargentina2,PaolaCarosellaTemRazao,PaolaChefNotaDez,PaolaTemRazao)

load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[5]))

paola$palavra <- "paola"                   
paola_FC$palavra <- "paola_FC"                
paola_lacra_lucra$palavra <- "paola_lacra_lucra"       
paola_voltapraargentina$palavra <- "paola_voltapraargentina" 
paola_voltapraargentina2$palavra <- "paola_voltapraargentina"
PaolaCarosellaTemRazao$palavra <- "PaolaTemRazao"
PaolaChefNotaDez$palavra <- "PaolaChefNotaDez"        
PaolaTemRazao$palavra <- "PaolaTemRazao"   

banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_FC)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_lacra_lucra)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina2)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaCarosellaTemRazao)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaChefNotaDez)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaTemRazao)
remove(paola, paola_FC,paola_lacra_lucra,paola_voltapraargentina,paola_voltapraargentina2,PaolaCarosellaTemRazao,PaolaChefNotaDez,PaolaTemRazao)
remove(Paola)

load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[4]))

paola$palavra <- "paola"                   
paola_FC$palavra <- "paola_FC"                
paola_lacra_lucra$palavra <- "paola_lacra_lucra"       
paola_voltapraargentina$palavra <- "paola_voltapraargentina" 
paola_voltapraargentina2$palavra <- "paola_voltapraargentina"
PaolaCarosellaTemRazao$palavra <- "PaolaTemRazao"
PaolaChefNotaDez$palavra <- "PaolaChefNotaDez"        
PaolaTemRazao$palavra <- "PaolaTemRazao"   

banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_FC)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_lacra_lucra)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_voltapraargentina2)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaCarosellaTemRazao)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaChefNotaDez)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaTemRazao)
remove(paola, paola_FC,paola_lacra_lucra,paola_voltapraargentina,paola_voltapraargentina2,PaolaCarosellaTemRazao,PaolaChefNotaDez,PaolaTemRazao)
remove(Paola)


load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[3]))

paola$palavra <- "paola"                   
paola_FC$palavra <- "paola_FC"                
paola_lacra_lucra$palavra <- "paola_lacra_lucra"       
PaolaCarosellaTemRazao$palavra <- "PaolaTemRazao"
PaolaChefNotaDez$palavra <- "PaolaChefNotaDez"        
PaolaTemRazao$palavra <- "PaolaTemRazao"   
paolacarosella_oficial$palavra <- "paolacarosella_oficial"   


banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_FC)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(paola_lacra_lucra)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaCarosellaTemRazao)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaChefNotaDez)
banco_de_dados_paola <- banco_de_dados_paola  %>% add_row(PaolaTemRazao)
remove(paola, paola_FC,paola_lacra_lucra,PaolaCarosellaTemRazao,PaolaChefNotaDez,PaolaTemRazao)
remove(paolacarosella_oficial)

banco_de_dados_paola <-banco_de_dados_paola %>% distinct()
#------------------------------------------------------------------------------------------
saveRDS(banco_de_dados_paola,file='/home/steven/dir-do-R//dados-temporarios/banco_de_dados_paola_27_07_2022.Rds')

paolacarosella_oficial <- get_timelines("PaolaCarosella",n = 10000, 
                                        language = 'pt',
                                        token = token)

saveRDS(paolacarosella_oficial,file='/home/steven/dir-do-R//dados-temporarios/paolacarosella_oficial_27_07_2022.Rds')







#rm(list = ls())
paola_voltapraargentina$palavra <- "paola_voltapraargentina"
paola_voltapraargentina2$palavra <-"paola_voltapraargentina"

paola_voltapraargentina %>% add_row(paola_voltapraargentina2)

banco_de_dados_paola <-tibble::tibble(user_id=NA,status_id=NA,created_at=NA,screen_name=NA,text=NA,source=NA,display_text_width=NA, reply_to_status_id=NA,reply_to_user_id=NA, reply_to_screen_name=NA,is_quote=NA, is_retweet=NA,favorite_count=NA, retweet_count=NA,quote_count=NA, reply_count=NA,hashtags=NA, symbols=NA,urls_url=NA, urls_t.co=NA,urls_expanded_url=NA, media_url=NA,media_t.co=NA, media_expanded_url=NA,media_type=NA, ext_media_url=NA,ext_media_t.co=NA, ext_media_expanded_url=NA,ext_media_type=NA, mentions_user_id=NA,mentions_screen_name=NA, lang=NA,quoted_status_id=NA, quoted_text=NA,quoted_created_at=NA, quoted_source=NA,quoted_favorite_count=NA, quoted_retweet_count=NA,quoted_user_id=NA, quoted_screen_name=NA,quoted_name=NA, quoted_followers_count=NA, quoted_friends_count=NA, quoted_statuses_count=NA,quoted_location=NA, quoted_description=NA,quoted_verified=NA, retweet_status_id=NA,retweet_text=NA, retweet_created_at=NA,retweet_source=NA, retweet_favorite_count=NA,retweet_retweet_count=NA, retweet_user_id=NA,retweet_screen_name=NA, retweet_name=NA,retweet_followers_count=NA, retweet_friends_count=NA,retweet_statuses_count =NA, retweet_location=NA,retweet_description=NA, retweet_verified=NA,place_url=NA, place_name=NA,place_full_name=NA, place_type=NA,country=NA, country_code=NA,geo_coords=NA, coords_coords=NA,bbox_coords=NA, status_url=NA,name=NA,location=NA,description=NA, url=NA,protected=NA, followers_count=NA,friends_count=NA, listed_count=NA,statuses_count=NA, favourites_count=NA,account_created_at=NA,verified=NA,profile_url=NA, profile_expanded_url=NA,account_lang=NA, profile_banner_url=NA, profile_background_url =NA, profile_image_url=NA, palavra=NA)

banco_de_dados_paola <-banco_de_dados_paola %>% add_row()

remove(Aldir_Blanc,emergencia_cultural,Lei_Aldir_Blanc,leialdirblanc,trabalhadores_cultura)
banco_de_dados_paola <-banco_de_dados_aldir_blanc %>% distinct()
#------------------------------------------------------------------------------------------
saveRDS(banco_de_dados_aldir_blanc,file='/home/steven/dir-do-R//dados-temporarios/banco_de_dados_paola_27_07_2022.Rds')





#for(i in 3:8){
#load(paste0("/home/steven/tweet-steven/dados-rtweet/",list.files("/home/steven/tweet-steven/dados-rtweet/",pattern="RData")[i]))
# criacao da origem
#emergencia_cultural$palavra  <-'emergencia_cultural'  

# juntando as linhas 
#banco_de_dados_aldir_blanc <-banco_de_dados_aldir_blanc %>% add_row(Aldir_Blanc)
#banco_de_dados_aldir_blanc <-banco_de_dados_aldir_blanc %>% add_row(emergencia_cultural)

#}

#------------------------------------------------------------------------------------------
