# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')

table(paola$palavra)

# remove duplicate rows based on one column in r
# https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth

paola <- paola %>%
  arrange(text, -favorite_count) %>%
  filter(duplicated(text) == FALSE)



#---------------------------------------------------------
#Most liked tweet
#---------------------------------------------------------
#To find the most liked tweet we can sort our tweets by the
#“favorite_count” variable in descending order and print 
#the rows with the top 5 highest counts.
most_liked_tweet<-paola %>% 
  filter(screen_name!='rhenzonogueira') %>% 
  arrange(-favorite_count) %>%
  top_n(16, favorite_count) %>% 
  select(created_at, screen_name, text, favorite_count)

tabela_most_liked_tweet <- most_liked_tweet %>%
  gt()   %>%
  cols_width(
    starts_with("created_at") ~ px(200),
  ) %>% 
  fmt_datetime(
  columns = created_at,
  date_style = 'day_m_year') %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 2 - Twitter mais curtidos"),
    subtitle = md("15 tweets mais curtidos relacionados à **Paola Carosella** durante o período do estudo.")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    created_at = "Data",
    screen_name = "Nome",
    text = "Tweet",
    favorite_count = "Quantidade de curtidas"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) 


tabela_most_liked_tweet

gtsave(
  data = tabela_most_liked_tweet,
  filename = paste0(getwd(),"/img/tabela_most_liked_tweet.png"))

remove(tabela_most_liked_tweet,most_liked_tweet)

#---------------------------------------------------------
#Most liked tweet por palavra
#---------------------------------------------------------

most_liked_palavra<-paola  %>%  filter(palavra!='paola') %>%  group_by(palavra) %>%
  filter(screen_name!='rhenzonogueira') %>% 
  arrange(-favorite_count) %>%
  top_n(10, favorite_count) %>% 
  select(created_at, screen_name,paola_lacra_lucra, text, favorite_count)


tabela_most_liked_palavra <- 
  most_liked_palavra %>%
  gt()   %>%
  cols_width(
    starts_with("created_at") ~ px(200),
  ) %>% 
  fmt_datetime(
    columns = created_at,
    date_style = 'day_m_year') %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 3 - Twitter mais curtidos"),
    subtitle = md("10 tweets mais curtidos relacionados à **paola_lacra_lucra** ou **paola_voltapraargentina**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    created_at = "Data",
    screen_name = "Nome",
    text = "Tweet",
    favorite_count = "Quantidade de curtidas"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  )  %>%
  tab_style(
    #style = cell_text(weight = "bold"),
    style = cell_fill(color = "#71797E"),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = everything()
    )
  )



tabela_most_liked_palavra

gtsave(
  data = tabela_most_liked_palavra,
  filename = paste0(getwd(),"/img/tabela_most_liked_palavra.png"))

remove(tabela_most_liked_palavra,most_liked_palavra)


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# PAOLA OFICIAL 
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

# remove duplicate rows based on one column in r
# https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth

paola_oficial <- paola_oficial %>%
  arrange(text, -favorite_count) %>%
  filter(duplicated(text) == FALSE)


most_liked_oficial<-paola_oficial %>%
  arrange(-favorite_count) %>%
  top_n(10, favorite_count) %>% 
  select(created_at, text, favorite_count)

tabela_oficial <- 
  most_liked_oficial %>%
  gt()   %>%
  cols_width(
    starts_with("created_at") ~ px(200),
  ) %>% 
  fmt_datetime(
    columns = created_at,
    date_style = 'day_m_year') %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 4 - Twitter mais curtidos"),
    subtitle = md("10 tweets mais curtidos relacionados ao perfil oficial da **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    created_at = "Data",
    text = "Tweet",
    favorite_count = "Quantidade de curtidas"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  )



tabela_oficial

gtsave(
  data = tabela_oficial,
  filename = paste0(getwd(),"/img/tabela_most_liked_oficial.png"))

remove(tabela_oficial,most_liked_oficial)















