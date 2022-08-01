# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')

table(paola$palavra)

# remove duplicate rows based on one column in r
# https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth

paola <- paola %>%
  arrange(text, -retweet_count) %>%
  filter(duplicated(text) == FALSE)

#--------------------------------------------------------------
#Most retweeted tweet
#--------------------------------------------------------------
#The Twitter API helpfully returns a “retweet_count” variable 
#whose values can easily be sorted. Here we sort all the tweets 
#in descending order by the size of the “retweet_count”, 
#slice off the top row and print the date, handle, text and 
#retweet count.
most_retweeted_tweet<-paola  %>% filter(screen_name!='rhenzonogueira') %>% 
  arrange(-retweet_count) %>%
  top_n(15, retweet_count) %>% 
  #slice(1) %>% 
  select(created_at, screen_name, text, retweet_count)


tabela_retweeted <-
  most_retweeted_tweet %>%
  gt()   %>%
  cols_width(
    starts_with("created_at") ~ px(200),
  ) %>% 
  fmt_datetime(
    columns = created_at,
    date_style = 'day_m_year') %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - Twitter mais retweetados"),
    subtitle = md("15 tweets mais retweetados relacionados à **Paola Carosella** durante o período do estudo.")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    created_at = "Data",
    screen_name = "Nome",
    text = "Tweet",
    retweet_count = "Quantidade de retweets"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) 


tabela_retweeted

gtsave(
  data = tabela_retweeted,
  filename = paste0(getwd(),"/img/tabela_retweeted.png"))

remove(tabela_retweeted,most_retweeted_tweet)

#---------------------------------------------------------
#Most retweeted por palavra
#---------------------------------------------------------

most_retweeted_palavra <-paola  %>% filter(screen_name!='rhenzonogueira') %>% 
  filter(palavra!='paola') %>% 
  arrange(-retweet_count) %>%
  group_by(palavra) %>%
  top_n(10, retweet_count) %>% 
  #slice(1) %>% 
  select(created_at, screen_name, text, retweet_count,palavra)



tabela_retweeted <-
  most_retweeted_palavra %>%
  gt()   %>%
  cols_width(
    starts_with("created_at") ~ px(200),
  ) %>% 
  fmt_datetime(
    columns = created_at,
    date_style = 'day_m_year') %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - Twitter mais retweetados"),
    subtitle = md("15 tweets mais retweetados relacionados à **paola_lacra_lucra** ou **paola_voltapraargentina**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    created_at = "Data",
    screen_name = "Nome",
    text = "Tweet",
    retweet_count = "Quantidade de retweets"
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  )   %>%
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


tabela_retweeted

gtsave(
  data = tabela_retweeted,
  filename = paste0(getwd(),"/img/tabela_retweeted_palavra.png"))

remove(tabela_retweeted,most_retweeted_palavra)
beepr::beep(8)











