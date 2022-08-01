

# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
paola_oficial <- readRDS('dados_trabalhados/paolacarosella_oficial_vf.Rds')

table(paola$palavra)




#--------------------------------------------------------------
# Most frequently shared link
#--------------------------------------------------------------
#The “urls_expanded_url” variable provides the full URL of shared links. Here we exclude tweets without a shared link, count, sort the frequency of links in descending order and print the top 5.

most_frequently_shared_link<-paola %>% 
  group_by(palavra) %>%
  filter(!is.na(urls_expanded_url)) %>% 
  count(urls_expanded_url, sort = TRUE) %>% 
  top_n(10)


# remove duplicate rows based on one column in r
# https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth

most_frequently_shared_link <- most_frequently_shared_link %>%
  arrange(urls_expanded_url, -n) %>%
  filter(duplicated(urls_expanded_url) == FALSE)

most_frequently_shared_link <- most_frequently_shared_link %>%
  filter(palavra!='paola')



tabela <-
  most_frequently_shared_link %>%
  gt()  %>% 
  cols_align(align = "left",
             columns = 1) %>% 
  cols_align(align = "left",
             columns = 2) %>%
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Tabela 1 - Links mais compartilhados no Twitter"),
    subtitle = md("10 tweets mais compartilhados no Twitter relacionados à **paola_lacra_lucra** ou **paola_voltapraargentina**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Twitter,2022. Processamento dos autores.")
  ) %>%
  cols_label(
    urls_expanded_url = "Link",
    n = "Quantidade"
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


tabela

gtsave(
  data = tabela,
  filename = paste0(getwd(),"/img/tabela_most_frequently_shared_link_palavra.png"))

remove(tabela,most_frequently_shared_link)
beepr::beep(8)


#writexl::write_xlsx(most_frequently_shared_link,path = "most_frequently_shared_link.xlsx")


