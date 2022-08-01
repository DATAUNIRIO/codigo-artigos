# C:\Users\Hp\Documents\GitHub\Mobilidade_Urbana\codigo julho de 2021

#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola<- readRDS('dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')

table(paola$palavra)


#----------------------------------------------------------------
#                 lista de palavras banidas
#----------------------------------------------------------------

palavras_banidas<-c("e", "de", "o", "a", "com", "que", "do", "um", "para", "uma", 
                    "no", "em", "os", "da", "pelo", "ao", "mas", "nos", "na", "ser", 
                    "as", "por","tambem",":","alem","so","ate","pra",".","'",")", "(")

aspas<-'"'
virgula<-","
parenteses <- ")"

library(quanteda)
palavras_banidas2<-quanteda::stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,palavras_banidas2)
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
palavras_banidas<-append(palavras_banidas,parenteses)
remove(palavras_banidas2,aspas,virgula,parenteses)


#----------------------------------------------------------------
#----------------------------------------------------------------
#    CORPUS
#----------------------------------------------------------------
#----------------------------------------------------------------

library(dplyr)
library(quanteda)

corp   <- corpus(paola$text)
corp_polarizado   <- paola %>% filter(palavra!='paola') %>% pull(text) %>% corpus()
paola2 <-paola %>% filter(palavra!='paola')
docvars(corp_polarizado, "palavra") <- paste0(paola2$palavra)
corp_polarizado2   <-corpus_group(corp_polarizado, groups = palavra)

head(docvars(corp_polarizado))

texto_palavra   <- paola %>% filter(palavra!='paola') %>% pull(palavra)
corp_polarizado$palavra <- paste(texto_palavra)
table(corp_polarizado$palavra)
remove(texto_palavra)

# the puncuations and numbers in the texts were removed as there is no need to predict punctations or numbers
palavras <- tokens(corp,"word",
                   remove_numbers = T,
                   remove_symbols = T,
                   remove_punct = T,
                   remove_twitter = TRUE,
                   remove_url = TRUE,
                   remove_separators = T,
                   remove_hyphens = F) %>% 
  tokens_remove(pattern = palavras_banidas)

palavras_polarizadas <- tokens(corp_polarizado2,"word",
                   remove_numbers = T,
                   remove_symbols = T,
                   remove_punct = T,
                   remove_url = TRUE,
                   remove_separators = T) %>% 
  tokens_remove(pattern = palavras_banidas)

# After cleaning the data, we have to stem it.
# library(SnowballC)
# #getStemLanguages()
# palavras_stem <- tokens_wordstem(palavras, language = "portuguese")
# 
# palavras_stem[["text1"]]
# palavras[["text1"]]
# 
# palavras_stem[["text2"]]
# palavras[["text2"]]
# 
# remove(palavras_stem)

#----------------------------------------------------------------
#                       DFM
#----------------------------------------------------------------
# criando o document-feature matrix
dfm1 <- dfm(palavras) %>%
  dfm_remove(palavras_banidas) %>%
  dfm_trim(min_termfreq = 5) 


#----------------------------------------------------------------
#               NUVEM DE PALAVRAS
#----------------------------------------------------------------
library(quanteda.textplots)
png("img/nuvem/nuvem_31_07_2022.png",width = 1000, height = 800, units = "px")
nuvem<-textplot_wordcloud(dfm1)
dev.off()
#----------------------------------------------------------------

# comparison plot of lacra_y_lucra e paolavoltaparaargentina
dfm2 <- dfm(palavras_polarizadas) %>%
  dfm_remove(palavras_banidas) %>%
  dfm_trim(min_termfreq = 5)  

dfm2 <- dfm_group(dfm2, groups = palavra)
png("img/nuvem/nuvem_polarizada_31_07_2022.png",width = 1000, height = 800, units = "px")
textplot_wordcloud(dfm2, comparison = TRUE,
                   min_size = 1.5, max_size=4,
                   color = c("blue", "red"))
dev.off()
#----------------------------------------------------------------
#               Word keyness
#----------------------------------------------------------------
library(ggthemes)
library(ggplot2)
library(quanteda.textstats)

# keyness<-corp %>%  corpus_subset(veiculo %in% c("barcas", "bicicleta", "brt", "metro", "moto_taxi","onibus","supervia","taxi","trem","uber")) %>%
PALAVRAS <-c('(',' )','q','ta','ja','la')
meu_docs<-corp_polarizado$palavra
keyness <- corp_polarizado %>% dfm() %>% dfm_group(groups = meu_docs) %>%   dfm_remove(c(palavras_banidas,PALAVRAS))   
remove(meu_docs)

keyness %>%
  textstat_keyness(target = "paola_lacra_lucra") %>%   
  textplot_keyness(color = c("red", "blue"))

png("img/nuvem/keyness_31-07-2021.png",width = 1100, height = 800, units = "px")
keyness %>%
  textstat_keyness(target = "paola_lacra_lucra") %>%   
  textplot_keyness(color = c("yellow", "purple"))+ theme(legend.position="bottom")
dev.off()

#----------------------------------------------------------------
# Plotting topic model
#----------------------------------------------------------------
# It seems the DFM still contains words that don't pertain to my research, so I will clean the DFM to only contain words that appear in 50% of the tweets
#text_dfm <- dfm_trim(text, min_termfreq = .3, docfreq_type = "prop")

library(topicmodels)
library(tidytext)
dtm <- convert(dfm1, to = "topicmodels")
m = LDA(dtm, method = "Gibbs", k = 9,  control = list(alpha = 0.1))
terms(m, 5)

m_topics <- tidy(m, matrix = "beta")

m_top_terms <- 
  m_topics %>%
  group_by(topic) %>%
  top_n(8,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


library (ggplot2)

m_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  mutate(topic = paste("Topic #", topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  theme_minimal() +
  theme(plot.title = 
          element_text(hjust = 0.5, size = 18)) +
  labs(
    title = "Modelo de tópicos dos Tweets relacionados a Paola Carosella",
    caption = "Principais termos por tópico (top Terms by Topic)"
  ) + 
  ylab("") +
  xlab("") +
  coord_flip()
ggsave("img/nuvem/topic_model.png",width = 3400, height = 1800, units = "px")


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
#            Find Frequent Terms
#Find frequent terms in a document-term or term-document matrix.
#----------------------------------------------------------------
library(tm)
palavras_banidas
s <- paola$text
for(i in palavras_banidas) {
  s[i] <- gsub(palavras_banidas[i]," ",s)
}

#s <- gsub(palavras_banidas," ",s)
s <- SimpleCorpus(VectorSource(unlist(lapply(s, as.character))))
#s <- SimpleCorpus(VectorSource(unlist(lapply(banco_completo$text, as.character))))
m <- TermDocumentMatrix(s, control = list(weighting =
                                            function(x)
                                              weightTfIdf(x, normalize =FALSE),
                                          removeNumbers = TRUE,
                                          stopwords = TRUE,
                                          stemming = FALSE))
FT <-m %>% findFreqTerms(lowfreq = 5000)

inaugFeatures <- topfeatures(dfm1, 150)
# Create a data.frame for ggplot
topDf <- data.frame(
  list(
    palavra = names(inaugFeatures),
    frequencia = unname(inaugFeatures)
  )
)

`%!in%` = Negate(`%in%`)
topDf<-topDf[topDf$palavra%!in% c('via','vai', 'la','q', 'ja','apos','ta','sao'),]


library(ggplot2)
topDf %>% filter(frequencia>680) %>% 
  ggplot(aes(x = frequencia)) +
  geom_bar(aes(y = reorder(palavra, frequencia)),stat = "identity",color='white',fill='steelblue')+
  labs(x="frequência", y="palavra")

ggsave("img/nuvem/top.png")
#ggsave("img/nuvem/top.png",width = 1200, height = 1200, units = "px")

# Sort by reverse frequency order
#topDf$palavra <- with(topDf, reorder(palavra, -frequencia))
#write.csv2(topDf,file = "tweets_do_servidor_12_03_2021/resultados/top_100_04_04_2021.csv")

# criando a rede
png("img/nuvem/rede.png",width = 800, height = 600, units = "px")
dfm_trim(dfm1,
         min_termfreq = 50,
         termfreq_type = "rank")  %>%   dfm_remove(c(palavras_banidas,PALAVRAS))   %>% 
  textplot_network(edge_size = 0.6,edge_color="grey",
                   vertex_color = c("black"))+
  labs(title = "Co-ocorrência de termos",
       subtitle = "Tweets sobre Paola Carosella",
       x = "",  y = "")+
  theme_minimal()
dev.off()
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
library(gt)

tabela<-topDf %>% 
  gt()  %>% 
  cols_align(align = "left",
             columns = 1) %>% 
  cols_align(align = "right",
             columns = 2) %>% 
  cols_width(
    everything() ~ px(200)
  )  %>% 
  tab_options(table_body.hlines.color = "lightgrey") %>%
  tab_header(
    title = md("Palavras"),
    subtitle = md("mais utilizadas relacionadas à **Paola Carosella**")
  ) %>%
  tab_source_note(
    source_note = md("Fonte: Tweeter,2022. Processamento dos autores")
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = everything()
    )
  )%>%
  cols_label(
    palavra = "Palavra Utilizada",
    frequencia = "Frequência")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#801614"),
    locations = cells_column_spanners(
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(
    )
  )

gtsave(
  data = tabela,
  filename = "img/nuvem/tabela_freq.png")



# Set reference scores
refscores <- c(rep(NA, 4), 1, -1, rep(NA, 8))
refscores <- c(seq(-1.5, 1.5, 0.75), NA)
refscores <- ifelse(dfm2@docvars$palavra=='paola_lacra_lucra',1,-1)


library("quanteda.textmodels")
library(quanteda.textplots)

# comparison plot of lacra_y_lucra e paolavoltaparaargentina
dfm3 <- dfm(palavras_polarizadas) %>%
  dfm_remove(palavras_banidas) %>%
  dfm_trim(min_termfreq = 50)  

dfm3 <- dfm_group(dfm3, groups = palavra)
# Predict Wordscores model
ws <- textmodel_wordscores(dfm3, y = refscores, smooth = 1)

# Plot estimated word positions (highlight words and print them in red)
textplot_scale1d(ws,
                 highlighted = c("paolatemrazao", "paola_lacra_y_lucra","paolacarosellatemrazao","dilmaetaon",
                                 "argentina","escrota","voltaparaargentinapaolacarosella","lucianohangbr"), 
                 highlighted_color = "red")+
  labs(subtitle = "Posição estimada das palavras.",
       caption = '-1 representa "paola volta pra argentina"" e 1 representa "paola lacra y lucra"')
ggsave("img/nuvem/Predict_Wordscores_model.png",width = 4600, height = 2500, units = "px")


# Get predictions
pred <- predict(ws, se.fit = TRUE)

# Plot estimated document positions and group by "party" variable
textplot_scale1d(pred, margin = "documents",
                 groups = dfm3@docvars$palavra)

ggsave("img/nuvem/Plot estimated document positions.png",width = 4600, height = 2500, units = "px")
# Estimate Wordfish model
wf <- textmodel_wordfish(dfm3)

# Plot estimated word positions
textplot_scale1d(wf, margin = "features", 
                 highlighted = c("paolatemrazao", "paola_lacra_y_lucra","paolacarosellatemrazao","dilmaetaon",
                                 "argentina","escrota","voltaparaargentinapaolacarosella","lucianohangbr"), 
                 highlighted_color = "red")+
  labs(subtitle = " Wordfish model.")
ggsave("img/nuvem/Estimate Wordfish model.png",width = 4600, height = 2500, units = "px")


