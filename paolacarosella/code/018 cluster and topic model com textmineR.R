# https://blog.metodosquantitativos.com/textclustering/index.html

# semente para reproduzir as simulacoes
set.seed(12345)
# carregar a biblioteca
library(textmineR)

paola<- readRDS('dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')

table(paola$palavra)

  
#----------------------------------------------------------------
# selecionando aleatoriamente 150 tweets do banco de dados
#----------------------------------------------------------------
library(dplyr)
paola<- paola %>% filter(palavra!="paola")
paola <- paola %>% select(text,status_id)%>% sample_n(1000)
paola$id <-1:dim(paola)[1]

#----------------------------------------------------------------
# criando o document term matrix 
#----------------------------------------------------------------
dtm <- CreateDtm(doc_vec = paola$text, # character vector of documents
                 doc_names = paola$status_id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("pt"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, #
                 cpus = 2) # default is all available cpus on the system

# construct the matrix of term counts to get the IDF vector
tf_paola <- TermDocFreq(dtm)


kable(head(tf_paola))
# TF-IDF 
tfidf <- t(dtm[ , tf_paola$term ]) * tf_paola$idf
tfidf <- t(tfidf)

# similaridade por cosseno
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

# %*% is matrix multiplication
csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)

hc <- hclust(cdist, method="ward.D")

clustering <- cutree(hc, 5)

plot(hc, main = "agrupamento hierárquico dos 150 tweers da LAB",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 5, border = "red")

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[clustering == 5 , ]

  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})



# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
library(gt)
gt(cluster_summary)

#-----------------------------------------------------------------------------
#----------------------------------------------------------------
# Topic modeling/Modelagem de tópicos
#----------------------------------------------------------------
#----------------------------------------------------------------

# semente para reproduzir as simulacoes
set.seed(12345)
# carregar a biblioteca
library(textmineR)

paola<- readRDS('dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')

table(paola$palavra)


#----------------------------------------------------------------
# selecionando aleatoriamente 150 tweets do banco de dados
#----------------------------------------------------------------
library(dplyr)
paola1<- paola %>% filter(palavra=="paola_voltapraargentina")
paola2<- paola %>% filter(palavra=="paola_lacra_lucra")
paola2 <- paola2 %>%  sample_n(296)
paola <- paola1 %>% add_row(paola2)
paola$id <-1:dim(paola)[1]
paola <- paola %>% select(text,id)


#----------------------------------------------------------------
# criando o document term matrix 
#----------------------------------------------------------------
dtm <- CreateDtm(doc_vec = paola$text, # character vector of documents
                 doc_names = paola$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("pt"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, #
                 cpus = 2) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2]
# Fit a Latent Dirichlet Allocation model
# note the number of topics is arbitrary here

model <- FitLdaModel(dtm = dtm, 
                     k = 10,
                     iterations = 500, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 



model$r2
# log Likelihood (does not consider the prior) 
plot(model$log_likelihood, type = "l",
     main="Log-verossimilhança do modelo com R2 de 0,659")


# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
tabela <- head(t(model$top_terms))
tabela <- data.frame(tabela)

library(gt)
tabela <- gt(tabela)%>%
  cols_label(
    X1 = "Tópico 1",
    X2 = "Tópico 2",
    X3 = "Tópico 3",
    X4 = "Tópico 4",
    X5 = "Tópico 5",
    X6 = "Tópico 6",
    X7 = "Tópico 7",
    X8 = "Tópico 8",
    X9 = "Tópico 9",
    X10 = "Tópico 10")
gtsave(
  data = tabela,
  #filename = "img/textmineR/tabela_top_terms_of_each_topic.png")
  filename = "img/textmineR/tabela_top_terms_of_each_topic.html")


model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)

head(model$labels)

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)


tabela2<-model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]
tabela2 <- gt(tabela2)%>%
  cols_label(
    topic = "Tópico",
    label_1 = "Rótulo",
    coherence = "Coerência",
    prevalence = "Prevalência",
    top_terms = "Principais termos")
gtsave(
  data = tabela2,
  #filename = "img/textmineR/tabela_top_terms_of_each_topic.png")
  filename = "img/textmineR/tabela_top_terms_of_each_topic_and label.html")


# predictions with gibbs
assignments <- predict(model, dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)

# predictions with dot
assignments_dot <- predict(model, dtm,
                           method = "dot")


# compare
barplot(rbind(assignments[10,], assignments_dot[10,]),
        col = c("red", "blue"), las = 2, beside = TRUE)
legend("topright", legend = c("gibbs", "dot"), col = c("red", "blue"), 
       fill = c("red", "blue"))
