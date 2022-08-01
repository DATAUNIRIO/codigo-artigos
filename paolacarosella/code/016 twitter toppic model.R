
#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------

paola<- readRDS('dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')

table(paola$palavra)

library(Twitmo)
pool <- pool_tweets(paola)

pool.corpus <- pool$corpus
pool.dfm <- pool$document_term_matrix


#Find optimal number of topics
find_lda(pool.dfm)

#Fitting a LDA model
model <- fit_lda(pool.dfm, n_topics = 9)

#View most relevant terms for each topic
lda_terms(model)

#or which hashtags are heavily associated with each topic
lda_hashtags(model)

#Inspecting LDA distributions
#Check the distribution of your LDA Model with
lda_distribution(model)

#Filtering tweets
#Sometimes you can build better topic models by blacklisting or whitelisting certain keywords from your data. You can do this with a keyword dictionary using the filter_tweets() function. In this example we exclude all tweets with “football” or “mood” in them from our data.
paola %>% dim()
filter_tweets(paola, keywords = "paola,carosella", include = FALSE) %>% dim()
#Analogously if you want to run your collected tweets through a whitelist use
mytweets %>% dim()
filter_tweets(mytweets, keywords = "football,mood", include = TRUE) %>% dim()
#Fiting a STM
#Structural topic models can be fitted with additional external covariates. In this example we metadata that comes with the tweets such as retweet count. This works with parsed unpooled tweets. Pre-processing and fitting is done with one function.

stm_model <- fit_stm(paola, n_topics = 9, xcov = ~ retweet_count + followers_count + reply_count + quote_count + favorite_count,
                     remove_punct = TRUE,
                     remove_url = TRUE,
                     remove_emojis = TRUE,
                     stem = TRUE,
                     stopwords = "pt")

# STMs can be inspected via
summary(stm_model)

# Visualizing models with LDAvis
library("LDAvis")
library("servr")

to_ldavis(model, pool.corpus, pool.dfm)

#Plotting geo-tagged tweets
paola         <- readRDS('dados_trabalhados/paola_vf_27_07_2022.Rds')
plot_tweets(paola, region = "Brazil", alpha=0.1)
plot_hashtag(paola, region = "Brazil", hashtag = "VoltaPraArgentina", ignore_case=TRUE, alpha=0.2)

cluster_tweets(paola)

