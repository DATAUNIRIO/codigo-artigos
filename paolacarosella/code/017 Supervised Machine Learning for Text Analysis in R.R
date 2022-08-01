
#----------------------------------------------------------------
# Carregar o banco de dados                 
#----------------------------------------------------------------
paola<- readRDS('dados_trabalhados/paola_vf_27_07_2022_SEM_EMOJI_E_SEM_CARACTER_ESPECIAL.RDS')
table(paola$palavra)

library(dplyr)
paola <- paola %>% filter(palavra!="paola")

library(tidymodels)
set.seed(1234)
paola_split <- initial_split(paola, strata = palavra)
paola_train <- training(paola_split)
paola_test <- testing(paola_split)
dim(paola_train)
dim(paola_test)

paola_rec <-  recipe(palavra ~ text, data = paola_train)

library(textrecipes)
paola_rec <- paola_rec %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text)

paola_wf <- workflow() %>%
  add_recipe(paola_rec)

library(discrim)
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")
nb_spec


nb_fit <- paola_wf %>%
  add_model(nb_spec) %>%
  fit(data = paola_train)

# We have trained our first classification model!
# Evaluation
paola_folds <- vfold_cv(paola_train)
paola_folds

nb_wf <- workflow() %>%
  add_recipe(paola_rec) %>%
  add_model(nb_spec)
nb_wf


nb_rs <- fit_resamples(
  nb_wf,
  paola_folds,
  control = control_resamples(save_pred = TRUE)
)


#We can extract the relevant information using collect_metrics() and collect_predictions()
nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs, summarize = F)

nb_rs_metrics

nb_rs_predictions <-nb_rs_predictions %>% rename(pred_palavra=.pred_class)

nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = product, .pred_Credit) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for",
    subtitle = "Each resample fold is shown in a different color"
  )

nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = palavra, .pred_paola_lacra_lucra) %>%
  autoplot()

nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = palavra, .pred_paola_voltapraargentina) %>%
  autoplot()+
  labs(
    color = NULL,
    title = "ROC curve for US Consumer Finance Complaints",
    subtitle = "Each resample fold is shown in a different color"
  )

# Confusion matrix for naive Bayes classifier, showing some bias
conf_mat_resampled(nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")
ggsave('img/Supervised Machine Learning/Confusion matrix for naive Bayes classifier.png')


lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
lasso_spec
lasso_wf <- workflow() %>%
  add_recipe(paola_rec) %>%
  add_model(lasso_spec)

set.seed(2020)
lasso_rs <- fit_resamples(
  lasso_wf,
  paola_folds,
  control = control_resamples(save_pred = TRUE)
)


lasso_rs_metrics <- collect_metrics(lasso_rs)
lasso_rs_predictions <- collect_predictions(lasso_rs)

lasso_rs_metrics

lasso_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = palavra, .pred_paola_lacra_lucra) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for US Consumer Finance Complaints",
    subtitle = "Each resample fold is shown in a different color"
  )

ggsave('img/Supervised Machine Learning/ROC curve for lasso classification model.png',width = 1600,height = 1200,units ="px" )

conf_mat_resampled(lasso_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")
ggsave('img/Supervised Machine Learning/Confusion matrix for lasso classification model.png')

# Tuning lasso hyperparameters

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
tune_spec

lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

tune_wf <- workflow() %>%
  add_recipe(paola_rec) %>%
  add_model(tune_spec)
set.seed(2020)
tune_rs <- tune_grid(
  tune_wf,
  paola_folds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)
tune_rs

collect_metrics(tune_rs)

autoplot(tune_rs) +
  labs(
    title = "Desempenho do modelo Lasso em todas as penalidades/regularização",
    subtitle = "Métricas de desempenho podem ser usadas para identificar a melhor penalidade"
  )
ggsave('img/Supervised Machine Learning/Desempenho do modelo Lasso.png',width = 2000,height = 1600,units ="px" )

tune_rs %>%
  show_best("roc_auc")

chosen_auc <- tune_rs %>%
  select_by_one_std_err(metric = "roc_auc", -penalty)
chosen_auc

final_lasso <- finalize_workflow(tune_wf, chosen_auc)
final_lasso
fitted_lasso <- fit(final_lasso, paola_train)

coeficientes <- fitted_lasso %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(-estimate)


writexl::write_xlsx(coeficientes,path = "img/Supervised Machine Learning/coeficientes_lasso.xlsx")

library(vip)
library(forcats)
paola_imp <- pull_workflow_fit(fitted_lasso) %>%
  vi(lambda = chosen_auc$penalty)
paola_imp %>%
  mutate(
    Sign = case_when(Sign == "POS" ~ "Menos provável lacra e lucra",
                     Sign == "NEG" ~ "Mais provável lacra e lucra"),
    Importance = abs(Importance)) %>%
  group_by(Sign) %>%
  top_n(11, Importance) %>%
  ungroup %>%
  ggplot(aes(x = Importance,
             y = fct_reorder(Variable, Importance),
             fill = Sign)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~Sign, scales = "free") +
  labs(
    y = NULL,
    title = "Importância da variável/palavra para prever o tema lacra_y_lucra ou voltapra_argentina",
    subtitle = paste("Esses recursos/palavras são os mais importantes na previsão",
                     " para prever o tema lacra_y_lucra ou voltapra_argentina")
  )

ggsave('img/Supervised Machine Learning/Importância_da_palavra.png',width = 3000,height = 2200,units ="px" )

#We can gain some final insight into our model by looking at observations from
#the test set that it misclassified. Let’s bind together the predictions on the
#test set with the original complaints_test data. Then let’s look at complaints
#that were labeled as about credit reporting in the original data but that our
#final model thought had a low probability of being about credit reporting.

paola_bind <- collect_predictions(lasso_rs) %>%
  bind_cols(paola_train %>% select(-palavra))
paola_bind %>%
  filter(palavra == "paola_lacra_lucra", .pred_paola_lacra_lucra < 0.2) %>%
  select(text)

paola_bind %>%
  filter(palavra == "paola_lacra_lucra", .pred_paola_lacra_lucra < 0.8) %>%
  select(text)


#--------------------------------------------------------
# amostra de dez textos mais fortes
amostra1 <- paola_bind %>%
  filter(.pred_paola_lacra_lucra < 0.1) %>%
  select(text) %>% 
  slice_sample(n = 10)

amostra2 <- paola_bind %>%
  filter(.pred_paola_lacra_lucra < 0.9) %>%
  select(text)  %>% 
  slice_sample(n = 10)

prox_palavra<-list(amostra1,amostra2)
writexl::write_xlsx(prox_palavra,path = "img/Supervised Machine Learning/amostra_de_dez_textos_mais_fortes_a_favor_e_contra.xlsx")



