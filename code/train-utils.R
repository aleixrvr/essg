library(stringr)

evaluate_predictions <- function(model, test, outcome){
  real <- test[, outcome]
  prediction <- predict(model, test)
  caret::postResample(pred = prediction, obs = real)
}

. %>% 
  str_replace_all('/' %>% fixed, '') %>% 
  str_replace_all('(' %>% fixed, '') %>% 
  str_replace_all(')' %>% fixed, '') %>% 
  str_replace_all('-' %>% fixed, '') %>% 
  str_replace_all('  ' %>% fixed, '_') %>% 
  str_replace_all(' ' %>% fixed, '_') ->
  clean_name

colnames(dt) %>% sapply(clean_name)
