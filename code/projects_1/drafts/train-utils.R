library(stringr)

evaluate_predictions <- function(model, test, outcome, type_model){
  real <- test[[outcome]]
  prediction <- predict(model, test)
  
  if(type_model == 'classification'){
    mean(prediction == real)
  }else{
    sqrt( mean( ( prediction - real )^2 ) )
  }
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
