library(magrittr)
library(logger)

train_model <- function(
  dt, outcome_name, k_fold=5, tuneLength=5, models=c('lm', 'boosting', 'elastic'), verbose=FALSE
){
  results <- list()
  pred_formula <- '`{outcome_name}` ~ .' %>% f %>% as.formula
  trControl <-  caret::trainControl(method = "cv", number = k_fold)
  model_type <- ifelse(class(dt[[outcome_name]]) == 'numeric', 'regression', 'classification')
  method <- ifelse(model_type == 'regression', 'lm', 'glm')
  
  # Linear Model with data partition (train/test)
  if( 'lm' %in% models){
    results[[method]] <-   caret::train(
      pred_formula, data = dt,
      method = method,
      trControl = trControl
    )
    if( verbose == TRUE ) log_info('LM -> Done')
  }
  
  # Gradient Boosting Machine
  if( 'boosting' %in% models ){
    results[['boosting']] <-   caret::train(
      pred_formula, data = dt, method='xgbTree', trControl = trControl, tuneLength = tuneLength, verbose = FALSE
    )
    if( verbose == TRUE ) log_info('Boosting -> Done \n')
  }
  
  
  # Elastic Net
  if( 'elastic' %in% models ){
    results[['elastic_net']] <- caret::train(
      pred_formula, data=dt, method='glmnet', trControl = trControl, standardize = FALSE, tuneLength = tuneLength
    )
    if( verbose == TRUE ) log_info('Elastic Net -> Done \n ')
  }
  
  best_model <- select_best_model(results, model_type)
  return(best_model)  
}

select_best_model <- function(results_model, model_type){
  
  if( model_type == 'regression' ){
    results_model %>% lapply(
      . %>% .$results %>% .$RMSE %>% min
    ) -> results_rmse
    
    results_rmse %>% 
      which.min %>% 
      names ->
      sel_model
    
    best_model <- list(
      sel_model = sel_model, 
      params = results_model[[sel_model]]$bestTune, 
      rmse = results_rmse[[sel_model]], 
      model = results_model[[sel_model]]
    )
  }else{
    results_model %>% lapply(
      . %>% .$results %>% .$Accuracy %>% max
    ) -> results_accuracy
    
    results_accuracy %>% 
      which.max %>% 
      names ->
      sel_model
    
    best_model <- list(
      sel_model = sel_model, 
      params = results_model[[sel_model]]$bestTune, 
      accuracy = results_accuracy[[sel_model]], 
      model = results_model[[sel_model]]
    )
  }
  
  return(best_model)
}

run_cross_validation <- function(){
  
}
