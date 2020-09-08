library(magrittr)
library(logger)

train_model <- function(dt, outcome, k_fold=5, tuneLength=5, models=c('lm', 'boosting', 'elastic')){
  results <- list()
  pred_formula <- '{outcome} ~ .' %>% f %>% as.formula
  trControl <-  caret::trainControl(method = "cv", number = k_fold)
  method <- ifelse(class(dt[[outcome]]) == 'numeric', 'lm', 'glm')
  
  # Linear Model with data partition (train/test)
  if( 'lm' %in% models){
    results[[model_type]] <-   caret::train(
      pred_formula, data = dt,
      method = method,
      trControl = trControl
    )
    log_info('LM -> Done')
  }
  
  # Gradient Boosting Machine
  if( 'boosting' %in% models ){
    results[['boosting']] <-   caret::train(
      pred_formula, data = dt, method='xgbTree', trControl = trControl, tuneLength = tuneLength, verbose = FALSE
    )
    log_info('Boosting -> Done \n')
  }
  
  
  # Elastic Net
  if( 'elastic' %in% models ){
    results[['elastic_net']] <- caret::train(
      pred_formula, data=dt, method='glmnet', trControl = trControl, standardize = FALSE, tuneLength = tuneLength
    )
    log_info('Elastic Net -> Done \n ')
  }
  
  return(results)  
}

select_best_model <- function(results_model){
  
  results_model %>% lapply(
    . %>% .$results %>% tail(1) %>% .$Accuracy
  ) -> results_accuracy
  
  results_accuracy %>% 
    which.max %>% 
    names ->
    sel_model
  
  return(list(
    sel_model = sel_model, 
    params = results_model[[sel_model]]$bestTune, 
    accuracy = results_accuracy[[sel_model]], 
    model = results_model[[sel_model]]
  ))
}

