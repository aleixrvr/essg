library(magrittr)
library(logger)

train_model <- function(
  dt, outcome_name, k_fold=5, tuneLength=5, models=c('lm', 'boosting', 'elastic'), verbose=FALSE
){
  results <- list()
  pred_formula <- '`{outcome_name}` ~ .' %>% f %>% as.formula
  trControl <-  caret::trainControl(method = "cv", number = k_fold)
  # trControl <-  caret::trainControl(method = "cv", number = k_fold, classProbs = TRUE)
  model_type <- ifelse(class(dt[[outcome_name]]) == 'numeric', 'regression', 'classification')
  method <- ifelse(model_type == 'regression', 'lm', 'glm')
  
  if( model_type == 'classification' ){
    metric <- 'Accuracy'
    # metric <- 'ROC'
  }else{
    metric <- 'RMSE'
  }
  
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
      pred_formula, data = dt, method='xgbTree', trControl = trControl, tuneLength = tuneLength, 
      verbose = FALSE, metric = metric
    )
    if( verbose == TRUE ) log_info('Boosting -> Done \n')
  }
  
  
  # Elastic Net
  if( 'elastic' %in% models ){
    results[['elastic_net']] <- caret::train(
      pred_formula, data=dt, method='glmnet', trControl = trControl, standardize = FALSE, 
      tuneLength = tuneLength, metric = metric
    )
    if( verbose == TRUE ) log_info('Elastic Net -> Done \n ')
  }
  
  # Lasso
  if( 'lasso' %in% models ){
    tuneGrid <- data.frame(alpha = 1, lambda = 10^seq(-4, -1, length = tuneLength))
    results[['elastic_net']] <- caret::train(
      pred_formula, data=dt, method='glmnet', trControl = trControl, standardize = FALSE, 
      metric = metric, tuneGrid = tuneGrid
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
      model_type = model_type,
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
      model_type = model_type,
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

bootstrap_model <- function(dt_, outcome_name, trained_model, is_classification, repetitions=10){
  pred_formula <- '`{outcome_name}` ~ .' %>% f %>% as.formula
  sel_model <- trained_model$sel_model
  
  if( 'lm' == sel_model ){
    method_ <- 'lm'
  }else if('glm' == sel_model){
    method_ <- 'glm'
  }else if('elastic_net' == sel_model){
    method_ <- 'glmnet'
  }else if('boosting' == sel_model){
    method_ <- 'xgbTree'
  }else{
    stop('Wrong model selection')
  }
  
  row_n <- dt_ %>% nrow
  predictions <- data.table()
  for(r_ in 1:repetitions){
    inds <- sample(1:row_n, row_n, replace=TRUE)
    dt_sample <- dt_[inds, ]
    
    model_ <- train(pred_formula, dt_sample, method=method_, tuneGrid=trained_model$params)
    
    if( is_classification == TRUE ){
      pred_y <- predict(model_, dt_sample, type='prob')$Yes
    }else{
      pred_y <- predict(model_, dt_sample)
    }
    
    predictions %<>% cbind(pred_y)
  }
  
  return(predictions)
}


