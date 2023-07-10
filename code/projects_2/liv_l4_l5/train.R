library(magrittr)
library(data.table)
library(caret)
# library(MLmetrics)
library(pROC)



train_model <- function(
  dt, outcome_name, k_fold=5, tuneLength=5, 
  models=c('lm', 'boosting', 'elastic'), verbose=FALSE,
  only_one=FALSE, tuneGrid = NULL, split_vars = NULL
){
  results <- list()
  pred_formula <- '`{outcome_name}` ~ .' %>% f %>% as.formula
  
  numeric_types <- c('numeric', 'integer')
  model_type <- ifelse(class(dt[[outcome_name]]) %in% numeric_types, 'regression', 'classification')
  if( model_type == 'classification' ){
    split_cat <- dt[, .SD, .SDcols = c(outcome_name, split_vars)]
    y_folds <- split_cat %>% 
      .[, 
        .(new_cat = paste(.SD, collapse="")), 
        1:nrow(split_cat)] %>% 
      .[, new_cat]
  }else{
    y_folds <- dt[[outcome_name]]
  }
  
  index <- createFolds(y_folds, k_fold, returnTrain = TRUE)
  # trControl <-  caret::trainControl(method = "cv", number = k_fold, index=index)
  
  method <- ifelse(model_type == 'regression' & model_type=='classification', 'lm', 'glm')
  
  outcome_val <- dt[[outcome_name]] %>% unique %>% .[1] %>% as.character()
  auc_metric <- function(data, lev=NULL, model=NULL){
    auc_res <- auc(roc(data$obs, data[[outcome_val]]))
    return(c(AUC=auc_res))
  }
  
  if( model_type == 'classification' ){
    metric <- 'AUC'
    objective <- 'binary:logistic'
    if( only_one ){
      tuneLength <- 1
      k_fold <- 1
      trControl <- caret::trainControl(
        method = "none", number = k_fold, classProbs = TRUE, savePredictions='all')
    }else{
      trControl <- caret::trainControl(
        method = "cv", number = k_fold, classProbs = TRUE, 
        summaryFunction=auc_metric, savePredictions='all', index=index)
    }
  }else{
    metric <- 'RMSE'
    objective <- 'reg:squarederror'
    if( only_one ){
      tuneLength <- 1
      k_fold <- 1
      trControl <-  caret::trainControl(
        method = "none", number = k_fold, savePredictions='all')
    }else{
      trControl <-  caret::trainControl(
        method = "cv", number = k_fold, savePredictions='all', index=index)
    }
  }
  
  # Linear Model with data partition (train/test)
  if( 'lm' %in% models | 'glm' %in% models){
    results[[method]] <-   caret::train(
      pred_formula, data = dt,
      method = method,
      trControl = trControl, 
      tuneGrid=tuneGrid
    )
    if( verbose == TRUE ) log_info('LM -> Done')
  }
  
  # Gradient Boosting Machine
  if( 'boosting' %in% models ){
    results[['boosting']] <-   caret::train(
      pred_formula, data = dt, method='xgbTree', 
      trControl = trControl, tuneLength = tuneLength, 
      verbose = FALSE, metric = metric, maximize=TRUE, 
      objective=objective, tuneGrid=tuneGrid
    )
    if( verbose == TRUE ) log_info('Boosting -> Done \n')
  }
  
  
  # Elastic Net
  if( 'elastic' %in% models ){
    results[['elastic']] <- caret::train(
      pred_formula, data=dt, method='glmnet',
      trControl = trControl, standardize = FALSE, 
      tuneLength = tuneLength, metric = metric, 
      maximize=TRUE, tuneGrid=tuneGrid
    )
    if( verbose == TRUE ) log_info('Elastic Net -> Done \n ')
  }
  
  best_model <- select_best_model(results, model_type)
  return(best_model)
  # return(results)
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
      . %>% .$results %>% .$AUC %>% max
    ) -> results_auc
    
    results_auc %>% 
      which.max %>% 
      names ->
      sel_model
    
    best_model <- list(
      model_type = model_type,
      sel_model = sel_model, 
      params = results_model[[sel_model]]$bestTune, 
      auc = results_auc[[sel_model]], 
      model = results_model[[sel_model]]
    )
  }
  
  preds <- best_model$model$pred %>% as.data.table
  if( nrow(preds) > 0){
    best_params <- best_model$params %>% as.list
    for( i in 1:len(best_params) ){
      param_ <- names(best_params)[i]
      value_ <- best_params[[i]]
      preds <- preds[get(param_) == value_]  
    }
    best_model$model$pred <- preds
  }
  return(best_model)
}

cross_fitting <- function(dt_0, best_model, outcome_name, outcome_value, 
  prediction_name='propensity', split_vars=NULL){
  dt_1 <- dt_0 %>% copy
  
  split_cat <- dt_1[, .SD, .SDcols = c(outcome_name, split_vars)]
  new_cat <- split_cat %>% 
    .[, 
      .(new_cat = paste(.SD, collapse="")), 
      1:nrow(split_cat)] %>% 
    .[, new_cat]
  folds <- createFolds(new_cat, k=5)
  
  for( test in folds){
    test_data <- dt_1[test]
    train_data <- dt_1[-test]
    tuneGrid <- best_model$params %>% as.data.frame()
    
    fold_model <- train_model(train_data, outcome_name = outcome_name,
      only_one = TRUE, tuneGrid = tuneGrid,
      models = best_model$sel_model)
    
    if( class(dt_1[, get(outcome_name)]) == 'numeric' ){
      prediction <- predict(fold_model$model, test_data)
    }else{
      prediction <- predict(fold_model$model, test_data, type='prob')[[outcome_value]]  
    }
    dt_0[test, c(prediction_name) := prediction]
  }
  
  return(dt_0)
}

cross_fitting_t_learner <- function(dt_0, best_model_1, best_model_2, outcome_name,
  prediction_name='propensity', treatment_name, outcome_value){
  dt_1 <- dt_0 %>% copy
  folds <- createFolds(dt_0[, get(treatment_name)], k=5)
  
  ITEs <- c()
  for( test in folds){
    test_data <- dt_1[test]
    train_data <- dt_1[-test]

    tuneGrid_1 <- best_model_1$params %>% as.data.frame()
    train_data %>% 
      .[get(treatment_name)==outcome_value] %>% 
      .[, c(treatment_name):=NULL] %>% 
      train_model(
        outcome_name = outcome_name,
        only_one = TRUE, tuneGrid = tuneGrid_1,
        models = best_model_1$sel_model) ->
      fold_model_1
    prediction_1 <- predict(fold_model_1$model, test_data)
    
    tuneGrid_2 <- best_model_2$params %>% as.data.frame()
    train_data %>% 
      .[get(treatment_name)!=outcome_value] %>% 
      .[, c(treatment_name):=NULL] %>% 
      train_model(
        outcome_name = outcome_name,
        only_one = TRUE, tuneGrid = tuneGrid_2,
        models = best_model_2$sel_model) ->
      fold_model_2
    prediction_2 <- predict(fold_model_2$model, test_data)
    
    ITEs <- c(ITEs, prediction_1 - prediction_2)
  }
  
  return(ITEs)
}

