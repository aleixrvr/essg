library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(stringr)
library(zeallot)

source('code/basic.R')
source('code/utils.R')
source('code/train.R')

XLS_PATH <- 'data/ESSG extraction July 2020_3.xlsx'

# outcome <- 'had_complication'
# k_fold <- 10
# tuneLength <- 2
# models <- c('elastic')
# first_visit=TRUE
# increment=FALSE
# aa <- create_model(
#   outcome, first_visit, increment, k_fold, tuneLength, models
# )

create_model <- function(
  outcome, first_visit=TRUE, increment=FALSE, k_fold, tuneLength, models
){

  c(data_set, predictive_vars, outcome) %<-% get_data(outcome, first_visit, increment)
  c(train_data, validation_data) %<-% create_validation_set(data_set)
  
  best_model <- train_model(train_data, outcome, k_fold, tuneLength, models)
  
  if( best_model$model_type == 'regression' ){
    best_model$distribution <- data_set[, get(outcome)] %>% quantile
  }else{
    best_model$distribution <- data_set[, get(outcome)] %>% table %>% prop.table()
  }
  validation_results <- eval_validation(validation_data, best_model)
  
  return(list(
    best_model=best_model, predictive_vars=predictive_vars, 
    validation_results=validation_results
  ))
}

get_accuracy <- function(best_model){
  if( best_model$model_type == 'regression' ){
    text <- 'RMSE'
    acc <- best_model$rmse
  }else{
    text <- 'Accuracy'
    acc <- best_model$accuracy
  }
  return(list(acc=acc, text=text))
}

show_stats <- function(best_model, predictive_vars, validation_results, show_vars=TRUE){

  "{outcome}" %>% f %>% print
  "{get_accuracy(best_model)$text}: {get_accuracy(best_model)$acc}" %>% f %>% print
  "{outcome} distribution:" %>% f %>% print
  best_model$distribution %>% print
  "Model Type: {best_model$sel_model}" %>% f %>% print
  "Params: {as.yaml(best_model$params)}" %>% f %>% print
  
  if(show_vars){
    cat('\nPredictive Variables')
    cat(predictive_vars$predictive %>% as.yaml)
  }
  
  return(invisible())
}

get_data <- function(outcome='', first_visit=TRUE, increment=FALSE){
  
  clinical_data <- read_excel(XLS_PATH) %>% 
    data.table
  
  reinterventions <- read_excel(XLS_PATH, sheet = "Rev surgeries") %>%
    data.table %>%
    .[, .(reinterventions=.N*1.0), `Code of the patient`]
  
  complication_patients <- read_excel(XLS_PATH, sheet='Complications') %>%
    as.data.table %>%
    .[`Complication Type`=='Primary'] %>%
    .[`Reoperation Due to Complication`=='Yes'] %>% 
    .[, unique(`Code of the patient`)]
  
  clinical_data %<>% 
    .[, had_complication := 'No'] %>% 
    .[`Code of the patient` %in% complication_patients, had_complication := 'Yes'] %>%
    merge(reinterventions, by="Code of the patient", all.x=TRUE, all.y=FALSE)
  
  # predictive_vars <- read_yaml('code/five_years/predictive_vars_{outcome}.yml')
  predictive_vars <- read_yaml('code/five_years/predictive_vars.yaml')  
  
  if( increment ){
    outcome_base <- get_base_outcome(outcome, first_visit)
    clinical_data[, increment := get(outcome) - get(outcome_base)]
    outcome <- 'increment'
  }
  
  clinical_data %>% 
    clean_data(predictive_vars$predictive) %>% 
    .[, .SD, .SDcols=c(outcome, predictive_vars$predictive)] %>% 
    remove_constant_cols %>% 
    na.omit ->
    data_set
  
  return(list(
    data_set,
    predictive_vars,
    outcome
  ))
}

create_validation_set <- function(data_set){
  inds <- sample(1:nrow(data_set), floor(nrow(data_set)*.8), replace=FALSE)
  train_data <- data_set[inds] %>% remove_constant_cols()
  cols_ <- train_data %>% colnames
  validation_data <- data_set[-inds, .SD, .SDcols=cols_] 
  
  return(list(train_data, validation_data))
}

eval_validation <- function(validation_data, best_model){
  
}
