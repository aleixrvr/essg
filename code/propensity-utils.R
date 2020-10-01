library(ggplot2)
library(zeallot)
library(magrittr)

source('code/basic.R')
source('code/train.R')
source('code/utils.R')

explore_vars <- function(sel_data, treatment_name) {
  
  outcome <- sel_data[[treatment_name]]
  var_names <- colnames(sel_data) %!in% treatment_name
  
  for( sel_var_name in  var_names){
    sel_var <- sel_data[[sel_var_name]] 
    htmltools::h2(sel_var_name) %>% print
    nas <- floor(1000*sum(is.na(sel_var))/length(sel_var))/10
    if( nas == 0 ){
      nas <- floor(1000*sum(sel_var == 'NA')/length(sel_var))/10
    }
    
    'Proportion of na: {nas}%' %>% f %>% cat
    cat('\n')
    if( class(sel_var) == 'character' ){
      kable(table(outcome, sel_var), format = "html", booktabs = TRUE) %>% 
        kable_styling() %>% 
        print
    }else{
      data.frame(outcome, sel_var) %>% 
        ggplot(aes(sel_var, outcome)) +
        ggtitle(sel_var_name) +
        ylab(treatment_name) +
        geom_jitter(width = 0.05, height = 0.05) ->
        res_plot
      print(res_plot)
    }
    cat('\n\n')
  }
}

run_propensity <- function(sel_data, treatment_name, k_fold=5, tuneLength=5, models=c('lm', 'boosting', 'elastic')){
  sel_data %>% 
    na.omit -> clean_data
  
  clean_data %>% 
    train_model(treatment_name, k_fold, tuneLength, models) ->
    best_model 
  
  propensity <- predict(best_model$model, clean_data, type='prob')$Yes
  clean_data[, Propensity := propensity]
  
  best_model$plot <- clean_data %>% 
    ggplot(aes_string('propensity', fill=treatment_name, color=treatment_name)) +
    geom_density(alpha = 0.1) +
    xlim(c(0, 1))
  
  best_model$clean_data <- clean_data
  
  return(best_model)
}

calc_ate <- function(
  data_, outcome, treatment_name, predictive_variates, first_visit=FALSE, 
  tuneLenghtATE, modelsATE, incremental=TRUE, is_classification
){
  if( incremental == TRUE ){
    base_outcome <- get_base_outcome(outcome, first_visit)
  }else{
    base_outcome <- outcome
  }
  
  sel_vars <- c(outcome, base_outcome, predictive_variates) %>% unique
  data_ %>%
    .[, .SD, .SDcols=sel_vars] %>%
    na.omit %>% 
    remove_constant_cols ->
    dt
  
  if( incremental == TRUE ){
    diff_outcome <- dt[, ..outcome] - dt[, ..base_outcome]
    dt[, diff_outcome:= diff_outcome]
    dt[, c(outcome):=NULL]
    dt[, c(base_outcome):=NULL]
  }else{
    setnames(dt, outcome, 'diff_outcome') 
  }
  
  dt %>%
    train_model('diff_outcome', tuneLength = tuneLenghtATE, models=modelsATE) ->
    best_model
  
  dt_y <- copy(dt)
  dt_y[[treatment_name]] ='Yes'
  if( is_classification == TRUE ){
    pred_y <- predict(best_model$model, dt_y, type='prob')$Yes
  }else{
    pred_y <- predict(best_model$model, dt_y)
  }
  
  dt_n <- copy(dt)
  dt_n[[treatment_name]] ='No'
  if( is_classification == TRUE ){
    pred_n <- predict(best_model$model, dt_n, type='prob')$Yes
  }else{
    pred_n <- predict(best_model$model, dt_n)
  }
  
  ate <- mean(pred_y - pred_n)
  
  if( is_classification == TRUE ){
    distribution <- c(Proportion=mean(dt[['diff_outcome']]=='Yes'))
  }else{
    distribution  <-  quantile(dt[['diff_outcome']], na.rm=TRUE)
  }
  
  return(list(
    distribution,
    best_model = best_model,
    ate=ate
  ))
}

print_ates <- function(outcome, results_outcomes, is_classification){
  "Outcome: {outcome}" %>% f %>% print
  "Distribution:" %>% f %>% print
  results_outcomes[[outcome]]$distribution %>% print
  "Model Type: {results_outcomes[[outcome]]$best_model$sel_model} \n" %>% f %>% print
  if( is_classification == TRUE ){
    "Accuracy: {results_outcomes[[outcome]]$best_model$accuracy} \n" %>% f %>% print
  }else{
    "RMSE: {results_outcomes[[outcome]]$best_model$rmse} \n" %>% f %>% print
  }
  "Params: {as.yaml(results_outcomes[[outcome]]$best_model$params)}" %>% f %>% print
  "ATE (Yes-No): {results_outcomes[[outcome]]$ate} \n" %>% f %>% print
  "\n\n" %>% f %>% print
  return(invisible())
}
