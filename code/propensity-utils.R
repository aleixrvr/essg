library(ggplot2)
library(zeallot)
library(magrittr)

source('code/basic.R')
source('code/train.R')

explore_vars <- function(sel_data, outcome_name) {
  
  outcome <- sel_data[[outcome_name]]
  var_names <- colnames(sel_data) %!in% outcome_name
  
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
        ylab(outcome_name) +
        geom_jitter(width = 0.05, height = 0.05) ->
        res_plot
      print(res_plot)
    }
    cat('\n\n')
  }
}

run_propensity <- function(sel_data, outcome_name, k_fold=5, tuneLength=5, models=c('lm', 'boosting', 'elastic')){
  sel_data %>% 
    na.omit -> clean_data
  
  clean_data %>% 
    train_model(outcome_name, k_fold, tuneLength, models) ->
    best_model 
  
  propensity <- predict(best_model$model, clean_data, type='prob')$Yes
  clean_data[, Propensity := propensity]
  
  best_model$plot <- clean_data %>% 
    ggplot(aes_string('propensity', fill=outcome_name, color=outcome_name)) +
    geom_density(alpha = 0.1) +
    xlim(c(0, 1))
  
  best_model$clean_data <- clean_data
  
  return(best_model)
}

