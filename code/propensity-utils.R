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
        geom_jitter(width = 0.05, height = 0.05) ->
        res_plot
      print(res_plot)
    }
    cat('\n\n')
  }
}

run_propensity <- function(data_sel, outcome_name, ...){
  data_sel %>% 
    na.omit %>% 
    train_model(outcome = outcome_name, ...) ->
    results_model 
  
  c(sel_model, params, accuracy, model) %<-% select_best_model(results_model)
  
  propensity <- predict(model, data_sel, type='prob')[, 1]
  
  data.frame(
    propensity,
    outcome=data_sel %>% na.omit %>% .[[outcome_name]]
  ) %>% 
    ggplot(aes(propensity, fill=outcome, color=outcome)) +
    geom_density(alpha = 0.1) +
    xlim(c(0, 1)) 
}
