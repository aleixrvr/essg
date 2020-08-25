library(ggplot2)

source('code/basic.R')

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

run_logistic_propensity <- function(sel_data, outcome_name){
  sel_data %>% 
    na.omit ->
    sel_data_reg
  
  log_formula <- "{outcome_name} ~ ." %>% f %>% as.formula
  log_model <- glm(log_formula, data=sel_data_reg, family='binomial') 
  sel_data_reg$predictions <- predict(log_model, sel_data_reg, type='response')
  
  outcome <- sel_data_reg[[outcome_name]]
  sel_data_reg[, outcome:=as.factor(outcome)]
  
  sel_data_reg %>% 
    ggplot(aes(predictions, fill=outcome, color=outcome)) +
    geom_density(alpha = 0.1) +
    xlim(c(0, 1)) 
}
