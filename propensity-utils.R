# source('basic.R')
library(glue)
f <- glue

explore_vars <- function(matching_vars, sel_data, outcome) {
  for( sel_var_name in matching_vars ){
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