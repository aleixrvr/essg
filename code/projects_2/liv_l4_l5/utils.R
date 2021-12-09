library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)


print_model_results <- function(
  outcome, model_1, data_1, model_2, data_2){
  
  outcome_vals <- c(
    data_1[, get(outcome)],
    data_2[, get(outcome)]
  )
  
  total_obs <- data_1[, .N] + data_2[, .N]
  rmse <- model_1$rmse * data_1[, .N] + model_2$rmse * data_2[, .N] 
  rmse <- rmse / total_obs
  rmse <- rmse / sqrt(total_obs)
  relative_rmse <- rmse / sd(outcome_vals)
  relative_rmse <- floor(relative_rmse*1000)/10
  
  'Outcome: {outcome}' %>% f %>% print
  "RMSE: {rmse}" %>% f %>% print
  "Relative RMSE: {relative_rmse} %" %>% f %>% print
}


print_ates <- function(outcome, ITEs){
  
  'ATE (short - long): {mean(ITEs)}' %>% f %>% print
  'ATE (std Error) {sd(ITEs)/sqrt(len(ITEs))}' %>% f %>% print
  p.val <- t.test(ITEs, alternative = 'two.sided')$p.val
  't.test p-value {p.val}' %>% f %>% print
  '\n\n' %>% cat
  return(invisible())
}

stats_fun_var <- function(dt, var_, treatment_name){
  dt[, .(treatment=get(treatment_name), outcome=get(var_))] %>% 
    ggplot(aes(treatment, outcome)) +
    ggtitle(var_) +
    xlab(treatment_name) +
    geom_jitter(width = 0.05, height = 0.05) ->
    res_plot
  print(res_plot)
  
  if( class(dt[, get(var_)]) == 'numeric' ){
    res_global <- dt[, .(
      mean = mean(get(var_), na.rm=TRUE),
      sd = sd(get(var_), na.rm = TRUE)
    )]
    res_treatment <- dt[, .(
      mean = mean(get(var_), na.rm=TRUE),
      sd = sd(get(var_), na.rm = TRUE)
    ), treatment_name]
    
    return( list(
      global = res_global,
      treatment = res_treatment
    ))
  }else{
    res_table <- table(dt[, get(var_)])
    return(list(
      table = res_table, 
      proportion = prop.table(res_table)
    ))
  }
}

clean_na <- function(sel_data, matching_vars){
  for( sel_var_name in matching_vars ){
    sel_var <- sel_data[[sel_var_name]]
    if( class(sel_var) == 'character' ){
      na_rows <- sel_var %>% is.na
      sel_data[na_rows, c(sel_var_name):='NA']
    }
  }  
  
  return(sel_data)
}


first_letter <- function(text){
  starts_letter <- substr(text, 1, 1) %>% as.numeric %>% is.na
  if( starts_letter == FALSE ){
    text <- substr(text, 2, nchar(text)) %>% 
      paste0('_', substr(text, 1, 1))
  }
  return(text)
}


. %>%
  str_replace_all('/' %>% fixed, '') %>%
  str_replace_all('(' %>% fixed, '') %>%
  str_replace_all(')' %>% fixed, '') %>%
  str_replace_all('-' %>% fixed, '') %>%
  str_replace_all('%' %>% fixed, '') %>%
  str_replace_all(':' %>% fixed, '') %>%
  str_replace_all('.' %>% fixed, '') %>%
  str_replace_all('+' %>% fixed, '_') %>%
  str_to_lower() %>% 
  str_replace_all(' ' %>% fixed, '_') %>% 
  first_letter %>% 
  str_replace_all('[ ]{2,}', '_') %>%
  str_replace_all('[_]{2,}', '_')  ->
  clean_name


clean_names <- function(sel_names){
  sel_names %>% 
    sapply(clean_name)
}

clean_names_dt <- function(sel_data){
  
  for(var_name in colnames(sel_data)){
    setnames(sel_data, var_name, var_name %>% clean_name)
  }
  
  return(sel_data)
}

clean_data <- function(sel_data, matching_vars){
  sel_data %>% 
    clean_na(matching_vars) 
  # clean_names_dt 
}

remove_constant_cols <- function(dt){
  cols <- colnames(dt)
  unique_val <- cols[dt[, sapply(.SD, uniqueN)] == 1]
  cols_ <- cols %!in% unique_val
  dt[, .SD, .SDcols=cols_]
}

check_matching_names <- function(sel_data, matching_vars){
  sel_data_names <- names(sel_data)
  for( var in matching_vars ){
    trobada <- which(sel_data_names == var)
    if( length(trobada) == 0 ){
      trobada <- 0
    }
    "{var}: {trobada}" %>% f %>% print
  }
  
  # sel_data[grep('3CO', clinical_names)]
}

get_base_outcome <- function(outcome, first_visit=FALSE){
  base_outcome <- substr(outcome, 5, nchar(outcome))
  if( first_visit == TRUE ){
    base_outcome <- "{base_outcome}_First Visit" %>% f
  }
  return(base_outcome)
}


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