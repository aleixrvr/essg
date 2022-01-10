library(data.table)
library(magrittr)
library(stringr)
source('code/projects_1/basic.R')


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
