library(data.table)
library(magrittr)
library(stringr)
source('code/basic.R')


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


. %>%
  str_replace_all('/' %>% fixed, '') %>%
  str_replace_all('(' %>% fixed, '') %>%
  str_replace_all(')' %>% fixed, '') %>%
  str_replace_all('-' %>% fixed, '') %>%
  str_replace_all('%' %>% fixed, '') %>%
  str_replace_all(':' %>% fixed, '') %>%
  str_replace_all('+' %>% fixed, '_') %>%
  str_to_lower() %>% 
  str_replace_all(' ' %>% fixed, '_') %>% 
  str_replace_all('[ ]{2,}', '_') %>%
  str_replace_all('[_]{2,}', '_') ->
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
