library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(stringr)

source('code/basic.R')
source('code/utils.R')

get_data <- function(){
  complications <- fread('data/Fenestrated screws/Complications tots.csv') 
  dps <- fread('data/Fenestrated screws/DPS ops.csv')
  
  setnames(dps, '3CO', 'CO3')
  
  matching_vars <- read_yaml('code/fenestrated/matching_vars.yml')
  
  dps %>% 
    .[, fenestrated:=`Fenestrated screws with or without cement`] %>% 
    .[, `Fenestrated screws with or without cement`:=NULL] %>% 
    .[, .SD, .SDcols = c('fenestrated', matching_vars)] %>% 
    clean_data(matching_vars) ->
    sel_data

  sel_data %<>% .[, 
    global_tilt:= global_tilt %>% 
      str_replace_all(',' %>% fixed, '.') %>% 
      as.numeric
    ] %>% 
    .[ age > 50]
    
  return(sel_data)
}

