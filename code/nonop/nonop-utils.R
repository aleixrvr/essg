library(readxl)
library(data.table)
library(magrittr)
library(yaml)


source('code/basic.R')
source('code/utils.R')

get_data <- function(){
  
  xls_path <- 'data/ESSG extraction July 2020_3.xlsx'
  # excel_sheets(xls_path)
  
  matching_vars <- read_yaml('code/nonop/matching_vars.yml')
  
  clinical_data <- read_excel(xls_path) %>% 
    data.table() 
  
  all_vars <- matching_vars %>% unlist
  
  sel_data <- clinical_data %>% 
    .[Study == 'NonOp'] %>% 
    .[, opnonop_categoric:='No'] %>% 
    .[!is.na(`Op/NonOp`), opnonop_categoric:='Yes'] %>% 
    .[, .SD, .SDcols = c(all_vars, 'opnonop_categoric')] 
  
  # cols_to_remove <- sel_data[, sapply(.SD, function(col) uniqueN(col) == 1)]
  # print('Columns removed because constant value')
  # print(names(cols_to_remove)[cols_to_remove])
  
  matching_vars$covariates <- c(matching_vars$covariates, 'opnonop_categoric')
  
  return(list(
    sel_data,
    matching_vars
  ))
}
