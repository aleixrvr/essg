library(readxl)
library(data.table)
library(magrittr)
library(yaml)


source('code/basic.R')
source('code/utils.R')

get_data <- function(){
  xls_path <- 'data/ESSG extraction July 2020_2.xlsx'
  # excel_sheets(xls_path)
  
  clinical_data <- read_excel(xls_path) %>% 
    as.data.table()
  
  matching_vars <- read_yaml('code/alif/matching_vars.yml')
  
  setnames(clinical_data, '3CO', 'CO3')
  setnames(clinical_data, '1st surgeon: experience in ASD surgery', 'first_surgeon')
  setnames(clinical_data, 'Levels Previously operated - Lower', 'Prev_op_low')
 
  clinical_data %>% 
    .[ALIF + TLIF + PLIF > 0] %>% 
    .[, alif := ifelse(ALIF > 0, 'Yes', 'No')] %>% 
    .[, .SD, .SDcols = c('alif', matching_vars)] %>% 
    aggregate_data  %>% 
    clean_data(matching_vars) ->
    sel_data
  
  sel_data[, ll_lordosis_diff:=ideal_ll - lordosis_top_of_l1s1]
  sel_data[, ideal_ll:=NULL]
  sel_data[, lordosis_top_of_l1s1:=NULL]
  
  return(sel_data)
}

aggregate_data <- function(sel_data){

  
  sel_data %>% 
    .[Site=='ANK Op', Site:='ANKZUR Op'] %>% 
    .[Site=='ZUR Op', Site:='ANKZUR Op'] 
  
  sel_data %>% 
    .[substr(Prev_op_low, 1, 1) == 'C', Prev_op_low:='C'] %>% 
    .[substr(Prev_op_low, 1, 1) == 'L', Prev_op_low:='L'] %>% 
    .[substr(Prev_op_low, 1, 1) == 'T', Prev_op_low:='T'] %>% 
    .[substr(Prev_op_low, 1, 1) == 'S', Prev_op_low:='S']
  
  sel_data %>% 
    .[first_surgeon == 'Less than 2 years', first_surgeon:='2-10 years']
  
  sel_data %>% 
    .[`ASA classification` == 4, `ASA classification`:=3]
  
  sel_data %>% 
    .[grepl('Current', `Tobacco use_First Visit`), `Tobacco use_First Visit` := 'Current'] %>% 
    .[grepl('Ex-User', `Tobacco use_First Visit`), `Tobacco use_First Visit` := 'Ex-User'] 
  
  return(sel_data)
}



