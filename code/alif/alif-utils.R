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
  matching_vars$covariates %<>% c('Alif')
  all_vars <- matching_vars %>% unlist
  
  # setnames(clinical_data, '3CO', 'CO3')
 
  clinical_data %>% 
    .[ALIF + TLIF + PLIF > 0] %>% 
    .[, Alif := ifelse(ALIF > 0, 'Yes', 'No')] %>% 
    .[, .SD, .SDcols = all_vars] %>% 
    aggregate_data  %>% 
    clean_data(all_vars) ->
    sel_data
  
  # sel_data[, ll_lordosis_diff:=ideal_ll - lordosis_top_of_l1s1]
  # sel_data[, ideal_ll:=NULL]
  # sel_data[, lordosis_top_of_l1s1:=NULL]
  
  ideal_ll <- sel_data[['Ideal LL']]
  lordosis_top_of_l1s1 <- sel_data[['Lordosis (top of L1-S1)']]
  sel_data[, `LL-Lordosis Difference`:=ideal_ll - lordosis_top_of_l1s1]
  sel_data[, `Ideal LL`:=NULL]
  sel_data[, `Lordosis (top of L1-S1)`:=NULL]
  
  matching_vars$covariates %<>% c('LL-Lordosis Difference')
  matching_vars$covariates %!in% c('Ideal LL', 'Lordosis (top of L1-S1)') ->
    matching_vars$covariates
  
  return(list(
    sel_data,
    matching_vars
  ))
}

aggregate_data <- function(sel_data){

  sel_data %>% 
    .[Site=='ANK Op', Site:='ANKZUR Op'] %>% 
    .[Site=='ZUR Op', Site:='ANKZUR Op'] 
  
  sel_data %>% 
    .[substr(`Levels Previously operated - Lower`, 1, 1) == 'C', `Levels Previously operated - Lower`:='C'] %>% 
    .[substr(`Levels Previously operated - Lower`, 1, 1) == 'L', `Levels Previously operated - Lower`:='L'] %>% 
    .[substr(`Levels Previously operated - Lower`, 1, 1) == 'T', `Levels Previously operated - Lower`:='T'] %>% 
    .[substr(`Levels Previously operated - Lower`, 1, 1) == 'S', `Levels Previously operated - Lower`:='S']
  
  sel_data %>% 
    .[`1st surgeon: experience in ASD surgery` == 'Less than 2 years', 
      `1st surgeon: experience in ASD surgery`:='2-10 years']
  
  sel_data %>% 
    .[`ASA classification` == 4, `ASA classification`:=3]
  
  sel_data %>% 
    .[grepl('Current', `Tobacco use_First Visit`), `Tobacco use_First Visit` := 'Current'] %>% 
    .[grepl('Ex-User', `Tobacco use_First Visit`), `Tobacco use_First Visit` := 'Ex-User'] 
  
  return(sel_data)
}



