library(readxl)
library(openxlsx)
library(data.table)
library(magrittr)
library(yaml)
library(stringr)
library(zeallot)

source('basic.R')
source('utils.R')


get_data <- function(evaluation=FALSE, correction_approach=FALSE){
  # xls_path <- 'data/ESSG extraction July 2020_3.xlsx'
  xls_path <- '../../data/ESSG extraction December 2020 - DEF.xlsx'
  # excel_sheets(xls_path)
  
  # clinical_data <- read_xlsx(xls_path) %>% 
  #   as.data.table()
  
  clinical_data <- read.xlsx(
    xls_path, sep.names = " ", detectDates = TRUE
  ) %>% 
    as.data.table()
  
  
  for(colname in colnames(clinical_data)){
    clean_name <- gsub('\r\n\r\n', '', colname, fixed=TRUE)
    setnames(clinical_data, colname, clean_name)
  }
  
  if(evaluation==TRUE){
    matching_vars <- read_yaml('alif/matching_vars_evaluation.yml')
  }else{
    matching_vars <- read_yaml('alif/matching_vars.yml')  
  }
  
  matching_vars$covariates %<>% c('Alif')
  outcomes_ql_index <- matching_vars$outcomes_ql %>% get_base_outcome(first_visit=TRUE)
  outcome_radiology_index <- matching_vars$outcomes_radiology %>% get_base_outcome(first_visit=FALSE)
  all_vars <- c(matching_vars %>% unlist, outcomes_ql_index, outcome_radiology_index) %>% unique
  
  valid_patients <- clinical_data %>% 
    .[`st1. Date of Stage 1` %>% as.Date() < as.Date('2018-12-15')] %>% 
    .[!is.na(`2 YEAR VISIT - Date of visit`) | !is.na(`3 YEAR VISIT - Date of visit`)] %>% 
    .[, `Code of the patient` %>% unique]
  
  clinical_data %<>% .[`Code of the patient` %in% valid_patients]
  
  if( correction_approach == TRUE){
    all_vars <- c(all_vars, 'Surgical Approach')
  }
 
  clinical_data %>% 
    .[ALIF + TLIF + PLIF > 0] %>% 
    .[`Posterior Instrumented Fusion`!='No'] %>% 
    .[, Alif := ifelse(ALIF > 0, 'Yes', 'No')] %>% 
    .[, .SD, .SDcols = all_vars] %>% 
    # .[ RSA < 60 ] %>% 
    # .[ RLL > -80 ] %>% 
    # .[RPV > -50 ] %>% 
    aggregate_data  %>% 
    clean_data(all_vars) ->
    sel_data
  
  if( correction_approach == TRUE ){
    sel_data %>% 
      .[`Surgical Approach` == 'Anterior-Posterior', `Surgical Approach` := 'Correction'] %>% 
      .[`Surgical Approach` == 'Anterior', `Surgical Approach` := 'Correction'] %>% 
      .[, Alif := ifelse(`Surgical Approach` =='Correction', 'Yes', 'No')]
  }
  
  ideal_ll <- sel_data[['ideal LL']]
  lordosis_top_of_l1s1 <- sel_data[['Lordosis (top of L1-S1)']]
  sel_data[, `LL-Lordosis Difference`:=ideal_ll - lordosis_top_of_l1s1]
  # sel_data[, `Ideal LL`:=NULL]
  # sel_data[, `Lordosis (top of L1-S1)`:=NULL]
  
  matching_vars$covariates %<>% c('LL-Lordosis Difference')
  matching_vars$covariates %!in% c('ideal LL', 'Lordosis (top of L1-S1)') ->
    matching_vars$covariates
  
  matching_vars$expanded <- c(
    matching_vars$covariates, 
    matching_vars$predictive,
    outcomes_ql_index,
    outcome_radiology_index
  ) %>% unique
  
  return(list(
    sel_data,
    matching_vars
  ))
}

aggregate_data <- function(sel_data){

  # sel_data %>% 
  #   .[Site=='ANK Op', Site:='ANKZUR Op'] %>% 
  #   .[Site=='ZUR Op', Site:='ANKZUR Op'] 
  
  if( "Cobb LS curve (Degree)" %in% colnames(sel_data)){
    sel_data[is.na(`Cobb LS curve (Degree)`), `Cobb LS curve (Degree)`:= 0] 
  }
  
  if( "Previous surgery - LEV" %in% colnames(sel_data)){
    sel_data %>%
      .[substr(`Previous surgery - LEV`, 1, 1) == 'C', `Previous surgery - LEV`:='C'] %>%
      .[substr(`Previous surgery - LEV`, 1, 1) == 'L', `Previous surgery - LEV`:='L'] %>%
      .[substr(`Previous surgery - LEV`, 1, 1) == 'T', `Previous surgery - LEV`:='T'] %>%
      .[substr(`Previous surgery - LEV`, 1, 1) == 'S', `Previous surgery - LEV`:='S']
  }
  
  if( "1st surgeon: experience in ASD surgery" %in% colnames(sel_data)){
    sel_data %>% 
      .[`1st surgeon: experience in ASD surgery` == 'Less than 2 years', 
        `1st surgeon: experience in ASD surgery`:='2-10 years']
  }
  
  sel_data %>% 
    .[`ASA classification` == 4, `ASA classification`:=3]
  
  sel_data %>% 
    .[grepl('Current', `Tobacco use_First Visit`), `Tobacco use_First Visit` := 'Current'] %>% 
    .[grepl('Ex-User', `Tobacco use_First Visit`), `Tobacco use_First Visit` := 'Ex-User'] 
  
  # fusion_names <- function(posterior_name){
  #   if(is.na(posterior_name)) return(posterior_name)
  #   
  #   posterior_name %>% str_split('-') %>% .[[1]] %>% 
  #     str_replace_all('[:digit:]', '') %>% 
  #     unique %>% 
  #     paste(collapse = '-')
  # }
  fusion_names <- function(posterior_name){
    if(is.na(posterior_name)) return(posterior_name)
    
    posterior_name %>% str_split('-') %>% .[[1]] %>% .[len(.)] %>% 
      str_replace_all('[:digit:]', '') -> endpoint
    
    ifelse(endpoint %in% c('Iliac', 'S'), 'Iliac+S', endpoint)
  }
  
  
  var_name <- 'Posterior Instrumented Fusion: Upper / Lower Levels'
  sel_data[, c(var_name):=fusion_names(get(var_name)), 1:nrow(sel_data)]
  
  return(sel_data)
}



