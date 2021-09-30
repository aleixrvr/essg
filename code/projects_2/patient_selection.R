library(readxl)
library(data.table)
library(magrittr)
library(yaml)

source('basic.R')

xls_path <- '../../data/ESSG extraction July 2021 DEF_v3.xlsx'


get_data <- function(){
  clinical_data <- read_excel(xls_path) %>% 
    as.data.table()
  
  analysis_vars <- read_yaml('descriptive.yml')
  discarded_patients <- readLines('discarded_patients')
  
  setnames(clinical_data, 'Major curve Cobb angle', 'Static Major curve Cobb angle')
  setnames(clinical_data, 'ODI - Score (%)_First Visit', 'ODI - Score (%)')
  setnames(clinical_data, 'SRS22 - Function_First Visit', 'SRS22 - Function / Activity')
  setnames(clinical_data, 'SRS22 - Pain_First Visit', 'SRS22 - Pain')
  setnames(clinical_data, 'SRS22 -SI_First Visit', 'SRS22 - Self image / Appearance')
  setnames(clinical_data, 'SRS22 - MH_First Visit', 'SRS22 - Mental health')
  setnames(clinical_data, 'SRS22 - SRS Subtotal score_First Visit', 'SRS22 - SRS Subtotal score')
  setnames(clinical_data, 'SF36 - PCS_First Visit', 'SF36 - PCS')
  setnames(clinical_data, 'SF36 - MCS_First Visit', 'SF36 - MCS') 
  
  uiv_ <- c('T10', 'T11', 'T12', 'L1')
  clinical_data %>% 
    .[, upper:= substr(`Posterior Instrumented Fusion: Upper / Lower Levels`, 1, 2)] %>% 
    .[, uiv_t10_12_l1 := ifelse(upper %in% uiv_, 'Yes', 'No')]
  analysis_vars$surgery <- c(analysis_vars$surgery, 'uiv_t10_12_l1')
  
  clinical_data %>% 
    .[grepl('Ex', `Tobacco use_First Visit`, fixed=TRUE),
      `Tobacco use_First Visit` := 'Ex'] %>% 
    .[grepl('Current', `Tobacco use_First Visit`, fixed=TRUE), 
      `Tobacco use_First Visit` := 'Current'] 
  
  clinical_data %<>% 
    .[, followup_2y := 
        !is.na(`2 YEAR VISIT - Date of visit`) | 
        !is.na(`3 YEAR VISIT - Date of visit`)] %>% 
    .[, followup_5y := 
        !is.na(`5 YEAR VISIT - Date of visit`) | 
        !is.na(`6 YEAR VISIT - Date of visit`)]
  
  clinical_data[, `ASA classification` := as.character(`ASA classification`)]
  
  
  patients_with_complications_bf_6m <- get_patients_with_complications_bf_6m() %>% 
    unique
  clinical_data[, complications_bf_6m := 'No']
  clinical_data[
    `Code of the patient` %chin% patients_with_complications_bf_6m, 
    complications_bf_6m := 'Yes']
  
  lordosis <- c(
    '6W. Lordosis (top of L1-S1)', 
    '6M. Lordosis (top of L1-S1)',
    '1Y. Lordosis (top of L1-S1)',
    '2Y. Lordosis (top of L1-S1)'
  )
  
  clinical_data[, 
    diff_lordosis := max(.SD, na.rm=TRUE) - min(.SD, na.rm=TRUE), 
    1:nrow(clinical_data),
    .SDcols=lordosis]
  
  clinical_data %>% 
    .[followup_2y==TRUE] %>% 
    .[Study=='Op'] %>% 
    .[Site != 'ANK Op'] %>% 
    .[`Vital status` == 'Alive'] %>% 
    .[!(`Code of the patient` %in% discarded_patients)] %>% 
    .[`st1. Date of Stage` %>% as.Date() < as.Date('2015-09-01')] ->
    data_
  
  return(data_)
}

get_patients_with_complications_bf_6m <- function(){
  complication_data <- read_excel(xls_path, sheet = 'Complications') %>% 
    as.data.table()
  
  has_complication <- function(complications){
    complications %>% 
      tolower %>% 
      sapply(. %>% grepl('proximal junctional failure', ., fixed = TRUE)) ->
      ind_pjf
    
    complications %>% 
      tolower %>% 
      sapply(. %>% grepl('distal junctional failure', ., fixed = TRUE)) ->
      ind_djf
    
    complications %>% 
      tolower %>% 
      sapply(. %>% grepl('proximal junctional kyphosis', ., fixed = TRUE)) ->
      ind_pjk
    
    complications %>% 
      tolower %>% 
      sapply(. %>% grepl('distal junctional kyphosis', ., fixed = TRUE)) ->
      ind_djk
    
    res <- ind_pjf | ind_djf | ind_pjk | ind_djk
    return(res)
  }
  
  complication_data %>% 
    .[`Days since surgery` < 180] %>% 
    .[has_complication(`Name of the complication`)] %>% 
    .[, `Code of the patient`] ->
    patients_with_complications_bf_6m
  
  return(patients_with_complications_bf_6m)
}

stats_fun <- function(variable){
  if( class(variable) == 'numeric' ){
    return( list(
      mean = mean(variable, na.rm=TRUE),
      sd = sd(variable, na.rm = TRUE)
    ))
  }else{
    res_table <- table(variable)
    return(list(
      table = res_table, 
      proportion = prop.table(res_table)
    ))
  }
}

calc_p_vals <- function(dt, var_, treatment_, treatment_vals=c('Yes', 'No')){
  vals_n <- dt[, get(var_)] %>% uniqueN
  treat_1 <- treatment_vals[1]
  treat_2 <- treatment_vals[2]
  
  p.val <- NULL
  if(class(dt[, get(var_)]) == 'numeric'){
    p.val <- t.test(
      dt[get(treatment_)==treat_1][,get(var_)],
      dt[get(treatment_)==treat_2][, get(var_)]
    )$p.val
  }else if(vals_n == 2){
    t1 <- table(dt[get(treatment_)==treat_1][, get(var_)])
    t2 <- table(dt[get(treatment_)==treat_2][, get(var_)])
    
    if( dim(t1) == 2 & dim(t2) == 2){
      p.val <- prop.test(rbind(t1, t2))$p.val
    }
  }else{
    p.val <- NULL
  }
  
  return(p.val)
}
