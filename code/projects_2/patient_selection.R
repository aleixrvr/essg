library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(lme4)
library(lmerTest)

source('basic.R')

xls_path <- '../../data/ESSG extraction July 2021 DEF_v3.xlsx'

get_data_base <- function(){
  
  discarded_patients <- readLines('discarded_patients')
  
  clinical_data <- read_excel(xls_path) %>% 
    as.data.table() %>% 
    .[!(`Code of the patient` %in% discarded_patients)] 
  
  
  setnames(clinical_data, 'Major curve Cobb angle', 'Static Major curve Cobb angle')
  setnames(clinical_data, 'ODI - Score (%)_First Visit', 'ODI - Score (%)')
  setnames(clinical_data, 'SRS22 - Function_First Visit', 'SRS22 - Function / Activity')
  setnames(clinical_data, 'SRS22 - Pain_First Visit', 'SRS22 - Pain')
  setnames(clinical_data, 'SRS22 -SI_First Visit', 'SRS22 - Self image / Appearance')
  setnames(clinical_data, 'SRS22 - MH_First Visit', 'SRS22 - Mental health')
  setnames(clinical_data, 'SRS22 - SRS Subtotal score_First Visit', 'SRS22 - SRS Subtotal score')
  setnames(clinical_data, 'SF36 - PCS_First Visit', 'SF36 - PCS')
  setnames(clinical_data, 'SF36 - MCS_First Visit', 'SF36 - MCS') 
  
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
  
  return(clinical_data)
}

get_data <- function(){
  clinical_data <- get_data_base()
  
  analysis_vars <- read_yaml('descriptive.yml')
  
  uiv_ <- c('T10', 'T11', 'T12', 'L1')
  clinical_data %>% 
    .[, upper:= substr(`Posterior Instrumented Fusion: Upper / Lower Levels`, 1, 2)] %>% 
    .[, uiv_t10_12_l1 := ifelse(upper %in% uiv_, 'Yes', 'No')]
  analysis_vars$surgery <- c(analysis_vars$surgery, 'uiv_t10_12_l1')
  
  patients_with_complications_bf_6m <- get_patients_with_complications_bf_6m() %>% 
    unique
  clinical_data[, complications_bf_6m := 'No']
  clinical_data[
    `Code of the patient` %chin% patients_with_complications_bf_6m, 
    complications_bf_6m := 'Yes']
  
  patients_with_complications_ever <- get_patients_with_complications() %>% 
    unique
  clinical_data[, complications_ever := 'No']
  clinical_data[
    `Code of the patient` %chin% patients_with_complications_ever, 
    complications_ever := 'Yes']
  
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
  
  data_[, `Posterior Instrumented Fusion: Upper / Lower Levels`] %>% 
    sapply(. %>% strsplit('-') %>% .[[1]] %>% .[1])  ->
    post_intr_fusion
  
  data_[, 
    Non_instrumented_segments := sapply(post_intr_fusion, 
      calc_number_non_instrumented_segments)]
  
  data_[, essg_category := 'others']
  data_[`ESSG Diagnosis` == 'Degenerative', 
    essg_category := 'degenerative']
  data_[`ESSG Diagnosis` == 'Failed-back', 
    essg_category := 'degenerative']
  data_[`ESSG Diagnosis` == 'Idiopathic', 
    essg_category := 'idiopathic']
  
  return(data_)
}

calc_number_non_instrumented_segments <- function(post_intr_fusion_){
  if( is.na(post_intr_fusion_) ) return(NA)
  pos_ <- substr(post_intr_fusion_, 1, 1)
  num_ <- substr(post_intr_fusion_, 2, nchar(post_intr_fusion_)) %>% as.numeric
  if( pos_ == 'L'){
    num_ <- 11 + num_
  }else if(pos_ == 'T'){
    num_ <- num_ - 1
  }else{
    num_ <- NA
  }
  return(num_)
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

get_patients_with_complications <- function(){
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
    .[has_complication(`Name of the complication`)] %>% 
    .[, `Code of the patient`] ->
    patients_with_complications
  
  return(patients_with_complications)
}

stats_fun <- function(variable){
  if( class(variable) == 'numeric' ){
    # return( list(
    #   mean = mean(variable, na.rm=TRUE),
    #   sd = sd(variable, na.rm = TRUE)
    # ))
    return( list(
      mean = mean(variable, na.rm=TRUE) %>% round(2)
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

calc_complications <- function(data_, treatment_vals){
  
  complication_data <- read_excel(xls_path, sheet = 'Complications') %>% 
    as.data.table() %>% 
    .[`Days since surgery` < 5*365] 
  
  patient_groups <- list()
  patient_groups$all <- data_[, `Code of the patient` %>% unique]
  patient_groups[[treatment_vals[1]]] <- data_ %>% 
    .[get(treatment_) == treatment_vals[1], `Code of the patient` %>% unique]
  patient_groups[[treatment_vals[2]]] <- data_ %>% 
    .[get(treatment_) == treatment_vals[2], `Code of the patient` %>% unique]
  
  
  number_reiqs <- list()
  impacts <- list()
  categories <- list()
  for( name_ in names(patient_groups) ){
    complication_data %>% 
      .[`Code of the patient` %chin% patient_groups[[name_]]] ->
      comp_
    
    number_reiqs[[name_]] <- data.table(
      group=name_, 
      reiqs = comp_[`Reoperation Due to Complication` =='Yes', .N],
      patients = comp_[`Reoperation Due to Complication` =='Yes', uniqueN(`Code of the patient`)]
    )
    
    impact_name <- paste(name_, 'num', sep='_')
    imp_num <- comp_[, .(impact_name = .N), `Complication Impact`] %>% 
      setnames('impact_name', impact_name)
    impact_name <- paste(name_, 'patients', sep='_')
    imp_patients <- comp_[,
      .(impact_name = uniqueN(`Code of the patient`)), `Complication Impact`] %>% 
      setnames('impact_name', impact_name)
    impacts[[name_]] <- merge(imp_num, imp_patients)
    
    cat_name <- paste(name_, 'num', sep='_')
    cat_num <- comp_[, .(cat_name = .N), `Category of the complication`] %>% 
      setnames('cat_name', cat_name)
    cat_name <- paste(name_, 'patients', sep='_')
    cat_patients <- comp_[, 
      .(cat_name = uniqueN(`Code of the patient`)), `Category of the complication`] %>% 
      setnames('cat_name', cat_name)
    categories[[name_]] <- merge(cat_num, cat_patients)
  }
  
  number_reiqs <- do.call(rbind, number_reiqs)
  impacts <- impacts[[1]] %>%
    merge(impacts[[2]], all=TRUE ) %>% 
    merge(impacts[[3]], all=TRUE) 
  impacts[is.na(impacts)] <- 0
  categories <- categories[[1]] %>% 
    merge(categories[[2]], all=TRUE ) %>% 
    merge(categories[[3]], all=TRUE) 
  categories[is.na(categories)] <- 0
  
  total_all <- data_[, .N]
  total_1 <- data_[get(treatment_) == treatment_vals[1], .N]
  total_2 <- data_[get(treatment_) == treatment_vals[2], .N]
  
  number_reiqs$total <- c(total_all, total_1, total_2)
  
  impacts[, total_all := total_all]
  impacts[, c(treatment_vals[1]) := total_1]
  impacts[, c(treatment_vals[2]) := total_2]
  
  categories[, total_all := total_all]
  categories[, c(treatment_vals[1]) := total_1]
  categories[, c(treatment_vals[2]) := total_2]
  
  var_1 <- paste(treatment_vals[1], 'patients', sep='_')
  var_2 <- paste(treatment_vals[2], 'patients', sep='_')
  
  p_vals_reiqs <- data.frame(
    p_val_1_total = binom.test(
      c(number_reiqs[group==treatment_vals[1], reiqs],
        number_reiqs[group=='all', reiqs]),
      c(number_reiqs[group==treatment_vals[1], total],
        number_reiqs[group=='all', total])
    )$p.val, 
    p_val_2_total = binom.test(
      c(number_reiqs[group==treatment_vals[2], reiqs],
        number_reiqs[group=='all', reiqs]),
      c(number_reiqs[group==treatment_vals[2], total],
        number_reiqs[group=='all', total])
    )$p.val, 
    p_val_1_2 = binom.test(
      c(number_reiqs[group==treatment_vals[1], reiqs],
        number_reiqs[group==treatment_vals[2], reiqs]),
      c(number_reiqs[group==treatment_vals[1], total],
        number_reiqs[group==treatment_vals[2], total])
    )$p.val
  )
  
  
  rows_notna <- impacts[, !any(is.na(.SD)), 1:nrow(impacts)][,V1]
  impacts[rows_notna, p_val_1_total := binom.test(
    c(get(var_1), all_patients),
    c(get(treatment_vals[1]), total_all)
  )$p.val , 1:nrow(impacts)]
  
  impacts[rows_notna, p_val_2_total := binom.test(
    c(get(var_2), all_patients),
    c(get(treatment_vals[2]), total_all)
  )$p.val , 1:nrow(impacts)]
  
  impacts[rows_notna, p_val_1_2 := binom.test(
    c(get(var_1), get(var_2)),
    c(get(treatment_vals[1]), get(treatment_vals[2]))
  )$p.val , 1:nrow(impacts)]
  
  rows_notna <- categories[, !any(is.na(.SD)), 1:nrow(categories)][,V1]
  categories[rows_notna, p_val_1_total := binom.test(
    c(get(var_1), all_patients),
    c(get(treatment_vals[1]), total_all)
  )$p.val , 1:nrow(categories)]
  
  categories[rows_notna, p_val_2_total := binom.test(
    c(get(var_2), all_patients),
    c(get(treatment_vals[2]), total_all)
  )$p.val , 1:nrow(categories)]
  
  categories[rows_notna, p_val_1_2 := binom.test(
    c(get(var_1), get(var_2)),
    c(get(treatment_vals[1]), get(treatment_vals[2]))
  )$p.val , 1:nrow(categories)]
  
  
  return(list(
    impacts = impacts,
    categories = categories,
    number_reiqs = number_reiqs,
    p_vals_reiqs = p_vals_reiqs
  ))
}


get_time_points <- function(tps){
  sapply(tps, get_time_point)
}

get_time_point <- function(tp){
  tp_pre <- substr(tp, 1,2)
  time_val <- 0
  if( tp_pre == '6M') return( 0.5 )
  else{
    return(substr(tp_pre, 1, 1))
  }
}


calc_estability <- function(data_, factors_, demo, treatment=NULL){
  time_points <- c('6M.', paste(1:5, 'Y.', sep=""))
  
  for(qual_ in factors_){
    base_tp <- paste('6W.', qual_)
    base_tp_num <- 6/52
    if( !(base_tp %in% colnames(data_))){
      base_tp <- paste('6M.', qual_)
      base_tp_num <- 1/2
    }
    
    print("\n\n\n### {qual_}" %>% f)
    print("Base Time point {base_tp}" %>% f)
    print("\n\n Time evolution" %>% f)
    c(
      base_tp,
      paste(time_points, qual_),
      'Code of the patient',
      treatment
    ) %>% unique %>%
      calc_long_format(data_, ., treatment, base_tp, base_tp_num, qual_)  %>%
      create_lme_model(treatment)

    # print("\n\n Basic Demographics" %>% f)
    # c(
    #   base_tp, 
    #   paste(time_points, qual_),
    #   'Code of the patient',
    #   demo[['level_0']],
    #   treatment
    # ) %>% unique %>% 
    #   calc_long_format(data_, ., treatment, base_tp, base_tp_num, demo[['level_0']]) %>% 
    #   create_lme_model(treatment, demo[['level_0']])

    # print("\n\n Preop Demographics" %>% f)
    c(
      base_tp,
      paste(time_points, qual_),
      'Code of the patient',
      demo %>% unlist,
      treatment
    ) %>% unique %>%
      calc_long_format(data_, ., treatment, base_tp, base_tp_num, qual_, demo %>% unlist) %>%
      create_lme_model(treatment, demo %>% unlist)
  }
}

calc_long_format <- function(data_, cols_, treatment, base_tp, base_tp_num, qual_, demo_=NULL){
  data_[, .SD, .SDcols = c(cols_, qual_)] %>% 
    melt(id.vars =c('Code of the patient', demo_, treatment, base_tp, qual_)) %>% 
    .[, value := value - get(base_tp)] %>%
    .[, preop := get(qual_)] %>%
    .[, inc_preop := get(base_tp) - get(qual_)] %>%
    .[, time_point := get_time_points(variable)] %>%
    .[, time_point := as.numeric(as.character(time_point)) - base_tp_num] %>%
    setnames('Code of the patient', 'patient_id') %>% 
    .[, .SD, .SDcols = c(
      "patient_id", "time_point", "value", "inc_preop", 
      "preop", demo_, treatment)
    ] %>% 
    na.omit ->
    long_data
  
  return(long_data)
}

create_lme_model <- function(long_data, treatment, demo_=NULL){
  
  if( !is.null(treatment) ){
    if(!is.null(demo_)){
      control_vars <- c(demo_, 'inc_preop', 'preop')
      formula_ <- paste("`", control_vars, "`", sep="", collapse=" + ")
      formula_ <- "value ~ {formula_} + time_point + {treatment} + (time_point - 1|patient_id)" %>%
        f %>% as.formula
      smodel <- lmer(formula_, data=long_data)
    }else{
      formula_ <- "value ~ time_point + {treatment} + (time_point - 1|patient_id)" %>% 
        f %>% as.formula
      smodel <- lmer(formula_, data=long_data)  
    }
  }else{
    if(!is.null(demo_)){
      control_vars <- c(demo_, 'inc_preop', 'preop')
      formula_ <- paste("`", control_vars, "`", sep="", collapse=" + ")
      formula_ <- "value ~ {formula_} + time_point + (time_point - 1|patient_id)" %>%
        f %>% as.formula
      smodel <- lmer(formula_, data=long_data)
    }else{
      smodel <- lmer(value ~ time_point + (time_point - 1|patient_id), data=long_data) 
    }
  }
  
  if( isSingular(smodel)) stop('Singular Model')
  # print(smodel %>% summary)
  # ranef(smodel)
  print(summary(smodel)$coefficients %>% round(4))
}
