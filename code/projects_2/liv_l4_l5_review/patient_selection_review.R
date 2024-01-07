library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(lme4)
library(lmerTest)

source('basic.R')

get_data_base <- function(xls_path=xls_path){
  
  discarded_patients <- readLines('discarded_patients.txt')
  discarded_patients <- c(discarded_patients)
  
  clinical_data <- read_excel(xls_path) %>% 
    as.data.table() %>% 
    .[!(`Code of the patient` %in% discarded_patients)] 
  
  setnames(clinical_data, 'Major curve Cobb angle', 'Static Major curve Cobb angle')
  setnames(clinical_data, '6W. Static Major Curve Cobb Angle', '6W. Static Major curve Cobb angle')
  # setnames(clinical_data, '6M. Static Major Curve Cobb Angle', '6M. Static Major curve Cobb angle')
  # setnames(clinical_data, '1Y. Static Major Curve Cobb Angle', '1Y. Static Major curve Cobb angle')
  setnames(clinical_data, '2Y. Static Major Curve Cobb Angle', '2Y. Static Major curve Cobb angle')
  setnames(clinical_data, '5Y. Static Major Curve Cobb Angle', '5Y. Static Major curve Cobb angle')
  setnames(clinical_data, 'st1. ASA classification', 'ASA classification')
  setnames(clinical_data, 'Site Name', 'Site')
  setnames(clinical_data, 'Statut vital', 'Vital status')
  setnames(clinical_data, 'ODI - Score (%)_First Visit', 'ODI - Score (%)')
  setnames(clinical_data, 'SRS22 - Function_First Visit', 'SRS22 - Function / Activity')
  setnames(clinical_data, 'SRS22 - Pain_First Visit', 'SRS22 - Pain')
  setnames(clinical_data, 'SRS22 -SI_First Visit', 'SRS22 - Self image / Appearance')
  setnames(clinical_data, 'SRS22 - MH_First Visit', 'SRS22 - Mental health')
  setnames(clinical_data, 'SRS22 - SRS Subtotal score_First Visit', 'SRS22 - SRS Subtotal score')
  setnames(clinical_data, 'SF36 - PCS_First Visit', 'SF36 - PCS')
  setnames(clinical_data, 'SF36 - MCS_First Visit', 'SF36 - MCS') 
  setnames(clinical_data, 'SRS22 - Satisfaction_First Visit', 'SRS22 - Satisfaction with management')
  
  clinical_data[, `Lordosis (top of L1-S1)`:=as.numeric(`Lordosis (top of L1-S1)`)]
  
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
  
  clinical_data[`Code of the patient`=='BCNISSY0091-SC',
    `Posterior Instrumented Fusion: Upper / Lower Levels` := "T7-Iliac"]
  clinical_data[`Code of the patient`=='BCNISSY0105-SC',
    `Posterior Instrumented Fusion: Upper / Lower Levels` := "T9-Iliac"]
  
  return(clinical_data)
}

get_data <- function(xls_path=xls_path){
  clinical_data <- get_data_base(xls_path)
  
  uiv_ <- c('T10', 'T11', 'T12', 'L1')
  clinical_data %>% 
    .[, upper:= substr(`Posterior Instrumented Fusion: Upper / Lower Levels`, 1, 2)] %>% 
    .[, uiv_t10_12_l1 := ifelse(upper %in% uiv_, 'Yes', 'No')]
  
  
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
  
  clinical_data[, `Posterior Instrumented Fusion: Upper / Lower Levels`] %>% 
    sapply(. %>% strsplit('-') %>% .[[1]] %>% .[1])  ->
    post_intr_fusion
  
  clinical_data[, 
    Non_instrumented_segments := sapply(post_intr_fusion, 
      calc_number_non_instrumented_segments)]
  
  clinical_data[, essg_category := 'others']
  clinical_data[`ESSG Diagnosis` == 'Degenerative', 
    essg_category := 'degenerative']
  clinical_data[`ESSG Diagnosis` == 'Failed-back', 
    essg_category := 'degenerative']
  clinical_data[`ESSG Diagnosis` == 'Idiopathic', 
    essg_category := 'idiopathic']
  
  return(clinical_data)
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

stats_fun <- function(variable, effect_size=FALSE){
  if( class(variable) == 'numeric' ){
    if( effect_size == TRUE ){
      return( list(
        mean = mean(variable, na.rm=TRUE) %>% round(2),
        sd = sd(variable, na.rm = TRUE) %>% round(2),
        effect_size = (mean(variable, na.rm=TRUE)/sd(variable, na.rm = TRUE)) %>% 
          round(2)
      ))
    }else{
      return( list(
        mean = mean(variable, na.rm=TRUE) %>% round(2),
        sd = sd(variable, na.rm = TRUE) %>% round(2)
      ))  
    }
  }else{
    res_table <- table(variable)
    return(list(
      table = res_table, 
      proportion = prop.table(res_table)
    ))
  }
}


calc_p_vals <- function(dt, var_, treatment_, treatment_vals=c('Yes', 'No'), 
  treat_and_co = NULL){
  vals_n <- dt[, get(var_)] %>% uniqueN
  treat_1 <- treatment_vals[1]
  treat_2 <- treatment_vals[2]
  
  if( !is.null(treat_and_co) ){
    outcome_1 <- dt[get(treatment_)==treat_and_co][, get(var_)]
    outcome_2 <- dt[get(treatment_)!=treat_and_co][, get(var_)]
  }else{
    outcome_1 <- dt[get(treatment_)==treat_1][, get(var_)]
    if( treat_1 == 'all' ) outcome_1 <- dt[, get(var_)]
    outcome_2 <- dt[get(treatment_)==treat_2][, get(var_)]
    if( treat_2 == 'all' ) outcome_2 <- dt[, get(var_)]
  }
  
  p.val <- NA
  if(class(dt[, get(var_)]) == 'numeric'){
    p.val <- t.test(
      outcome_1,
      outcome_2
    )$p.val
  }else if(vals_n == 2){
    t1 <- table(outcome_1)
    t2 <- table(outcome_2)
    
    if( dim(t1) == 2 & dim(t2) == 2){
      p.val <- prop.test(rbind(t1, t2))$p.val
    }
  }else{
    p.val <- NA
  }
  
  return(p.val)
}

calc_complications <- function(data_, treatment_vals, treatment_, 
  lower_years=0, upper_years=5){
  
  complication_data <- read_excel(xls_path, sheet = 'Complications') %>% 
    as.data.table() %>% 
    .[`Days since surgery` <= upper_years * 365] %>% 
    .[`Days since surgery` > lower_years * 365] %>% 
    .[`Complication Type`=='Primary']
  
  patient_groups <- list()
  patient_groups$all <- data_[, `Code of the patient` %>% unique]
  patient_groups[[treatment_vals[1]]] <- data_ %>% 
    .[get(treatment_) == treatment_vals[1], `Code of the patient` %>% unique]
  patient_groups[[treatment_vals[2]]] <- data_ %>% 
    .[get(treatment_) == treatment_vals[2], `Code of the patient` %>% unique]
  
  
  number_reiqs <- list()
  impacts <- list()
  categories <- list()
  categories_major <- list()
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
    
    cat_name <- paste(name_, 'num', sep='_')
    cat_num <- comp_[`Complication Impact`=='Major Complication',
      .(cat_name = .N), `Category of the complication`] %>% 
      setnames('cat_name', cat_name)
    cat_name <- paste(name_, 'patients', sep='_')
    cat_patients <- comp_[`Complication Impact`=='Major Complication', 
      .(cat_name = uniqueN(`Code of the patient`)), `Category of the complication`] %>% 
      setnames('cat_name', cat_name)
    categories_major[[name_]] <- merge(cat_num, cat_patients)
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
  categories_major <- categories_major[[1]] %>% 
    merge(categories_major[[2]], all=TRUE ) %>% 
    merge(categories_major[[3]], all=TRUE) 
  categories_major[is.na(categories_major)] <- 0
  
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
  
  categories_major[, total_all := total_all]
  categories_major[, c(treatment_vals[1]) := total_1]
  categories_major[, c(treatment_vals[2]) := total_2]
  
  var_1 <- paste(treatment_vals[1], 'patients', sep='_')
  var_2 <- paste(treatment_vals[2], 'patients', sep='_')
  
  p_vals_reiqs <- data.frame(
    p_val_1_total = prop.test(
      c(number_reiqs[group==treatment_vals[1], patients],
        number_reiqs[group=='all', patients]),
      c(number_reiqs[group==treatment_vals[1], total],
        number_reiqs[group=='all', total])
    )$p.val, 
    p_val_2_total = prop.test(
      c(number_reiqs[group==treatment_vals[2], patients],
        number_reiqs[group=='all', patients]),
      c(number_reiqs[group==treatment_vals[2], total],
        number_reiqs[group=='all', total])
    )$p.val, 
    p_val_1_2 = prop.test(
      c(number_reiqs[group==treatment_vals[1], patients],
        number_reiqs[group==treatment_vals[2], patients]),
      c(number_reiqs[group==treatment_vals[1], total],
        number_reiqs[group==treatment_vals[2], total])
    )$p.val
  )
  
  
  rows_notna <- impacts[, !any(is.na(.SD)), 1:nrow(impacts)][,V1]
  impacts[rows_notna, p_val_1_total := prop.test(
    c(get(var_1), all_patients),
    c(get(treatment_vals[1]), total_all)
  )$p.val , 1:nrow(impacts)]
  
  impacts[rows_notna, p_val_2_total := prop.test(
    c(get(var_2), all_patients),
    c(get(treatment_vals[2]), total_all)
  )$p.val , 1:nrow(impacts)]
  
  impacts[rows_notna, p_val_1_2 := prop.test(
    c(get(var_1), get(var_2)),
    c(get(treatment_vals[1]), get(treatment_vals[2]))
  )$p.val , 1:nrow(impacts)]
  
  rows_notna <- categories[, !any(is.na(.SD)), 1:nrow(categories)][,V1]
  categories[rows_notna, p_val_1_total := prop.test(
    c(get(var_1), all_patients),
    c(get(treatment_vals[1]), total_all)
  )$p.val , 1:nrow(categories)]
  
  categories[rows_notna, p_val_2_total := prop.test(
    c(get(var_2), all_patients),
    c(get(treatment_vals[2]), total_all)
  )$p.val , 1:nrow(categories)]
  
  categories[rows_notna, p_val_1_2 := prop.test(
    c(get(var_1), get(var_2)),
    c(get(treatment_vals[1]), get(treatment_vals[2]))
  )$p.val , 1:nrow(categories)]
  
  
  rows_notna <- categories_major[, !any(is.na(.SD)), 1:nrow(categories_major)][,V1]
  categories_major[rows_notna, p_val_1_total := prop.test(
    c(get(var_1), all_patients),
    c(get(treatment_vals[1]), total_all)
  )$p.val , 1:nrow(categories_major)]
  
  categories_major[rows_notna, p_val_2_total := prop.test(
    c(get(var_2), all_patients),
    c(get(treatment_vals[2]), total_all)
  )$p.val , 1:nrow(categories_major)]
  
  categories_major[rows_notna, p_val_1_2 := prop.test(
    c(get(var_1), get(var_2)),
    c(get(treatment_vals[1]), get(treatment_vals[2]))
  )$p.val , 1:nrow(categories_major)]

  return(list(
    impacts = impacts,
    categories = categories,
    number_reiqs = number_reiqs,
    p_vals_reiqs = p_vals_reiqs,
    categories_major = categories_major
  ))
}

calc_time_to_surgery <- function(data_, treatment_, complication_data){
  complication_data %>% 
    .[, .(days_to_treatment = min(`Days since surgery`)), "Code of the patient"] %>% 
    merge(data_[, .(`Code of the patient`, treatment = get(treatment_))], ., all.x=TRUE, all.y=FALSE) ->
    time_to_treat
  
  treatment_vals_ <- data_[, get(treatment_) %>% unique]
  res <- list()
  for( treat_1 in 1:(len(treatment_vals_)-1)){
    val_1 <- treatment_vals_[treat_1]
    res[[val_1]] <- list()
    for( treat_2 in (treat_1+1):len(treatment_vals_)){
      val_2 <- treatment_vals_[treat_2]
      p_val <- t.test(
        time_to_treat[treatment == val_1, days_to_treatment],
        time_to_treat[treatment == val_2, days_to_treatment]
      )$p.val
      res[[val_1]][[val_2]] <- p_val
    }
  }
  
  return(list(
    p_vals = res %>% unlist,
    stats = time_to_treat[, .(
      mean=mean(days_to_treatment, na.rm=TRUE),
      sd=sd(days_to_treatment, na.rm=TRUE)
      ), treatment]
  ))
}

calc_complications_many <- function(data_, treatment_vals, treatment_, lower_years=0, upper_years=5){
  
  complication_data <- read_excel(xls_path, sheet = 'Complications') %>% 
    as.data.table() %>% 
    .[`Days since surgery` <= upper_years * 365] %>% 
    .[`Days since surgery` > lower_years * 365] %>% 
    .[`Complication Type`=='Primary']
  
  patient_groups <- list()
  patient_groups$all <- data_[, `Code of the patient` %>% unique]
  
  for( val_ in treatment_vals){
    patient_groups[[val_]] <- data_ %>% 
      .[get(treatment_) == val_, `Code of the patient` %>% unique]
  }
  
  number_reiqs <- list()
  impacts <- list()
  categories <- list()
  categories_major <- list()
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
    
    cat_name <- paste(name_, 'num', sep='_')
    cat_num <- comp_[`Complication Impact`=='Major Complication',
      .(cat_name = .N), `Category of the complication`] %>% 
      setnames('cat_name', cat_name)
    cat_name <- paste(name_, 'patients', sep='_')
    cat_patients <- comp_[`Complication Impact`=='Major Complication', 
      .(cat_name = uniqueN(`Code of the patient`)), `Category of the complication`] %>% 
      setnames('cat_name', cat_name)
    categories_major[[name_]] <- merge(cat_num, cat_patients)
  }
  
  number_reiqs <- do.call(rbind, number_reiqs)
  
  impacts_ <- impacts[['all']] 
  for( val_ in treatment_vals){
    impacts_ %<>% merge(impacts[[val_]], all=TRUE )
  }
  impacts <- impacts_
  impacts[is.na(impacts)] <- 0
  
  categories_ <- categories[['all']] 
  for( val_ in treatment_vals){
    categories_ %<>% merge(categories[[val_]], all=TRUE )
  }
  categories <- categories_
  categories[is.na(categories)] <- 0
  
  categories_major_ <- categories_major[['all']] 
  for( val_ in treatment_vals){
    categories_major_ %<>% merge(categories_major[[val_]], all=TRUE )
  }
  categories_major <- categories_major_
  categories_major[is.na(categories_major)] <- 0
  
  total_patients <- list()
  total_patients[['all']] <- data_[, `Code of the patient` %>% uniqueN]
  for( val_ in treatment_vals){
    total_patients[[val_]] <- data_[
      get(treatment_) == val_, 
      `Code of the patient` %>% uniqueN]
  }
  
  total_patients_dt <- total_patients %>% 
    as.data.table() %>%
    melt %>% 
    setnames('variable', 'group') %>% 
    setnames('value', 'total')
  number_reiqs <- merge(number_reiqs, total_patients_dt)
  
  
  for(name_ in names(total_patients) ){
    impacts[, c(name_) := total_patients[[name_]]]
  }
  
  for(name_ in names(total_patients) ){
    categories[, c(name_) := total_patients[[name_]]]
  }
  
  for(name_ in names(total_patients) ){
    categories_major[, c(name_) := total_patients[[name_]]]
  }
  
  
  p_vals_reiqs <- data.table(to_remove=NA)
  for(pos_1 in 1:(len(total_patients) - 1)){
    val_1 <- names(total_patients)[pos_1]
    for(pos_2 in (pos_1 + 1):len(total_patients)){
      val_2 <- names(total_patients)[pos_2]
      p_val <- prop.test(
        c(number_reiqs[group==val_1, patients],
          number_reiqs[group==val_2, patients]),
        c(number_reiqs[group==val_1, total],
          number_reiqs[group==val_2, total])
      )$p.val %>% round(4)
      name_vals <- paste("p_val", val_1, val_2, sep="_")
      p_vals_reiqs[[name_vals]] <- p_val
    }  
  }
  p_vals_reiqs[, to_remove := NULL]
  
  rows_notna <- impacts[, !any(is.na(.SD)), 1:nrow(impacts)][,V1]
  for(pos_1 in 1:(len(total_patients) - 1)){
    val_1 <- names(total_patients)[pos_1]
    for(pos_2 in (pos_1 + 1):len(total_patients)){
      val_2 <- names(total_patients)[pos_2]
      
      var_1 <- paste(val_1, 'patients', sep='_')
      var_2 <- paste(val_2, 'patients', sep='_')
      name_vals <- paste("p_val", val_1, val_2, sep="_")
      impacts[rows_notna, c(name_vals) := prop.test(
        c(get(var_1), get(var_2)),
        c(get(val_1), get(val_2))
      )$p.val %>% round(4) , 1:nrow(impacts)]
    }  
  }
  
  rows_notna <- categories[, !any(is.na(.SD)), 1:nrow(categories)][,V1]
  for(pos_1 in 1:(len(total_patients) - 1)){
    val_1 <- names(total_patients)[pos_1]
    for(pos_2 in (pos_1 + 1):len(total_patients)){
      val_2 <- names(total_patients)[pos_2]
      
      var_1 <- paste(val_1, 'patients', sep='_')
      var_2 <- paste(val_2, 'patients', sep='_')
      name_vals <- paste("p_val", val_1, val_2, sep="_")
      categories[rows_notna, c(name_vals) := prop.test(
        c(get(var_1), get(var_2)),
        c(get(val_1), get(val_2))
      )$p.val %>% round(4) , 1:nrow(categories)]
    }  
  }
  
  rows_notna <- categories_major[, !any(is.na(.SD)), 1:nrow(categories_major)][,V1]
  for(pos_1 in 1:(len(total_patients) - 1)){
    val_1 <- names(total_patients)[pos_1]
    for(pos_2 in (pos_1 + 1):len(total_patients)){
      val_2 <- names(total_patients)[pos_2]
      
      var_1 <- paste(val_1, 'patients', sep='_')
      var_2 <- paste(val_2, 'patients', sep='_')
      name_vals <- paste("p_val", val_1, val_2, sep="_")
      categories_major[rows_notna, c(name_vals) := prop.test(
        c(get(var_1), get(var_2)),
        c(get(val_1), get(val_2))
      )$p.val %>% round(4) , 1:nrow(categories_major)]
    }  
  }
  
  return(list(
    impacts = impacts,
    categories = categories,
    number_reiqs = number_reiqs,
    p_vals_reiqs = p_vals_reiqs,
    categories_major = categories_major,
    time_to_surgery = calc_time_to_surgery(data_, treatment_, complication_data)
  ))
}

get_time_points <- function(tps){
  tps %>% 
    sapply(get_time_point) %>% 
    sapply(. %>% as.character %>% as.numeric) ->
    tps_
  tps_[is.na(tps_)] <- 0
  tps_
}

get_time_point <- function(tp){
  tp_pre <- substr(tp, 1,2)
  time_val <- 0
  if( tp_pre == '6W') return( 6/52 )
  if( tp_pre == '6M') return( 0.5 )
  else{
    return(substr(tp_pre, 1, 1))
  }
}


calc_estability <- function(data_, factors_, demo=NULL, treatment=NULL, radio_confounding=list()){
  time_points <- c('6M.', paste(1:5, 'Y.', sep=""))
  
  for(qual_ in factors_){
    base_tp <- paste('6W.', qual_)
    base_tp_num <- 6/52
    if( !(base_tp %in% colnames(data_))){
      base_tp <- paste('6M.', qual_)
      base_tp_num <- 1/2
    }
    
    print("\n\n\n### {qual_}" %>% f)
    print("Change vs Base Time point {base_tp}" %>% f)
    print("\n\n Time evolution" %>% f)
    c(
      base_tp,
      paste(time_points, qual_),
      'Code of the patient',
      treatment
    ) %>% unique %>%
      calc_long_format(data_, ., treatment, base_tp, base_tp_num, qual_)  %>%
      create_lme_model(treatment)


    if( !is.null(demo) ){
      qual_preop <- qual_
      # if( qual_ == 'RSA' ){
      #   qual_preop <- 'Global Tilt'
      # }else if(qual_ == 'RPV'){
      #   qual_preop <- 'Sacral Slope'
      # }
      
      # print("\n\nPreop & Inc preop variable: {qual_preop}" %>% f)
      
      print("\nBasic Demographics" %>% f)
      c(
        base_tp,
        paste(time_points, qual_),
        'Code of the patient',
        demo[['demographic_0']],
        treatment
      ) %>% unique %>%
        calc_long_format(
          data_, ., treatment, base_tp, base_tp_num, qual_preop, demo[['demographic_0']]
        ) %>%
        create_lme_model(treatment, demo[['demographic_0']], include_preop = FALSE)
      
      print("\n\n Factors" %>% f)
      demo_factors <- c(demo %>% unlist, radio_confounding[[qual_]])
      c(
        base_tp,
        paste(time_points, qual_),
        'Code of the patient',
        demo_factors,
        treatment
      ) %>% unique %>%
        calc_long_format(
          data_, ., treatment, base_tp, base_tp_num, 
          qual_preop, demo_factors
        ) %>%
        create_lme_model(treatment, demo_factors, include_preop = FALSE)
    }
    
  }
}

calc_long_format <- function(data_, cols_, treatment, base_tp, base_tp_num, qual_preop, demo_=NULL){
  base_qual_preop <- paste(substr(base_tp, 1, 3), qual_preop)
  data_[, .SD, .SDcols = c(cols_, qual_preop, base_qual_preop)] %>% 
    melt(id.vars =c(
      'Code of the patient', demo_, treatment, base_tp, 
      qual_preop, base_qual_preop) %>% unique
    ) %>% 
    .[, value := value - get(base_tp)] %>%
    .[, preop := get(qual_preop)] %>%
    .[, inc_preop := get(base_qual_preop) - get(qual_preop)] %>%
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

create_lme_model <- function(long_data, treatment, demo_=NULL, include_preop=TRUE){
  
  if( !is.null(treatment) ){
    if(!is.null(demo_)){
      if( !include_preop ){
        control_vars <- demo_
      }else{
        control_vars <- c(demo_, 'inc_preop', 'preop')
      }
      formula_ <- paste("`", control_vars, "`", sep="", collapse=" + ")
      formula_ <- "value ~ {formula_} + time_point + {treatment} + (time_point - 1|patient_id)" %>%
        f %>% as.formula
      smodel <- lmer(formula_, data=long_data)
      if( isSingular(smodel) ){
        model_summmary <- summary(smodel)
        model_summmary <- as.data.table(model_summmary$varcor)
        cov_pat <- model_summmary[grp=='patient_id', vcov]
        
        if(cov_pat < 1e-10){
          formula_ <- paste("`", control_vars, "`", sep="", collapse=" + ")
          formula_ <- "value ~ {formula_} + time_point + {treatment}" %>%
            f %>% as.formula
          smodel <- lm(formula_, data=long_data) 
        }else{
          stop()
        }
      }
    }else{
      formula_ <- "value ~ time_point + {treatment} + (time_point - 1|patient_id)" %>% 
        f %>% as.formula
      smodel <- lmer(formula_, data=long_data)  
      if( isSingular(smodel) ){
        model_summmary <- summary(smodel)
        model_summmary <- as.data.table(model_summmary$varcor)
        cov_pat <- model_summmary[grp=='patient_id', vcov]
        
        if(cov_pat < 1e-10){
          formula_ <- "value ~ time_point + {treatment}" %>% 
            f %>% as.formula
          smodel <- lm(formula_, data=long_data) 
        }else{
          stop()
        }
      }
    }
  }else{
    if(!is.null(demo_)){
      if( !include_preop ){
        control_vars <- demo_
      }else{
        control_vars <- c(demo_, 'inc_preop', 'preop')
      }
      formula_ <- paste("`", control_vars, "`", sep="", collapse=" + ")
      formula_ <- "value ~ {formula_} + time_point + (time_point - 1|patient_id)" %>%
        f %>% as.formula
      smodel <- lmer(formula_, data=long_data)
      if( isSingular(smodel) ){
        model_summmary <- summary(smodel)
        model_summmary <- as.data.table(model_summmary$varcor)
        cov_pat <- model_summmary[grp=='patient_id', vcov]
        
        if(cov_pat < 1e-10){
          formula_ <- paste("`", control_vars, "`", sep="", collapse=" + ")
          formula_ <- "value ~ {formula_} + time_point" %>%
            f %>% as.formula
          smodel <- lm(formula_, data=long_data) 
        }else{
          stop()
        }
      }
    }else{
      smodel <- lmer(value ~ time_point + (time_point - 1|patient_id), data=long_data) 
      if( isSingular(smodel) ){
        model_summmary <- summary(smodel)
        model_summmary <- as.data.table(model_summmary$varcor)
        cov_pat <- model_summmary[grp=='patient_id', vcov]
        
        if(cov_pat < 1e-10){
          smodel <- lm(value ~ time_point , data=long_data) 
        }else{
          stop()
        }
      }
    }
  }
  
  # if( isSingular(smodel)) stop('Singular Model')
  # print(smodel %>% summary)
  # ranef(smodel)
  print(summary(smodel)$coefficients %>% round(4))
}



calc_time_p_values <- function(data__, var_, var_6_b, var_2y, var_5y, cluster_name){
  val_ <- data__[[var_]]
  val_6_b <- data__[[var_6_b]]
  val_2y <- data__[[var_2y]]
  val_5y <- data__[[var_5y]]
  value_6_b <- substr(var_6_b, 1, 2)
  
  time_p_vals <- data.frame()
  time_p_vals %<>% 
    rbind(data.frame(
      t0='Preop', t1=value_6_b, p.val=t.test(val_6_b, val_)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0='Preop', t1='2Y', p.val=t.test(val_2y, val_)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0='Preop', t1='5Y', p.val=t.test(val_5y, val_)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0=value_6_b, t1='2Y', p.val=t.test(val_2y, val_6_b)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0=value_6_b, t1='5Y', p.val=t.test(val_5y, val_6_b)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0='2Y', t1='5Y', p.val=t.test(val_5y, val_2y)$p.val %>% round(4)
    ))  
  time_p_vals$group <- cluster_name
  
  return(time_p_vals)
}

calculate_mae <- function(data_, var__){
  
  if( !is.numeric(data_[, get(var__)]) ) return(list(mae=NA, rel_mae=NA))
  
  prediction <- data_[, .(get(var__), cluster)]
  setnames(prediction, 'V1', var__)
  
  ind <- 1:(floor(nrow(prediction)/2))
  data_1 <- prediction[ind]
  pred_1 <- data_1[, .(pred=mean(get(var__), na.rm=TRUE)), cluster]
  
  data_2 <- prediction[-ind]
  pred_2 <- data_2[, .(pred=mean(get(var__), na.rm=TRUE)), cluster]
  
  data_1 <- merge(data_1, pred_2, by='cluster')
  data_2 <- merge(data_2, pred_1, by='cluster')
  mae_1 <- data_1[, mean(abs(pred - get(var__)), na.rm=TRUE)]
  mae_2 <- data_2[, mean(abs(pred - get(var__)), na.rm=TRUE)]
  
  val_mean <- mean(data_1[, get(var__)], na.rm=TRUE)
  val_mean <- val_mean + mean(data_2[, get(var__)], na.rm=TRUE)
  val_mean <- val_mean/2
  mae <- (mae_1 + mae_2)
  rel_mae <- mae/val_mean
  
  return(list(mae=mae, rel_mae=rel_mae))
}

