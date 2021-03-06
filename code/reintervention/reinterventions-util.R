library(readxl)
library(magrittr)
library(data.table)
library(stringr)
library(yaml)
library(ggplot2)
library(Hmisc)


get_data <- function(){
  xls_path <- 'data/ESSG extraction December 2020 - DEF.xlsx'
  # excel_sheets(xls_path)
  
  vars_ <- read_yaml('code/reintervention/treatment_vars.yml')
  
  rev_patient_data <- read_excel(xls_path, sheet = "Revision surgeries") %>% 
    data.table 
  clinical_data <- read_excel(xls_path) %>% 
    data.table() 
  complications <- read_excel(xls_path, sheet = "Complications") %>% 
    data.table 
  
  setnames(clinical_data, 'ideal LL\r\n\r\n', 'ideal LL')
  setnames(clinical_data, 'RLL\r\n\r\n', 'RLL')
  
  
  discarded_patients <- readLines('code/five_years/discarded_patients')
  
  clinical_data %<>% 
    .[, followup_2y := 
        !is.na(`2 YEAR VISIT - Date of visit`) | 
        !is.na(`3 YEAR VISIT - Date of visit`)] %>% 
    .[, followup_5y := 
        !is.na(`5 YEAR VISIT - Date of visit`) | 
        !is.na(`6 YEAR VISIT - Date of visit`)]
  
  clinical_data %>% 
    .[, `Pelvic Fixation` := grepl(
      'Iliac',
      `Posterior Instrumented Fusion: Upper / Lower Levels`,
      fixed=TRUE
    )] %>%
  .[, `Pelvic Fixation` := ifelse(`Pelvic Fixation` == TRUE, 'Yes', 'No')]

  valid_patients <- clinical_data %>% 
    .[followup_2y==TRUE] %>% 
    .[Site != 'ANK Op'] %>% 
    .[`Vital status` == 'Alive'] %>% 
    .[!(`Code of the patient` %in% discarded_patients)] %>% 
    .[`st1. Date of Stage 1` %>% as.Date() < as.Date('2015-3-15')] %>% 
    .[, `Code of the patient` %>% unique]
  
  clinical_data %<>% .[`Code of the patient` %in% valid_patients]
  rev_patient_data %<>% .[`Code of the patient` %in% valid_patients]
  complications %<>% .[`Code of the patient` %in% valid_patients]
  
  time_evolution <- get_time_evolution(clinical_data, rev_patient_data)
  complications <- get_complications(complications)
  category_los <- get_category_los(clinical_data, rev_patient_data, complications)
  
  groups <- get_prior_groups()
  
  time_evolution[, prior_group:='none']
  complications[, prior_group:='none']
  category_los[, prior_group:='none']
  for(group_name in groups %>% names){
    group <- groups[[group_name]]
    time_evolution[patient_id %in% group, prior_group:=group_name]
    complications[patient_id %in% group, prior_group:=group_name]
    category_los[patient_id %in% group, prior_group:=group_name]
  }
  
  return(list(
    time_evolution, 
    complications, 
    category_los,
    clinical_data
  ))
}

get_prior_groups <- function(){
  prev_surgeries <- read_excel('data/Previous surgeries.xlsx') %>% 
    as.data.table
  
  groups <- list()
  
  groups[['a']] <- prev_surgeries[, any(`Decompression (yes/no)` == 'Yes') & all(`Fusion (yes/no)` == 'No'), 
    `Code of the patient`] %>% 
    .[V1==TRUE] %>% 
    .[, `Code of the patient`]
    
  groups[['b']] <- prev_surgeries[, max(`Fusion nb`, na.rm = TRUE), `Code of the patient`] %>% 
    .[0 < V1 & V1 < 4] %>% 
    .[, `Code of the patient`]
  
  groups[['c']] <- prev_surgeries[, max(`Fusion nb`, na.rm = TRUE), `Code of the patient`] %>% 
    .[V1 >= 4] %>% 
    .[, `Code of the patient`]
  
  # groups[['c_1']] <- prev_surgeries[`Code of the patient` %in% groups[['c']]] %>% 
  #   .[grepl('Iliac', `Fusion upper-lower levels`) | 
  #       `Pedicle subtraction osteotomy yes/no` == 'Yes', ]
  # 
  # groups[['c_2']] <- prev_surgeries[`Code of the patient` %in% groups[['c']]] %>% 
  #   .[!(`Code of the patient` %in% groups[['c_1']]), ]
  
  return(groups)
}
    

get_category_los <- function(clinical_data, rev_patient_data, complications){
  rev_patient_data[, 
    .(patient_id=`Code of the patient`, 
      hospitalization_time=`Hospitalization time`, 
      blood_loss=`Total blood loss (mL)`,
      surgical_time=`Total surgical time st1+st2+st3`,
      date=`st1. Date of Stage`,
      sicu_transferred=`Patient transferred to SICU post op`)] %>% 
    .[, hospitalization_time:=gsub(' days', '', hospitalization_time) %>% as.numeric] ->
    rev_patient_data_
  
  clinical_data[, .(prior_spine_surgery=`Prior Spine Surgery`),
    .(patient_id=`Code of the patient`)] ->
    prior_surgery
  
  complications %>% 
    merge(rev_patient_data_, by=c('patient_id', 'date')) %>% 
    merge(prior_surgery, by='patient_id') ->
    category_los
  
  return(category_los)
}

get_complications <- function(complications){
  complications %>% 
    .[`Complication Type`=='Primary'] %>%
    .[, .(
      category=`Category of the complication`,
      days_since_surgery=`Days since surgery`,
      patient_id=`Code of the patient`,
      date=`Date of Reoperation`
    )] %>% 
    na.omit %>% 
    setorder(patient_id, days_since_surgery) %>% 
    .[, .(
      category, date, days_since_surgery,
      reintervention=calc_reintervention(days_since_surgery)), 
      patient_id] 
}

get_time_evolution <- function(clinical_data, rev_patient_data){
  rev_patient_data %>% 
    .[, `Schwab Type 3: Number` := `Schwab Type 3: Number` %>%  as.numeric] %>% 
    .[, `Schwab Type 4: Number` := `Schwab Type 4: Number` %>%  as.numeric] %>% 
    .[, `Schwab Type 5: Number` := `Schwab Type 5: Number` %>%  as.numeric] %>% 
    .[, co3 := 0] %>% 
    .[`Schwab Type 3: Number` + `Schwab Type 4: Number` + `Schwab Type 5: Number` > 0, co3 := 1] %>% 
    .[, .(
      patient_id=`Code of the patient`, 
      stage_date=`st1. Date of Stage`,
      surgical_time=`Total surgical time st1+st2+st3`,
      blood_loss=`Total blood loss (mL)`,
      hospitalization_time=`Hospitalization time`,
      sicu_transferred=`Patient transferred to SICU post op`,
      sicu_los=`SICU length of stay`,
      co3,
      `Infection debridement`,
      `Implant removal`,
      `Anterior Instrumented Fusion`,
      `Number of Anterior Instrumented Levels`,
      `Posterior Instrumented Fusion`,
      `Number of Posterior Instrumented Levels`,
      `Decompression`,
      `Number of decompressions`,
      `Interbody Fusion`,
      `Number of Interbody Fusions`,
      `Osteotomy`,
      `Rod change`,
      `Scar revision`
    )] %>% 
    .[, hospitalization_time:=gsub(' days', '', hospitalization_time) %>% as.numeric]->
    time_evolution
  
  clinical_data %>% 
    .[, co3 := 0] %>% 
    .[`Schwab Type 3: Number` + `Schwab Type 4: Number` + `Schwab Type 5: Number` > 0, co3 := 1] %>% 
    .[, .(
      patient_id=`Code of the patient`, 
      stage_date=`st1. Date of Stage 1`,
      surgical_time=`Total surgical time st1+st2+st3`,
      blood_loss= `Total Operative Blood Loss st1+st2+st3`,
      hospitalization_time=`Hospitalization time`,
      sicu_transferred=`Patient transferred to SICU post op`,
      sicu_los=`SICU length of stay`,
      co3,
      `Infection debridement`,
      `Implant removal`,
      `Anterior Instrumented Fusion`,
      `Number of Anterior Instrumented Levels`,
      `Posterior Instrumented Fusion`,
      `Number of Posterior Instrumented Levels`,
      `Decompression`,
      `Number of decompressions`,
      `Interbody Fusion`,
      `Number of Interbody Fusions`,
      `Osteotomy`,
      `Rod change`,
      `Scar revision`
    )] %>% 
    rbind(time_evolution) ->
    time_evolution
  
  time_evolution %<>% 
    .[is.na(sicu_los) & is.na(sicu_transferred), sicu_transferred:='No'] %>% 
    .[!is.na(stage_date)] %>% 
    .[, diff_years:=difftime(stage_date, min(stage_date), units = "days")/365, patient_id] %>% 
    .[, diff_years:=as.numeric(diff_years)] %>% 
    setorder(patient_id, diff_years) %>% 
    .[, reintervention:=factor(0:(.N-1)), patient_id]
  
  time_evolution
}

calc_reintervention <- function(days_since_surgery){
  days_since_surgery %>% 
    unique %>% 
    sort %>% 
    data.table(days=., position=1:length(.)) %>% 
    merge(data.table(days=days_since_surgery)) %>% 
    .[, position]
}


plot_evolution <- function(dt, value_title=''){
  dt %>% 
    .[, .(
      prop=mean(condition),
      x = sum(condition),
      N=.N),
      reintervention] %>% 
    .[, ci:=1.96*sqrt(prop*(1-prop)/N)] ->
    res
  
  res %>% 
    .[, ymin:=binconf(x, N)[2], 1:nrow(res)] %>% 
    .[, ymax:=binconf(x, N)[3], 1:nrow(res)] %>% 
    # .[, ymin:=max(prop-ci, 0), 1:nrow(res)] %>% 
    # .[, ymax:=min(prop+ci, 1), 1:nrow(res)] %>% 
    ggplot(aes(reintervention, y=prop, group=1)) +
    geom_line() +
    geom_ribbon(
      aes(ymin=ymin, ymax=ymax), 
      linetype=2, alpha=0.1) +
    ggtitle('Proportion of {value_title}' %>% f) +
    ylab('%') +
    ylim(c(-0.001, 1.001)) +
    xlab('Reintervention') ->
    res_plot
  
  dt %>%
    copy() %>% 
    .[, reintervention:= as.numeric(as.character(reintervention))] %>% 
    .[reintervention <= 3] %>% 
    .[, condition:=ifelse(condition == TRUE, 1, 0)] ->
    lm_data
  
  glm(condition~reintervention, data=lm_data) %>% 
    summary %>% coefficients ->
    res_lm
  
  return(list(
    res_plot=res_plot,
    res_lm=res_lm
  ))
}

estimate_dilution <- function(res_, outcome, controlling_vars_, sel_years=1:3){
  reg_formula <- "{outcome} ~ ." %>% f %>% formula
  
  res_ %>% 
    .[outcome_year %in% sel_years] %>% 
    .[, .SD, .SDcols=c(
      outcome, controlling_vars_, 'reint_n',
      'outcome_year', 'years_last_op')
    ] %>% 
    na.omit ->
    res_cols
  
  cols <- colnames(res_cols)
  inds <- which(res_cols[, sapply(.SD, . %>% na.omit %>% uniqueN)] > 1)
  res_cols %>% 
    .[, .SD, .SDcols = cols[inds]] %>% 
    lm(reg_formula, data=.) %>% summary %>% coefficients() ->
    lm_res
  
  ind <- which(row.names(lm_res) == 'years_last_op')
  stat_res <- data.frame(lm_res)[ind, c(1, 2, 4)] %>% round(3)
  if(nrow(stat_res) > 0 ){
    colnames(stat_res) <- c('Estimate', 'StdError', 'p_value')
    row.names(stat_res) <- NULL
  }else{
    stat_res <- data.frame(
      Estimate=NA,
      StdError=NA,
      p_value=NA
    )
  }
  
  return(stat_res)
}

estimate_impacts <- function(res_, outcome, impact_var, controlling_vars_, sel_years=1:5){
  
  reg_formula <- "{outcome} ~ ." %>% f %>% formula
  
  coef_res <- data.frame()
  for( y_ in sel_years ){
    res_[outcome_year==y_] %>% 
      .[, .SD, .SDcols=c(outcome, impact_var, controlling_vars_)] %>% 
      na.omit ->
      res_cols
    
    cols <- colnames(res_cols)
    inds <- which(res_cols[, sapply(.SD, . %>% na.omit %>% uniqueN)] > 1)
    res_cols %>% 
      .[, .SD, .SDcols = cols[inds]] %>% 
      lm(reg_formula, data=.) %>% summary %>% coefficients() ->
      lm_res
    
    ind <- which(row.names(lm_res) == impact_var)
    stat_res <- data.frame(lm_res)[ind, c(1, 2, 4)] %>% round(3)
    if(nrow(stat_res) > 0 ){
      colnames(stat_res) <- c('Estimate', 'StdError', 'p_value')
      row.names(stat_res) <- NULL
    }else{
      stat_res <- data.frame(
        Estimate=NA,
        StdError=NA,
        p_value=NA
      )
    }
    stat_res$outcome_year <- y_      
    
    coef_res %<>% rbind(stat_res)
  }
  
  coef_res
}
