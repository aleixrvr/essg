library(readxl)
library(magrittr)
library(data.table)
library(stringr)
library(yaml)
library(ggplot2)


get_data <- function(only_two_years=TRUE){
  xls_path <- 'data/ESSG extraction July 2020_3.xlsx'
  # excel_sheets(xls_path)
  
  vars_ <- read_yaml('code/reintervention/treatment_vars.yml')
  
  rev_patient_data <- read_excel(xls_path, sheet = "Rev surgeries") %>% 
    data.table 
  clinical_data <- read_excel(xls_path) %>% 
    data.table() 
  complications <- read_excel(xls_path, sheet = "Complications") %>% 
    data.table 
  
  if( only_two_years ){
    valid_patients <- clinical_data[
      `st1. Date of Stage 1` %>% as.Date() < as.Date('2018-07-31'), 
      `Code of the patient` %>% unique
    ]
    clinical_data %<>% .[`Code of the patient` %in% valid_patients]
    rev_patient_data %<>% .[`Code of the patient` %in% valid_patients]
    complications %<>% .[`Code of the patient` %in% valid_patients]
  }
  
  time_evolution <- get_time_evolution(clinical_data, rev_patient_data)
  complications <- get_complications(complications)
  category_los <- get_category_los(clinical_data, rev_patient_data, complications)
  
  return(list(
    time_evolution, 
    complications, 
    category_los,
    clinical_data
  ))
}

get_category_los <- function(clinical_data, rev_patient_data, complications){
  rev_patient_data[, 
    .(patient_id=`Code of the patient`, 
      hospitalization_time=`Hospitalization time`, 
      blood_loss=`Total blood loss (mL) st1+st2+st3`,
      surgical_time=`Total surgical time st1+st2+st3`,
      date=`st1. Date of Stage 1`,
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
    .[, `Number of Schwab Type 3` := `Number of Schwab Type 3` %>%  as.numeric] %>% 
    .[, `Number of Schwab Type 4` := `Number of Schwab Type 4` %>%  as.numeric] %>% 
    .[, `Number of Schwab Type 5` := `Number of Schwab Type 5` %>%  as.numeric] %>% 
    .[, co3 := 0] %>% 
    .[`Number of Schwab Type 3` + `Number of Schwab Type 4` + `Number of Schwab Type 5` > 0, co3 := 1] %>% 
    .[, .(
      patient_id=`Code of the patient`, 
      stage_date=`st1. Date of Stage 1`,
      surgical_time=`Total surgical time st1+st2+st3`,
      blood_loss=`Total blood loss (mL) st1+st2+st3`,
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
      blood_loss= `Total blood loss (mL) st1+st2+st3`,
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
      N=.N),
      reintervention] %>% 
    .[, ci:=1.96*sqrt(prop*(1-prop)/N)] ->
    res
  res %>% 
    .[, ymin:=max(prop-ci, 0), 1:nrow(res)] %>% 
    .[, ymax:=min(prop+ci, 1), 1:nrow(res)] %>% 
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
  
  return(res_plot)
}

estimate_impacts <- function(res, outcome, impact_var, controlling_vars_){
  reg_formula <- "{outcome} ~ ." %>% f %>% formula
  coef_res <- data.frame()
  for( y_ in 1:6 ){
    res[outcome_year==y_] %>% 
      .[, .SD, .SDcols=c(outcome, impact_var, controlling_vars_)] %>% 
      lm(reg_formula, data=.) %>% summary %>% coefficients() ->
      lm_res
    
    ind <- which(row.names(lm_res) == impact_var)
    stat_res <- data.frame(lm_res)[ind, c(1, 2, 4)] %>% round(3)
    colnames(stat_res) <- c('Estimate', 'StdError', 'p_value')
    row.names(stat_res) <- NULL
    stat_res$outcome_year <- y_
    coef_res %<>% rbind(stat_res)
  }
  
  coef_res
}
