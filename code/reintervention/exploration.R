library(readxl)
library(magrittr)
library(data.table)
library(stringr)
library(yaml)
library(GGally)

source('code/basic.R')
source('code/utils.R')

xls_path <- 'data/ESSG extraction July 2020_2.xlsx'
# excel_sheets(xls_path)

vars_ <- read_yaml('code/reintervention/treatment_vars.yml')

rev_patient_data <- read_excel(xls_path, sheet = "Rev surgeries") %>% 
  data.table 
clinical_data <- read_excel(xls_path) %>% 
  data.table() 
complications <- read_excel(xls_path, sheet = "Complications") %>% 
  data.table 

complications[
  `Complication Impact`=='Major Complication', 
  .(patient_id=`Code of the patient`, 
    category=`Category of the complication`, 
    surgery=`Complication associated to surgery`,
    days=`Days since surgery`)] %>% 
  setorder(patient_id, days) %>% 
  .[, reintervention:=factor(1:.N), patient_id] ->
  complications_

rev_patient_data[, 
  .(patient_id=`Code of the patient`, 
    surgery=`Surgery number`, 
    hospitalization_time=`Hospitalization time`)] %>% 
  .[, hospitalization_time:=gsub(' days', '', hospitalization_time) %>% as.numeric] ->
  rev_patient_data_

complications_ %>% 
  merge(rev_patient_data_, by=c('patient_id', 'surgery')) ->
  category_los

category_los[, 
  .(hospitalization_time=mean(hospitalization_time, na.rm = TRUE), 
    hosp_t_sd=sd(hospitalization_time, na.rm=TRUE)), 
  .(reintervention, category)] %>% 
  .[is.na(hosp_t_sd), hosp_t_sd:=0] ->
  res_

res_[,
  .(hospitalization_time, 
    h_up=hospitalization_time + 1.96*hosp_t_sd,
    h_down=max(hospitalization_time - 1.96*hosp_t_sd, 0),
    reintervention,
    category), 1:nrow(res_)] %>% 
  .[category!='Central neurologic complications'] %>% 
  ggplot(aes(reintervention, hospitalization_time, group=category, color=category, fill=category)) +
  geom_line() +
  geom_ribbon(
    aes(ymin=h_down, ymax=h_up), 
    linetype=2, alpha=0.1) +
  facet_grid(category~.) +
  ylab('Hospitalization time')


category_los[, 
  reintervention_total:=.N, 
  .(reintervention)] %>% 
  .[, .(prop=100*.N/reintervention_total), .(reintervention, category)] %>% 
  ggplot(aes(reintervention, prop, color=category, group=category)) +
  geom_line() +
  ylab('%') +
  ggtitle('Proportion of categories')
