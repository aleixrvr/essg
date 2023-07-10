library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(stringr)

source('code/projects_1/basic.R')
source('code/projects_1/utils.R')


get_data <- function(){
  
  matching_vars <- read_yaml('code/projects_1/fenestrated/matching_vars.yml')
  
  comps <- fread('data/Fenestrated screws/Complications tots.csv') 
  
  comps %>% 
    .[, `Code of the patient`] %>% 
    unique ->
    patients_complications
  
  comps %>% 
    .[`Complication Impact`=='Major Complication', `Code of the patient`] %>%
    unique ->
    patients_major_complications
  
  comps %>% 
    .[`Category of the complication`=='Mechanical complications', `Code of the patient`] %>%
    unique ->
    patients_mechanical_complications
  
  comps[, .(reinterventions_n = sum(!is.na(`Date of Reoperation`))), `Code of the patient`] ->
    reinterventions
  
  reinterventions[reinterventions_n>0, `Code of the patient`] %>%
    unique ->
    patients_reinterventions
  
  fread('data/Fenestrated screws/DPS ops.csv') %>% 
    setnames('3CO', 'CO3') %>% 
    .[, fenestrated:=`Fenestrated screws with or without cement`] %>% 
    .[, `Fenestrated screws with or without cement`:=NULL] %>% 
    .[`ASA classification` > 1] %>% 
    .[`ASA classification` < 4] %>% 
    .[Age > 50] %>% 
    .[, complication:='No'] %>% 
    .[`Code of the patient` %in% patients_complications, complication :='Yes'] %>% 
    .[, major_complication:='No'] %>% 
    .[`Code of the patient` %in% patients_major_complications, major_complication :='Yes'] %>% 
    .[, mechanical_complication:='No'] %>% 
    .[`Code of the patient` %in% patients_mechanical_complications, mechanical_complication :='Yes'] %>% 
    .[, has_reintervention:='No'] %>% 
    .[`Code of the patient` %in% patients_reinterventions, has_reintervention :='Yes'] %>% 
    .[, 
      `Global Tilt`:= `Global Tilt` %>% 
        str_replace_all(',' %>% fixed, '.') %>% 
        as.numeric
    ] %>% 
    .[`Global Tilt`< 90] %>% 
    clean_data(matching_vars$covariates) ->
    sel_data
  
  rows_to_remove <- colnames(sel_data) %>% duplicated() %>% which
  sel_data %<>% .[, -rows_to_remove, with=FALSE] 

  matching_vars$covariates %<>% c('fenestrated')
  
  sel_data %<>% 
    merge(
      reinterventions, by='Code of the patient', all.x=TRUE, all.y=FALSE
    )
  
  return(list(
    sel_data,
    matching_vars
  ))
}

# 
## Stats
# comps <- fread('data/Fenestrated screws/Complications tots.csv')
# 
# comps %>%
#   .[, `Code of the patient`] %>%
#   unique ->
#   patients_complications
# 
# comps %>%
#   .[`Complication Impact`=='Major Complication', `Code of the patient`] %>%
#   unique ->
#   patients_major_complications
# 
# comps %>%
#   .[`Complication Impact`=='Minor Complication', `Code of the patient`] %>%
#   unique ->
#   patients_minor_complications
# 
# comps[!is.na(`Date of Reoperation`), `Code of the patient`] %>%
#   unique ->
#   patients_reinterventions
# 
# dps <- fread('data/Fenestrated screws/DPS ops.csv') %>%
#   setnames('Fenestrated screws with or without cement', 'fenestrated') %>%
#   .[, complication:='No'] %>%
#   .[`Code of the patient` %in% patients_complications, complication :='Yes'] %>%
#   .[, major_complication:='No'] %>%
#   .[`Code of the patient` %in% patients_major_complications, major_complication :='Yes'] %>%
#   .[, minor_complication:='No'] %>%
#   .[`Code of the patient` %in% patients_minor_complications, minor_complication :='Yes'] %>%
#   .[, reintervention:='No'] %>%
#   .[`Code of the patient` %in% patients_reinterventions, reintervention :='Yes']
# 
# 
# fenestrated_patients <- dps[
#   fenestrated == 'Yes',
#   `Code of the patient`]
# 
# 
# comps[`Code of the patient` %in% fenestrated_patients, .N, `Complication Impact`]
# 
# comps %>%
#   .[`Code of the patient` %in% fenestrated_patients, .N,
#     c("Name of the complication", "Complication Impact")] %>%
#   .[, proportion := round(N/sum(N)*100, 1)]  %>%
#   dcast(`Name of the complication` ~ `Complication Impact`, value.var=c('N', 'proportion'), fill=0) %>%
#   fwrite('code/fenestrated/stats_complications_by_type.csv')
# 
# dps[, .(
#   .N,
#   complications=mean(complication=='Yes') * 100,
#   complications_sd=sd(complication=='Yes') * 100,
#   major_complications=mean(major_complication=='Yes') *100,
#   major_complications_sd=sd(major_complication=='Yes') * 100,
#   minor_complications=mean(minor_complication=='Yes') *100,
#   minor_complications_sd=sd(minor_complication=='Yes') * 100,
#   reinterventions=mean(reintervention=='Yes') *100,
#   reinterventions_sd=sd(reintervention=='Yes') * 100
# ), fenestrated] %>%
#   fwrite('code/fenestrated/stats_complications.csv')
# 


# 
# 
# dps[, .(complications=sum(complication=='Yes'), .N), fenestrated] %>%
#   as.data.frame %>% .[, c(2, 3)] %>% t -> res
# 
# res_test <- prop.test(x = res[1, ], n = res[2, ])
# p_vals_data <- data.frame(complications=res_test$p.value)
# 
# dps[, .(sum(major_complication=='Yes'), .N), fenestrated] %>%
#   as.data.frame %>% .[, c(2, 3)] %>% t -> res
# 
# res_test <- prop.test(x = res[1, ], n = res[2, ])
# p_vals_data$major_complications <- res_test$p.value
# 
# dps[, .(sum(minor_complication=='Yes'), .N), fenestrated] %>%
#   as.data.frame %>% .[, c(2, 3)] %>% t -> res
# 
# res_test <- prop.test(x = res[1, ], n = res[2, ])
# p_vals_data$minor_complications <- res_test$p.value
# 
# dps[, .(sum(reintervention=='Yes'), .N), fenestrated] %>%
#   as.data.frame %>% .[, c(2, 3)] %>% t -> res
# 
# res_test <- prop.test(x = res[1, ], n = res[2, ])
# p_vals_data$reinterventions <- res_test$p.value
# 
# p_vals_data %>% fwrite('code/fenestrated/stats_complications_p_vals.csv')
