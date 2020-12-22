library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(stringr)

source('code/basic.R')
source('code/utils.R')


get_data <- function(){
  
  matching_vars <- read_yaml('code/fenestrated/matching_vars.yml')
  
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
  
  return(list(
    sel_data,
    matching_vars
  ))
}

# 
# ## Stats
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
# dps <- fread('data/Fenestrated screws/DPS ops.csv') %>%
#   setnames('Fenestrated screws with or without cement', 'fenestrated') %>%
#   .[, complication:='No'] %>%
#   .[`Code of the patient` %in% patients_complications, complication :='Yes'] %>%
#   .[, major_complication:='No'] %>%
#   .[`Code of the patient` %in% patients_major_complications, major_complication :='Yes']
# 
# fenestrated_patients <- dps[
#   fenestrated == 'Yes',
#   `Code of the patient`]
# 
# 
# comps[`Code of the patient` %in% fenestrated_patients, .N, `Complication Impact`]
# 
# comps %>%
#   .[`Code of the patient` %in% fenestrated_patients, .N, `Name of the complication`] %>%
#   .[, proportion := round(N/sum(N)*100, 1)]  %>%
#   fwrite('code/fenestrated/stats_complications.csv')
# 
# dps[, .(
#   .N,
#   complications=mean(complication=='Yes') * 100,
#   complications_sd=sd(complication=='Yes') * 100,
#   major_complications=mean(major_complication=='Yes') *100,
#   major_complications_sd=sd(major_complication=='Yes') * 100
# ), fenestrated]
# 
# dps[fenestrated=='Yes', complication]
# 
# dps[, .(complications=sum(complication=='Yes'), .N), fenestrated] %>%
#   as.data.frame %>% .[, c(2, 3)] %>% t -> res
# 
# res_test <- prop.test(x = res[1, ], n = res[2, ])
# res_test$p.value
# 
# dps[, .(complications=sum(major_complication=='Yes'), .N), fenestrated] %>%
#   as.data.frame %>% .[, c(2, 3)] %>% t -> res
# 
# res_test <- prop.test(x = res[1, ], n = res[2, ])
# res_test$p.value
