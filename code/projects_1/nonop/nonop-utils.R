library(readxl)
library(data.table)
library(magrittr)
library(yaml)


source('code/basic.R')
source('code/utils.R')

get_data <- function(){
  
  xls_path <- 'data/ESSG extraction July 2020_3.xlsx'
  # excel_sheets(xls_path)
  
  matching_vars <- read_yaml('code/nonop/matching_vars.yml')
  
  clinical_data <- read_excel(xls_path) %>% 
    data.table() 
  
  all_vars <- matching_vars %>% unlist
  
  sel_data <- clinical_data %>% 
    .[Study == 'NonOp'] %>% 
    .[, opnonop_categoric:='No'] %>% 
    .[!is.na(`Op/NonOp`), opnonop_categoric:='Yes'] %>% 
    calc_frailty %>% 
    .[, .SD, .SDcols = c(all_vars, 'opnonop_categoric', 'frailty_index')] 
  
  # cols_to_remove <- sel_data[, sapply(.SD, function(col) uniqueN(col) == 1)]
  # print('Columns removed because constant value')
  # print(names(cols_to_remove)[cols_to_remove])
  
  matching_vars$covariates <- c(matching_vars$covariates, 'opnonop_categoric', 'frailty_index')
  
  return(list(
    sel_data,
    matching_vars
  ))
}

calc_frailty <- function(sel_data){
  sel_data[, frailty:=0]
  sel_data[, count:=0]
  sel_data[, comorbidities:=0]
  
  sel_data[`BMI_First Visit` > 30 | `BMI_First Visit` < 25, frailty:=frailty+1]
  sel_data[`BMI_First Visit` > 30 | `BMI_First Visit` < 25, comorbidities:=comorbidities+1]
  sel_data[!is.na(`BMI_First Visit` > 30 | `BMI_First Visit` < 25), count:=count+1]
  
  sel_data[`Bowel incontinence`=='Yes', frailty:=frailty+1]
  sel_data[`Bowel incontinence`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Bowel incontinence`), count:=count+1]
  
  sel_data[`Leg weakness`=='Yes', frailty:=frailty+1]
  sel_data[`Leg weakness`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Leg weakness`), count:=count+1]
  
  sel_data[`Bladder incontinence`=='Yes', frailty:=frailty+1]
  sel_data[`Bladder incontinence`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Bladder incontinence`), count:=count+1]
  
  sel_data[`Diabetes without complications`=='Yes', frailty:=frailty+1]
  sel_data[`Diabetes without complications`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Diabetes without complications`), count:=count+1]
  
  sel_data[`Any non-metastatic solid tumor`=='Yes', frailty:=frailty+1]
  sel_data[`Any non-metastatic solid tumor`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Any non-metastatic solid tumor`), count:=count+1]
  
  sel_data[`Metastatic solid tumor`=='Yes', frailty:=frailty+1]
  sel_data[`Metastatic solid tumor`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Metastatic solid tumor`=='Yes'), count:=count+1]
  
  sel_data[Cardiac=='Yes', frailty:=frailty+1]
  sel_data[Cardiac=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(Cardiac), count:=count+1]
  
  sel_data[Hypertension=='Yes', frailty:=frailty+1]
  sel_data[Hypertension=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(Hypertension), count:=count+1]
  
  sel_data[`Mild liver disease`=='Yes', frailty:=frailty+1]
  sel_data[`Mild liver disease`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Mild liver disease`), count:=count+1]
  
  sel_data[`Chronic pulmonary disease`=='Yes', frailty:=frailty+1]
  sel_data[`Chronic pulmonary disease`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Chronic pulmonary disease`), count:=count+1]
  
  sel_data[`Peripheral vascular disease`=='Yes', frailty:=frailty+1]
  sel_data[`Peripheral vascular disease`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Peripheral vascular disease`), count:=count+1]
  
  sel_data[`Osteoporosis / osteopenia`=='Yes', frailty:=frailty+1]
  sel_data[`Osteoporosis / osteopenia`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Osteoporosis / osteopenia`), count:=count+1]
  
  sel_data[`Peripheral vascular disease`=='Yes', frailty:=frailty+1]
  sel_data[`Peripheral vascular disease`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Peripheral vascular disease`), count:=count+1]
  
  sel_data[`Depression / anxiety`=='Yes', frailty:=frailty+1]
  sel_data[`Depression / anxiety`=='Yes', comorbidities:=comorbidities+1]
  sel_data[!is.na(`Depression / anxiety`), count:=count+1]
  
  sel_data[`Occupation_First Visit`=="Retired due to back pain (permanent)", frailty:=frailty+1]
  sel_data[`Occupation_First Visit`=="Retired due to back pain (permanent)", comorbidities:=comorbidities+1]
  sel_data[!is.na(`Occupation_First Visit`), count:=count+1]
  
  sel_data[comorbidities>3, frailty:=frailty+1]
  sel_data[!is.na(comorbidities), count:=count+1]
  
  sel_data[`SF36-1 _First Visit` > 4, frailty:=frailty+1]
  sel_data[!is.na(`SF36-1 _First Visit`), count:=count+1]
  
  sel_data[`SF36 -2_First Visit` < 3, frailty:=frailty+1]
  sel_data[!is.na(`SF36 -2_First Visit`), count:=count+1]
  
  sel_data[`SF36 -3b_First Visit` < 3, frailty:=frailty+1]
  sel_data[!is.na(`SF36 -3b_First Visit`), count:=count+1]
  
  sel_data[(`SF36 -3c_First Visit` < 3) | (`ODI_Lifting_First Visit` > 2), frailty:=frailty+1]
  sel_data[!is.na((`SF36 -3c_First Visit` < 3) | (`ODI_Lifting_First Visit` > 2)), count:=count+1]
  
  sel_data[`SF36 -3e_First Visit` < 3, frailty:=frailty+1]
  sel_data[!is.na(`SF36 -3e_First Visit`), count:=count+1]
  
  sel_data[(`SF36 -3i_First Visit` < 3 ) | (`ODI_Walking_First Visit` > 2), frailty:=frailty+1]
  sel_data[!is.na((`SF36 -3i_First Visit` < 3 ) | (`ODI_Walking_First Visit` > 2)), count:=count+1]
  
  sel_data[(`SF36 -3j_First Visit` < 3), frailty:=frailty+1]
  sel_data[!is.na((`SF36 -3j_First Visit` < 3)), count:=count+1]
  
  sel_data[(`SF36 -9c_First Visit` < 3) | (`SRS22 - 7_First Visit` < 3), frailty:=frailty+1]
  sel_data[!is.na((`SF36 -9c_First Visit` < 3) | (`SRS22 - 7_First Visit` < 3)), count:=count+1]
  
  sel_data[(`SF36 -9f_First Visit` < 3) | (`SRS22 - 16_First Visit` < 3), frailty:=frailty+1]
  sel_data[!is.na((`SF36 -9f_First Visit` < 3) | (`SRS22 - 16_First Visit` < 3)), count:=count+1]
  
  sel_data[`SF36 -9g_First Visit` < 3, frailty:=frailty+1]
  sel_data[!is.na(`SF36 -9g_First Visit` < 3), count:=count+1]
  
  sel_data[`SF36 -9i_First Visit` < 3, frailty:=frailty+1]
  sel_data[!is.na(`SF36 -9i_First Visit` < 3), count:=count+1]
  
  sel_data[`SF36 11d_First Visit` > 3, frailty:=frailty+1]
  sel_data[!is.na(`SF36 11d_First Visit` > 3), count:=count+1]
  
  sel_data[`ODI_Personal Care_First Visit` > 2, frailty:=frailty+1]
  sel_data[!is.na(`ODI_Personal Care_First Visit` > 2), count:=count+1]
  
  sel_data[`ODI_Sleeping_First Visit` > 2, frailty:=frailty+1]
  sel_data[!is.na(`ODI_Sleeping_First Visit` > 2), count:=count+1]
  
  sel_data[(`ODI_Social Life_First Visit` > 2) | (`SRS22 - 14_First Visit` < 3), frailty:=frailty+1]
  sel_data[!is.na((`ODI_Social Life_First Visit` > 2) | (`SRS22 - 14_First Visit` < 3)), count:=count+1]
  
  sel_data[(`ODI_Social Life_First Visit` > 2) | (`SRS22 - 18_First Visit` < 3), frailty:=frailty+1]
  sel_data[!is.na((`ODI_Social Life_First Visit` > 2) | (`SRS22 - 18_First Visit` < 3)), count:=count+1]
  
  sel_data[`ODI_Traveling_First Visit` > 2, frailty:=frailty+1]
  sel_data[!is.na(`ODI_Traveling_First Visit`), count:=count+1]
  
  sel_data[(`SRS22 - 9_First Visit` < 3), frailty:=frailty+1] 
  sel_data[!is.na(`SRS22 - 9_First Visit`), count:=count+1]
  
  sel_data[(`SRS22 - 12_First Visit` < 3), frailty:=frailty+1] 
  sel_data[!is.na(`SRS22 - 12_First Visit`), count:=count+1]
  
  
  sel_data[, frailty_index:=frailty/count]
  
  sel_data
}












