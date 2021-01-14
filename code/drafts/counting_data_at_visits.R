clinical_data[, .(.N, visit_6w=sum(!is.na(`6W. ODI - Score (%)`)))]
clinical_data[, .(.N, visit_2y=sum(!is.na(`2Y. ODI - Score (%)`)))]
clinical_data[, .(.N, visit_2y_or_3y=sum(
  !(is.na(`2Y. ODI - Score (%)`) & 
      is.na(`3Y. ODI - Score (%)`))))]

clinical_data[, .(.N, visit_2y_or_3y=sum(
  !(is.na(`2Y. ODI - Score (%)`) & 
      is.na(`3Y. ODI - Score (%)`) & 
        is.na(`4Y. ODI - Score (%)`))))]


clinical_data[, .(.N, visit_2y=sum(!is.na(`2Y. ODI - Score (%)`)))]
clinical_data[, .(.N, visit_2y_or_3y=sum(!(is.na(`2Y. ODI - Score (%)`) & is.na(`3Y. ODI - Score (%)`))))]

valid_patients <- clinical_data[
  `st1. Date of Stage 1` %>% as.Date() < as.Date('2015-12-15'), 
  `Code of the patient` %>% unique
]
clinical_data[`Code of the patient` %in% valid_patients] ->
  sel_data

sel_data[, .(.N, visit_5y_or_6y=sum(!(is.na(`5Y. ODI - Score (%)`) & is.na(`6Y. ODI - Score (%)`))))]


## Alif

sel_data[, .(.N, visit_6w=sum(!is.na(`6 WEEKS VISIT - Date of visit`))), Alif]
sel_data[, .(.N, visit_2y_or_3y=sum(
  !(is.na(`2Y. ODI - Score (%)`) & 
      is.na(`3Y. ODI - Score (%)`)))), Alif]


valid_patients <- clinical_data[
  `st1. Date of Stage 1` %>% as.Date() < as.Date('2015-12-15'), 
  `Code of the patient` %>% unique
]
sel_data[`Code of the patient` %in% valid_patients] ->
  sel_data_5

sel_data_5[, .(.N, visit_5y_or_6y=sum(
  !(is.na(`2Y. ODI - Score (%)`) & 
      is.na(`3Y. ODI - Score (%)`)))), Alif]
