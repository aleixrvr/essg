library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(glue)

OUTCOME_NAMES <- c('ALIF', 'PLIF', 'TLIF')

xls_path <- 'data/ESSG extraction July 2020_2.xlsx'
excel_sheets(xls_path)

clinical_data <- read_excel(xls_path) %>% 
  as.data.table()

setnames(clinical_data, '3CO', 'CO3')
setnames(clinical_data, '3CO number', 'CO3_number')
setnames(clinical_data, '1st surgeon: experience in ASD surgery', 'first_surgeon')
setnames(clinical_data, 'Levels Previously operated - Upper', 'Prev_op_up')
setnames(clinical_data, 'Levels Previously operated - Lower', 'Prev_op_low')


colnames(clinical_data)

matching_vars <- read_yaml('matching_vars.yml')
# clinical_names <- names(clinical_data)
# for( var in matching_vars ){
#   trobada <- which(clinical_names == var)
#   if( length(trobada) == 0 ){
#     trobada <- 0
#   }
#   glue("{var}: {trobada}") %>% print
# }

grep('LGAP', clinical_names)
clinical_names[grep('umber', clinical_names)]

clinical_data[, ALIF] %>% na.omit %>% sum
clinical_data[, TLIF] %>% na.omit %>% sum
clinical_data[, PLIF] %>% na.omit %>% sum

clinical_data %>% 
  .[ALIF + TLIF + PLIF > 0] %>% 
  .[, done_alif := ifelse(ALIF > 0, TRUE, FALSE)] %>% 
  .[, .SD, .SDcols = c('done_alif', matching_vars)]->
  sel_data

done_alif <- sel_data$done_alif
for( sel_var_name in matching_vars ){
  sel_var <- sel_data[[sel_var_name]] 
  if( class(sel_var) == 'character' ){
    cat(sel_var_name)
    print(table(done_alif, sel_var))
    cat('\n\n')
  }
}


clinical_data[CO3=='No', ALIF] %>% na.omit %>% sum
clinical_data[CO3=='No', TLIF] %>% na.omit %>% sum
clinical_data[CO3=='No', PLIF] %>% na.omit %>% sum

sel_cols <- colnames(sel_data)
ind_excl <- which(sel_cols %in% c('LDI', 'Roussouly Type'))
sel_cols <- sel_cols[-ind_excl]

sel_data %>% 
  .[, .SD, .SDcols = sel_cols] %>% 
  na.omit ->
  sel_data_reg

log_model <- glm(done_alif~., data=sel_data_reg, family='binomial') 
predictions <- predict(log_model, sel_data_reg, type='response')

data.frame(predictions, alif=sel_data_reg$done_alif) %>% 
  ggplot(aes(predictions, fill=alif, color=alif)) +
  geom_density(alpha = 0.1)
