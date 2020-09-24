library(magrittr)
library(data.table)
library(ggplot2)

source('code/basic.R')
source('code/utils.R')


xls_path <- 'data/ESSG extraction July 2020_2.xlsx'
# excel_sheets(xls_path)

# vars_ <- read_yaml('code/nonop/treatment_vars.yml')

clinical_data <- read_excel(xls_path) %>% 
  data.table() 


clinical_names <- colnames(clinical_data)
clinical_names[grepl('Prior Spin', clinical_names)]

followup_data <- clinical_data %>% 
  clean_names_dt %>% 
  .[study == 'NonOp']


var_ <- 'age'
provide_stats <- function(dt, var_){
  dt_plot <- dt %>% 
    ggplot(aes_string(var_, fill='opnonop_categoric', color='opnonop_categoric')) +
    geom_density(alpha=0.1) +
    ggtitle('{var_} distribution' %>% f) 
  
  dt_stats <- dt[, .(mean=mean(get(var_)), sd=sd(get(var_))) , opnonop_categoric] 
  
  return(list(dt_plot, dt_stats))
}


