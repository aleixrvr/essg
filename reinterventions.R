library(readxl)
library(magrittr)
library(data.table)
library(stringr)
library(yaml)

source('code/basic.R')
source('code/utils.R')

xls_path <- 'data/ESSG extraction July 2020_2.xlsx'
# excel_sheets(xls_path)

clinical_data <- read_excel(xls_path) %>% 
  as.data.table()

treatment_vars <- read_yaml('code/reintervention/treatment_vars.yml')

clinical_data %>% 
  .[, .SD, .SDcols=treatment_vars] %>% 
  clean_names_dt %>% 
  .[, co3 := 0] %>% 
  .[schwab_type_3_number + schwab_type_4_number + schwab_type_5_number > 0, co3 := 1] %>% 
  .[, schwab_type_3_number := NULL] %>% 
  .[, schwab_type_4_number := NULL] %>% 
  .[, schwab_type_5_number := NULL] %>% 
  .[, pelvic_fixation := 0] %>% 
  .[grepl('Iliac', posterior_instrumented_fusion_upper_lower_levels, fixed=TRUE), 
    pelvic_fixation := 1] %>% 
  .[, posterior_instrumented_fusion_upper_lower_levels:=NULL] ->
  treats
  
factor_cols <- colnames(treats)[ treats %>% sapply(is.character) ]

treats[, .SD, .SDcols=factor_cols] -> facts
facts %>% lapply(uniqueN)
facts[, surgical_approach %>% unique]

melt(facts, measure.vars=factor_cols)
facts[, c(1, 2)] -> res

res %>% table %>% melt -> res


factor_cols_n <- len(factor_cols)
melted_data <- data.table()
for( var1_pos in 1:(factor_cols_n - 1) ){
  var1 <- factor_cols[var1_pos]
  for( var2_pos in (var1_pos + 1):factor_cols_n  ){
    var2 <- factor_cols[var2_pos]
    res <- facts[, c(var1, var2), with=FALSE]
    colnames(res) <- c('cat1', 'cat2')
    res$var1 <- var1
    res$var2 <- var2
    
    melted_data <- rbind(melted_data, res)
  }
}

melted_data[, var1:=factor(var1, levels=factor_cols)]
melted_data[, var2:=factor(var2, levels=factor_cols %>% rev)]
melted_data %>% 
  .[var1=='surgical_approach']

ggplot(data=melted_data) +
  geom_mosaic(aes(x=product(cat1, cat2)), na.rm = TRUE, color='red', size=0.3) +
  facet_grid(var1~var2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank())
  

