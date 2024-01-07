library(data.table)
library(magrittr)
library(haven)

dades <- read_sav('../data/BASEDATOSSRS.FINAL_19_10_23.sav') %>% 
  as.data.table


