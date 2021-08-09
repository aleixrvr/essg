library(readxl)
library(data.table)
library(magrittr)

prev_surgeries <- read_excel('data/Previous surgeries.xlsx') %>% 
  as.data.table

prev_surgeries[, `Code of the patient` %>% uniqueN]

prev_surgeries[`Fusion (yes/no)`=='Yes' & is.na(`Fusion nb`)]
prev_surgeries[`Decompression (yes/no)`=='Yes' & is.na(`Decompression nb`)]

prev_surgeries[`Decompression (yes/no)` == 'No' & `Fusion (yes/no)` == 'No', 
  `Code of the patient` %>% uniqueN]

prev_surgeries[, .(
    all(`Decompression (yes/no)` == 'No') & all(`Fusion (yes/no)` == 'No'),
  .N
  ), `Code of the patient`] %>% 
  setorder(-N) %>% 
  .[V1==TRUE] %>% 
  .[, `Code of the patient`] ->
  non_patients



# Group A
# TODO: tots 
prev_surgeries[, any(`Decompression (yes/no)` == 'Yes') & all(`Fusion (yes/no)` == 'No'), 
  `Code of the patient`] %>% 
  .[V1==TRUE]

prev_surgeries[, any(`Decompression (yes/no)` == 'Yes') & all(`Fusion (yes/no)` == 'No'), 
  `Code of the patient`] %>% 
  .[V1==TRUE] %>% 
  .[, `Code of the patient`] -> 
  a_pats


# Group B 
# TODO: agaafar max
prev_surgeries[, max(`Fusion nb`, na.rm = TRUE), `Code of the patient`] %>% 
  .[0 < V1 & V1 < 4]

prev_surgeries[, max(`Fusion nb`, na.rm = TRUE), `Code of the patient`] %>% 
  .[0 < V1 & V1 < 4] %>% 
  .[, `Code of the patient`] ->
  b_pats

# GRoup C 
group_c_ <- prev_surgeries[, max(`Fusion nb`, na.rm = TRUE), `Code of the patient`] %>% 
  .[V1 >= 4]

group_c_[, `Code of the patient` %>% unique] ->
  c_pats


group_c <- prev_surgeries[`Code of the patient` %in% group_c_[, `Code of the patient`]]
group_c_1 <- group_c[
  grepl('Iliac', `Fusion upper-lower levels`) | 
    `Pedicle subtraction osteotomy yes/no` == 'Yes', ]

group_c_2 <- group_c[!(`Code of the patient` %in% group_c_1[, `Code of the patient`])]
group_c[, `Code of the patient` %>% uniqueN]
group_c_1[, `Code of the patient` %>% uniqueN]
group_c_2[, `Code of the patient` %>% uniqueN]



# Counting
prev_surgeries[, all(`Decompression (yes/no)` == 'No') & all(`Fusion (yes/no)` == 'No'), 
  `Code of the patient`] %>% 
  .[V1==TRUE] ->
  nothing

#prev_surgeries[is.na(`Fusion (yes/no)`)]
prev_surgeries[, any(`Fusion (yes/no)` == 'Yes') & all(is.na(`Fusion nb`)), 
  `Code of the patient`] %>% 
  .[V1==TRUE] ->
  is_na


len(a_pats) +
len(b_pats) +
len(c_pats)

all <- c(a_pats, b_pats, c_pats, 
  nothing[, `Code of the patient`], is_na[, `Code of the patient`]
)
len(all)
prev_surgeries[, `Code of the patient` %>% uniqueN]
prev_surgeries[!(`Code of the patient` %in% all), `Code of the patient`]


intersect(a_pats, b_pats)
intersect(c_pats, b_pats)
intersect(c_pats, a_pats)

prev_surgeries[`Code of the patient`=='ANKISSY0052-DK', ]



prev_surgeries[, grepl('Iliac', `Fusion upper-lower levels`, fixed=TRUE) %>% sum]
