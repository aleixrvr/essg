cols <- followup_data %>% colnames

outcome <- vars_$covariates_increment[1] %>% clean_name()

detect_increment <- function(row_data){
  max_pos <- row_data %>% t %>% as.vector %>% is.na %>% `!` %>% which %>% max
  row_data[[1]] - row_data[[max_pos]]
}

outcome_time <- cols[grepl(outcome, cols)]
data_ <- followup_data[, .SD, .SDcols=outcome_time]
data_[, .SD %>% detect_increment, 1:nrow(data_)]
