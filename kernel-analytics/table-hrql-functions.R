getting_outcome_name_from_numbers_to_table <- function(number, time_period_zero){
  if (number == 1 | number == 2){
    return(paste('ODI_score', time_period_zero,sep = ''))
  }
  else if(number == 3 | number == 4){
    return(paste('SRS22_function_score', time_period_zero,sep = ''))
  }
  else if(number == 5 | number == 6){
    return(paste('SRS22_MH_score', time_period_zero,sep = ''))
  }
  else if(number == 7 | number == 8){
    return(paste('SRS22_pain_score', time_period_zero,sep = ''))
  }
  else if(number == 9 | number == 10){
    return(paste('SRS22_SI_score', time_period_zero,sep = '')) 
  }
  else if(number == 11 | number == 12){
    return(paste('SRS22_subtotal_score', time_period_zero,sep = '')) 
  }
  else if(number == 13 | number == 14){
    return(paste('SF36v2_MCS_score', time_period_zero,sep = '')) 
  }
  else if(number == 15 | number == 16){
    return(paste('SF36v2_PCS_score', time_period_zero,sep = '')) 
  }
  else{
    return('Number out of bounds.')
  }
}