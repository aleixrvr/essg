cont_vars_ <- list(
  `number_of_posterior_instrumented_levels`="number_of_posterior\ninstrumented_levels",
  co3="co3",
  `total_surgical_time_st1_st2_st3`="total_surgical_time\nst1_st2_st3",
  `total_blood_loss_ml_st1_st2_st3`="total_blood_loss\nst1_st2_st3",
  `1y._odi_score_`="1y._odi_score_",
  `1y._srs22_srs_subtotal_score`="1y._srs22_srs\nsubtotal_score",
  `1y._sf36_mcs`="1y._sf36_mcs",
  `1y._sf36_pcs`="1y._sf36_pcs"
)

data_ %>% 
  .[, .SD, .SDcols=cont_vars_ %>% names] %>% 
  .[, co3 := as.factor(co3)]->
  cont_data

for(var_name_ in cont_vars_ %>% names){
  setnames(cont_data, var_name_, cont_vars_[[var_name_]])
}

cont_data %>%
  ggpairs() +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1),
    strip.text.y=element_text(angle=30, hjust=0.5, vjust=0.5, size=10)
  ) 
