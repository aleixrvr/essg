library(caret)
library(data.table)

predictive_capability <- function(data_outcome, tps_prediction = c(2, 5), folds_n = 5){
  rmse_tp <- c()
  rmse_rel_tp <- c()
  for( tp in tps_prediction ){
    tp_data <- data_outcome[time_point==tp]
    
    cv_index <- createFolds(tp_data$cluster, folds_n, returnTrain = T)
    
    errors <- c()
    for( fold in 1:folds_n ){
      train_index <- cv_index[[fold]]
      train <- tp_data[train_index]
      test <- tp_data[-train_index]
      
      predictions <- train[, .(prediction = mean(value)), cluster]
      test %<>% merge(predictions, by='cluster', all.x = TRUE)
      errors %<>% c(errors, test[, value - prediction])
    }
    rmse <- sqrt(mean((errors)^2))
    rmse_rel <- rmse/tp_data[, sd(value)]
    
    rmse_tp %<>% c(rmse)
    rmse_rel_tp %<>% c(rmse_rel)
  }
  
  return(data.table(
    time_point = tps_prediction, 
    rmse = rmse_tp, 
    relative_rmse = rmse_rel_tp
  ))
}



analyze_cluster <- function(data_, tps_names, tps_prediction=c(2, 5)){
  data_ %>% 
    .[, .SD, .SDcols = c(tps_names, 'cluster', 'Code of the patient')] %>% 
    melt(id.vars=c("cluster", "Code of the patient"), value.name='value') %>% 
    na.omit %>% 
    setnames('Code of the patient', 'patient_id') %>% 
    .[, time_point := get_time_points(variable)] ->
    data_outcome
  
  data_outcome %>% 
    .[, .(
      mean_value = mean(value),
      sd_value = sd(value),
      se_value = sd(value)/sqrt(.N)
    ), .(cluster, time_point)] ->
    data_outcome_summary
  
  ggplot(data_outcome_summary, aes(
    time_point, mean_value, group=cluster, ymin = mean_value - 1.96*se_value, ymax = mean_value + 1.96*se_value)) +
    geom_line(aes(color=cluster)) +
    geom_ribbon(aes(fill=cluster), alpha = 0.1) +
    ggtitle("{var_}\nstandard error confidence intervals" %>% f) ->
    evol_plot
  
  model <- lmer(
    "value ~ time_point + cluster + (time_point - 1|patient_id)",
    data=data_outcome)  
  contrasts_results <- emmeans(model, specs = pairwise ~ cluster)
  
  contrasts_results_tps <- list()
  tps <- data_outcome[, time_point %>% unique]
  for( tp in tps ){
    data_tp <- data_outcome[time_point==tp]
    tp_model <- lm(value~cluster, data = data_tp)
    contrasts_results_tp <- emmeans(tp_model, specs = pairwise ~ cluster)
    contrasts_results_tps[[as.character(tp)]] <- contrasts_results_tp
  }
  
  predictive <- predictive_capability(data_outcome, tps_prediction)
  
  return(list(
    evol_plot = evol_plot,
    contrasts_results = contrasts_results,
    contrasts_results_tps = contrasts_results_tps,
    predictive = predictive
  ))
}