library(ggplot2)
library(zeallot)
library(boot)
library(magrittr)
library(gridExtra)
library(selectiveInference)

source('code/basic.R')
source('code/train.R')
source('code/utils.R')

explore_vars <- function(sel_data, treatment_name) {
  
  outcome <- sel_data[[treatment_name]]
  var_names <- colnames(sel_data) %!in% treatment_name
  
  for( sel_var_name in  var_names){
    sel_var <- sel_data[[sel_var_name]] 
    htmltools::h2(sel_var_name) %>% print
    nas <- floor(1000*sum(is.na(sel_var))/length(sel_var))/10
    if( nas == 0 ){
      nas <- floor(1000*sum(sel_var == 'NA')/length(sel_var))/10
    }
    
    'Proportion of na: {nas}%' %>% f %>% cat
    cat('\n')
    if( class(sel_var) == 'character' ){
      kable(table(outcome, sel_var), format = "html", booktabs = TRUE) %>% 
        kable_styling() %>% 
        print
    }else{
      data.frame(outcome, sel_var) %>% 
        ggplot(aes(sel_var, outcome)) +
        ggtitle(sel_var_name) +
        ylab(treatment_name) +
        geom_jitter(width = 0.05, height = 0.05) ->
        res_plot
      print(res_plot)
    }
    cat('\n\n')
  }
}

run_propensity <- function(sel_data, treatment_name, k_fold=5, tuneLength=5, models=c('lm', 'boosting', 'elastic')){
  sel_data %>% 
    na.omit -> clean_data
  
  patient_ids <- clean_data$patient_id
  clean_data[, patient_id:=NULL]
  
  clean_data %>% 
    train_model(treatment_name, k_fold, tuneLength, models) ->
    best_model 
  
  propensity <- predict(best_model$model, clean_data, type='prob')$Yes
  clean_data[, Propensity := propensity]
  clean_data[, patient_id:=patient_ids]
  
  best_model$plot <- clean_data %>% 
    ggplot(aes_string('propensity', fill=treatment_name, color=treatment_name)) +
    geom_density(alpha = 0.1) +
    xlim(c(0, 1))
  
  best_model$clean_data <- clean_data
  
  return(best_model)
}

calc_ate <- function(
  data_, outcome, treatment_name, predictive_variates, first_visit=FALSE, 
  tuneLenghtATE, modelsATE, incremental=TRUE, is_classification, repetitions=10
){
  if( incremental == TRUE ){
    base_outcome <- get_base_outcome(outcome, first_visit)
  }else{
    base_outcome <- outcome
  }
  
  sel_vars <- c(outcome, base_outcome, predictive_variates, "Code of the patient") %>% unique
  data_ %>%
    .[, .SD, .SDcols=sel_vars] %>%
    na.omit %>% 
    remove_constant_cols ->
    dt
  
  if( incremental == TRUE ){
    diff_outcome <- dt[, ..outcome] - dt[, ..base_outcome]
    dt[, diff_outcome:= diff_outcome]
    # dt[, c(outcome):=NULL]
    # dt[, c(base_outcome):=NULL]
  }else{
    dt[, diff_outcome:= get(outcome)]
    # setnames(dt, outcome, 'diff_outcome') 
  }
  
  ate_res <- bootstrap_ate(dt, 'diff_outcome', is_classification, repetitions, tuneLenghtATE, modelsATE)
  
  if( is_classification == TRUE ){
    distribution <- c(Proportion=mean(dt[['diff_outcome']]=='Yes'))
  }else{
    distribution <- quantile(dt[['diff_outcome']], na.rm=TRUE)
  }
  
  return(list(
    distribution=distribution,
    ate_res=ate_res,
    dt=dt
  ))
}

bootstrap_ate <- function(dt, outcome_name, is_classification, repetitions=10, 
  tuneLenghtATE, modelsATE
){
  pred_formula <- '`{outcome_name}` ~ .' %>% f %>% as.formula
  
  sel_cols <- c('Propensity', outcome_name)
  
  dt %>%
    .[get(treatment_name) == 'Yes'] %>% 
    .[, .SD, .SDcols=sel_cols] ->
    dt_yes
  
  dt %>%
    .[get(treatment_name) == 'No'] %>% 
    .[, .SD, .SDcols=sel_cols] ->
    dt_no
  
  dt_do_yes <- copy(dt)
  dt_do_yes[[treatment_name]] ='Yes'
  dt_do_no <- copy(dt)
  dt_do_no[[treatment_name]] ='No'
  
  dt_yes %>%
    train_model('diff_outcome', tuneLength = tuneLenghtATE, models=modelsATE) ->
    best_model_yes
  method_yes <- get_method(best_model_yes)
  
  dt_no %>%
    train_model('diff_outcome', tuneLength = tuneLenghtATE, models=modelsATE) ->
    best_model_no
  method_no <- get_method(best_model_no)
  
  dt_base <- copy(dt)
 
  trControl <-  caret::trainControl(method = "none", number = 1, p=1)
  
  predictions <- list()
  k_fold_rep <- 1
  for(r_ in 1:repetitions){
    dt_sample_yes <- stratified_resampling(dt_yes, outcome_name)
    dt_sample_no <- stratified_resampling(dt_no, outcome_name)
    
    # inds_yes <- sample(1:row_n_yes, row_n_yes, replace=TRUE)
    # dt_sample_yes <- dt_yes[inds_yes, ]
    # inds_no <- sample(1:row_n_no, row_n_no, replace=TRUE)
    # dt_sample_no <- dt_no[inds_no, ]
    
    model_yes <- train(pred_formula, dt_sample_yes, method=method_yes, 
      tuneGrid=best_model_yes$params, trControl = trControl)
    model_no <- train(pred_formula, dt_sample_no, method=method_no, 
      tuneGrid=best_model_no$params, trControl = trControl)

    dt_res <- copy(dt_base)
    if( is_classification == TRUE ){
      dt_res$pred_y <- predict(model_yes, dt_do_yes, type='prob')$Yes
      dt_res$pred_n <- predict(model_no, dt_do_no, type='prob')$Yes
    }else{
      dt_res$pred_y <- predict(model_yes, dt_do_yes)
      dt_res$pred_n <- predict(model_no, dt_do_no)
    }
    dt_res[, ite:=pred_y - pred_n]
    
    predictions[[r_]] <- dt_res
  }
  
  return(list(
    predictions=predictions,
    best_model_yes=best_model_yes,
    best_model_no=best_model_no)
  )
}

stratified_resampling <- function(dt_, outcome_name){
  row_n_ <- dt_ %>% nrow()
  if( class(dt_[, get(outcome_name)])  == 'numeric' ){
    inds_ <- sample(1:row_n_, row_n_, replace=TRUE)
    dt_sample_ <- dt_[inds_, ]
  }else{
    if( dt_[, get(outcome_name) %>% uniqueN] > 2 ){
      stop('More than 2 classes')
    }else{
      class_names <- dt_[, get(outcome_name) %>% unique %>% sort]
      if( !all(class_names == c('No', 'Yes')) ){
        stop('Binary with different names')
      }
      inds_y <-  dt_[, get(outcome_name) == 'Yes'] %>% which()
      inds_n <-  dt_[, get(outcome_name) == 'No'] %>% which()
      
      re_inds_y <- sample(inds_y, len(inds_y), replace=TRUE)
      re_inds_n <- sample(inds_n, len(inds_n), replace=TRUE)
      inds_ <- c(re_inds_n, re_inds_y)
      dt_sample_ <- dt_[inds_, ]
    }
  }
  
  return(dt_sample_)  
}

calc_ate_stats <- function(predictions, propensity_trim){
  mean_ate <- sapply(predictions, function(res_){
    res_[, ite %>% mean]
  })
  sd_mean_ate <- sd(mean_ate) %>% round(3)
  mean_ate <-mean(mean_ate) %>% round(3)
  mean_ate_trim <- sapply(predictions, function(res_){
    res_[Propensity < propensity_trim, ite %>% mean]
  })
  sd_mean_ate_trim <- sd(mean_ate_trim) %>% round(3)
  mean_ate_trim <- mean(mean_ate_trim) %>% round(3)
  mean_ate_compl <- sapply(predictions, function(res_){
    res_[Propensity > propensity_trim, ite %>% mean]
  })
  sd_mean_ate_compl <- sd(mean_ate_compl) %>% round(3)
  mean_ate_compl <- mean(mean_ate_compl) %>% round(3)
  
  return(list(
    mean_ate, sd_mean_ate, 
    mean_ate_trim, sd_mean_ate_trim, 
    mean_ate_compl, sd_mean_ate_compl 
  ))
}

print_ates <- function(
  treatment_name, outcome, results, is_classification, 
  propensity_trim=1, demographics=NULL
){
  "Outcome: {outcome}" %>% f %>% print
  "Distribution:" %>% f %>% print
  results$distribution %>% print
  
  "Model Type Y: {results$ate_res$best_model_yes$sel_model} \n" %>% f %>% print
  if( is_classification == TRUE ){
    "Accuracy: {results$ate_res$best_model_yes$accuracy} \n" %>% f %>% print
  }else{
    "RMSE: {results$ate_res$best_model_yes$rmse} \n" %>% f %>% print
  }
  "Params: {as.yaml(results$ate_res$best_model_yes$params)}" %>% f %>% print
  
  "Model Type No: {results$ate_res$best_model_no$sel_model} \n" %>% f %>% print
  if( is_classification == TRUE ){
    "Accuracy: {results$ate_res$best_model_no$accuracy} \n" %>% f %>% print
  }else{
    "RMSE: {results$ate_res$best_model_no$rmse} \n" %>% f %>% print
  }
  "Params: {as.yaml(results$ate_res$best_model_no$params)}" %>% f %>% print
  
  
  c(mean_ate, sd_mean_ate, mean_ate_trim, sd_mean_ate_trim,
    mean_ate_compl, sd_mean_ate_compl ) %<-% 
    calc_ate_stats(
      results$ate_res$predictions, propensity_trim
    )
  
  "ATE (Yes-No): {mean_ate} (Std.Error: {sd_mean_ate})\n" %>% f %>% print
  if( propensity_trim < 1){
    "Trimmed ATE (Yes-No): {mean_ate_trim} (Std.Error: {sd_mean_ate_trim})\n" %>% f %>% print
    "Upper ATE (Yes-No): {mean_ate_compl} (Std.Error: {sd_mean_ate_compl})\n" %>% f %>% print
  }
  
  dt_ <- results$dt %>% 
    copy %>% 
    setnames(treatment_name, 'treatment')

  if( !is.null(dt_) ){
    
    if( is_classification == TRUE ){
      dt_ %>% .[,
        .(outcome=mean(get(outcome)=='Yes')), 
        treatment] ->
        table_treatement
      
      dt_ %>% 
        .[, outcome_binary := ifelse(get(outcome) == 'Yes', 1, 0)] %>% 
        ggplot(aes(Propensity, outcome_binary, color=treatment)) +
        # geom_point() +
        geom_jitter(width =0, height=0.07) +
        geom_smooth() +
        ylim(c(-0.2,1.2)) +
        geom_abline(intercept = 0, slope = 0) +
        geom_abline(intercept = 1, slope = 0) +
        ylab(outcome) ->
        data_plot
    }else{
      dt_ %>% .[,
          .(outcome=mean(get(outcome))), 
          treatment] ->
        table_treatement
      
      dt_ %>% 
        ggplot(aes(Propensity, get(outcome), color=treatment)) +
        geom_point() +
        ylab(outcome) +
        geom_smooth() ->
        data_plot
    }
    ind_yes <- which(table_treatement[, 1] == 'Yes')
    ind_no <- (1:2)[-ind_yes]
    diff_treatment <- table_treatement[ind_yes, 2] - table_treatement[ind_no, 2]
    diff_treatment %<>% as.data.frame() %>% .[1, 1] %>% round(3)
    
    "Observational differences in treatment {diff_treatment} (Yes-No) \n\n" %>% f %>%  print
    
    print(table_treatement)
    print(data_plot)
    
    results$ate_res$predictions %>% 
      do.call(rbind, .) %>% 
      .[, .(ite=mean(ite), ite_sd=sd(ite)), .(`Code of the patient`, Propensity)] %>% 
      .[, ite_up:=ite + 1.96*ite_sd] %>% 
      .[, ite_low:=ite - 1.96*ite_sd] %>% 
      setorder(-Propensity) ->
      ites
    
    ites %>% 
      ggplot(aes(Propensity, ite, group=1)) +
      geom_line(color='orange') +
      geom_ribbon(
        aes(ymin=ite_low, ymax=ite_up), 
        linetype=2, alpha=0.1) +
      geom_abline(slope=0, intercept=0, color='black') +
      ggtitle("Individual Treatment effect by propensity\n{outcome}" %>% f) ->
      data_plot
    print(data_plot)
    
    ites %>% 
      merge(results$dt, by='Code of the patient') ->
      ites_demo
    
    if( length( demographics ) > 0){
      demo_plots <- list()
      for(demo in demographics){
        if( class(ites_demo[, get(demo)]) == 'numeric' ){
          ites_demo %>% 
            copy() %>% 
            setnames(demo, 'demo_var') %>% 
            ggplot(aes(demo_var, ite)) +
            geom_point() + 
            xlab(demo) +
            geom_smooth(method='lm') ->
            demo_plots[[demo]]
        }else{
          ites_demo %>% 
            copy() %>% 
            setnames(demo, 'demo_var') %>% 
            ggplot(aes(demo_var, ite)) +
            xlab(demo) +
            geom_boxplot() ->
            demo_plots[[demo]]
        }
      }
      
      
      do.call("grid.arrange", c(demo_plots, ncol=3))
    }
  }
  
  return(invisible())
}

plot_coefs <- function(treatment_name, covariates_data, best_model, replicas_boot){
  names_1 <- best_model$model$finalModel$xNames
  names_0 <- best_model$model$trainingData %>% colnames
  names_0 <- names_0[!(names_0 =='.outcome')]
  df <- best_model$model$trainingData
  
  
  df_names_0 <- data.table(variable=names_0, var_len=names_0 %>% nchar) %>% 
    setorder(var_len)
  
  df_names_1 <- data.frame(variable=names_1, original='', stringsAsFactors = FALSE)
  
  all_names <- data.frame()
  for( var_ in df_names_0$variable){
    
    var_expression <- var_
    is_large <- any(grepl(f('`{var_}`'), df_names_1$variable, fixed = TRUE) )
    if( is_large == TRUE ){
      is_large <- TRUE
      var_expression <- f('`{var_}`')
    }
    is_continuous <- FALSE
    if( any(var_expression == df_names_1$variable) ){
      is_continuous <- TRUE
    }
    
    if( is_continuous == TRUE ){
      all_names %<>% rbind(
        data.frame(variable=var_expression, original_name=var_)
      )  
    }else{
      all_values <- paste0(var_expression, table(df[[var_]]) %>% names)
      all_names %<>% rbind(
        data.frame(variable=all_values, original_name=var_)
      )  
    }
  }
  
  data_ <- covariates_data %>% 
    na.omit
  
  train_formula <- "{treatment_name} ~ ." %>% f %>% as.formula
  data_matrix <- model.matrix(train_formula, data_) %>% as.data.frame()
  data_matrix[[treatment_name]] <- data_[[treatment_name]]
  
  bootSamples <- boot(data_matrix, function(data, idx) {
    bootstrapData <- data[idx, ]
    
    train_formula <- "{treatment_name} ~ ." %>% f %>% as.formula
    bootstrapMod <- train(train_formula, 
      data = bootstrapData, 
      method = "glmnet", 
      trControl = trainControl(method = "none"),
      tuneGrid = best_model$model$bestTune)
    as.vector(coef(bootstrapMod$finalModel, best_model$model$bestTune$lambda))
  }, replicas_boot)
  
  
  coef_results <- coef(best_model$model$finalModel, best_model$params$lambda) 
  coefs_names <- rownames(coef_results)
  coef_results <- data.table(variable=coefs_names, bootSamples$t %>% t) %>% 
    melt(id.vars='variable', variable.name = "sample")
  
  all_names %>% 
    as.data.table %>% 
    merge(coef_results, by='variable', all=TRUE) %>% 
    .[, group:='difference'] %>% 
    .[is.na(value), group:='base'] %>% 
    .[is.na(value), value:=0] ->
    all_coefs
  
  # remove_original <- function(variable, original_name){
  #   variable <- gsub('`', '', variable, fixed=TRUE)
  #   if( !is.na(original_name) & variable != original_name ){
  #     variable <- gsub(original_name, '', variable, fixed=TRUE) 
  #   }
  #   variable
  # }
  
  # all_coefs[, variable:=remove_original(variable, original_name), 1:nrow(all_coefs)]
  
  all_coefs[, .(m=mean(value)), .(variable)] %>% 
    setorder(-m) %>% 
    .[, variable] ->
    variable_factors
  
  all_coefs %<>% 
    .[, variable := factor(variable, levels=variable_factors)] %>% 
    .[, original_name:=gsub(' ', '\n', original_name, fixed=TRUE), 1:nrow(all_coefs)]
  
  all_coefs %>% 
    .[!is.na(original_name)] %>% 
    ggplot(aes(variable, value, color=group), size=1) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(.~original_name, scales='free_x', space = "free_x") +
    theme( 
      panel.border = element_rect(color = "black", fill = NA, size = .4),
      strip.text.x = element_text(angle = 90),
      axis.text=element_text(size=10)) ->
    data_plot
  
  all_coefs[, .(
    low_ci=quantile(value, 0.025) %>% round(3), 
    median=median(value) %>% round(3), 
    up_ci=quantile(value, 0.975) %>% round(3),
    sd = sd(value) %>% round(3),
    t_value = round(abs(mean(value))/sd(value), 3)
  ), 
    variable] %>% 
    .[, selected := ifelse(low_ci*up_ci > 0, '*', '')] %>% 
    .[, .(variable, low_ci, median, up_ci, t_value, selected) ] ->
    table_ci
  
  return(list(
    data_plot, 
    table_ci
  ))
}


get_method <- function(trained_model){
  sel_model <- trained_model$sel_model
  
  if( 'lm' == sel_model ){
    method_ <- 'lm'
  }else if('glm' == sel_model){
    method_ <- 'glm'
  }else if('elastic_net' == sel_model){
    method_ <- 'glmnet'
  }else if('boosting' == sel_model){
    method_ <- 'xgbTree'
  }else{
    stop('Wrong model selection')
  }
  
  return(method_)
}
