library(ggplot2)
library(zeallot)
library(boot)
library(magrittr)

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
  ate_data, outcome, treatment_name, predictive_variates, first_visit=FALSE, 
  tuneLenghtATE, modelsATE, incremental=TRUE, is_classification
){
  if( incremental == TRUE ){
    base_outcome <- get_base_outcome(outcome, first_visit)
  }else{
    base_outcome <- outcome
  }
  
  sel_vars <- c(outcome, base_outcome, predictive_variates) %>% unique
  ate_data %>%
    .[, .SD, .SDcols=sel_vars] %>%
    na.omit %>% 
    remove_constant_cols ->
    dt
  
  if( incremental == TRUE ){
    diff_outcome <- dt[, ..outcome] - dt[, ..base_outcome]
    dt[, diff_outcome:= diff_outcome]
    dt[, c(outcome):=NULL]
    dt[, c(base_outcome):=NULL]
  }else{
    setnames(dt, outcome, 'diff_outcome') 
  }
  
  dt %>%
    .[get(treatment_name) == 'Yes'] %>% 
    .[, c(treatment_name):= NULL] %>%
    train_model('diff_outcome', tuneLength = tuneLenghtATE, models=modelsATE) ->
    best_model_yes
  
  dt %>%
    .[get(treatment_name) == 'No'] %>% 
    .[, c(treatment_name):= NULL] %>% 
    train_model('diff_outcome', tuneLength = tuneLenghtATE, models=modelsATE) ->
    best_model_no
  
  dt_y <- copy(dt)
  dt_y[[treatment_name]] ='Yes'
  if( is_classification == TRUE ){
    pred_y <- predict(best_model_yes$model, dt_y, type='prob')$Yes
  }else{
    pred_y <- predict(best_model_yes$model, dt_y)
  }
  
  dt_n <- copy(dt)
  dt_n[[treatment_name]] ='No'
  if( is_classification == TRUE ){
    pred_n <- predict(best_model_no$model, dt_n, type='prob')$Yes
  }else{
    pred_n <- predict(best_model_no$model, dt_n)
  }
  
  ate_res <- NULL
  ate <- mean(pred_y - pred_n)
  if( 'Propensity' %in% colnames(dt)){
    ate_dt <- dt %>% 
      copy %>% 
      setnames('diff_outcome', 'outcome')
    ate_dt$pred_y <- pred_y 
    ate_dt$pred_n <- pred_n
  }
  
  if( is_classification == TRUE ){
    distribution <- c(Proportion=mean(dt[['diff_outcome']]=='Yes'))
  }else{
    distribution <- quantile(dt[['diff_outcome']], na.rm=TRUE)
  }
  
  return(list(
    distribution=distribution,
    best_model_yes = best_model_yes,
    best_model_no = best_model_no,
    ate=ate,
    ate_dt=ate_dt
  ))
}

print_ates <- function(treatment_name, outcome, results, is_classification){
  "Outcome: {outcome}" %>% f %>% print
  "Distribution:" %>% f %>% print
  results$distribution %>% print
  "Model Type: {results$best_model$sel_model} \n" %>% f %>% print
  if( is_classification == TRUE ){
    "Accuracy: {results$best_model$accuracy} \n" %>% f %>% print
  }else{
    "RMSE: {results$best_model$rmse} \n" %>% f %>% print
  }
  "Params: {as.yaml(results$best_model$params)}" %>% f %>% print
  "ATE (Yes-No): {results$ate} \n" %>% f %>% print
  
  dt_ <- results$ate_dt %>% 
    copy %>% 
    .[, outcome_binary:=ifelse(outcome=='Yes', 1, 0)] %>% 
    setnames(treatment_name, 'treatment')
  if( !is.null(dt_) ){
    
    if( is_classification == TRUE ){
      results$ate_dt %>% .[,
        .(outcome=mean(outcome=='Yes')), 
        .(treatment=get(treatment_name))] %>% 
        setnames('outcome', outcome) %>% 
        setnames('treatment', treatment_name) ->
        table_treatement
      
      dt_ %>% 
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
      results$ate_dt %>% .[,
        .(outcome=mean(outcome)), 
        .(treatment=get(treatment_name))] %>% 
        setnames('outcome', outcome) %>% 
        setnames('treatment', treatment_name) ->
        table_treatement
      
      dt_ %>% 
        ggplot(aes(Propensity, outcome, color=treatment)) +
        geom_point() +
        geom_smooth(method='lm') ->
        data_plot
    }
    ind_yes <- which(table_treatement[, 1] == 'Yes')
    ind_no <- (1:2)[-ind_yes]
    diff_treatment <- table_treatement[ind_yes, 2] - table_treatement[ind_no, 2]
    diff_treatment %<>% as.data.frame() %>% .[1, 1] %>% round(3)
    
    "Observational differences in treatment {diff_treatment} (Yes-No) \n\n" %>% f %>%  print
    
    print(table_treatement)
    print(data_plot)
    
    dt_ %>% 
      setorder(-Propensity) %>% 
      .[, individual_ate:=pred_y - pred_n] %>% 
      .[, mean_ate_rolling:=cumsum(individual_ate)/(1:nrow(dt_))] %>% 
      ggplot(aes(Propensity, mean_ate_rolling, group=1)) +
      geom_line() +
      ggtitle("Mean accumulative by propensity of outcome {outcome}" %>% f) ->
      data_plot
    print(data_plot)
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
      axis.text=element_text(size=10))
  
  
  # std_err <- bootSamples$t %>% apply(2, sd)
  # 
  # coef_results <- coef(best_model$model$finalModel, best_model$params$lambda) 
  # 
  # coefs <- as.numeric(coef_results)
  # coefs_names <- rownames(coef_results)
  # 
  # coef_results <- data.table(variable=coefs_names, coef_value=coefs %>% round(3), std_err) %>% 
  #   setorder(-coef_value)
  # 
  # intercept_std <- coef_results[variable=='(Intercept)', std_err]
  # 
  # all_names %>% 
  #   as.data.table %>% 
  #   merge(coef_results, by='variable', all=TRUE) ->
  #   all_coefs
  # 
  # all_coefs %<>% 
  #   .[is.na(coef_value), std_err:=intercept_std] %>% 
  #   .[is.na(coef_value), coef_value:=0] %>% 
  #   setorder(-coef_value)
  # 
  # variables <- all_coefs[, variable]
  # all_coefs[, variable := factor(variable, levels=variables)]
  # all_coefs[, ci_up:=coef_value + 1.96*std_err]
  # all_coefs[, ci_low:=coef_value - 1.96*std_err]
  # 
  # 
  # all_coefs %>% 
  #   .[!is.na(original_name)] %>% 
  #   ggplot(aes(variable, coef_value)) +
  #   geom_col() +
  #   geom_errorbar(aes(ymin = ci_low, ymax = ci_up, width=0.3), linetype = "dashed") +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #   facet_grid(.~original_name, scales='free_x', space = "free_x") +
  #   theme( 
  #     panel.border = element_rect(color = "black", fill = NA, size = .2),
  #     axis.text=element_text(size=20))
}
