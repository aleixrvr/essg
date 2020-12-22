library(readxl)
library(data.table)
library(magrittr)
library(yaml)
library(stringr)
library(zeallot)

source('code/basic.R')
source('code/utils.R')
source('code/train.R')

XLS_PATH <- 'data/ESSG extraction July 2020_3.xlsx'

# outcome <- 'had_complication'
# k_fold <- 10
# tuneLength <- 2
# models <- c('elastic')
# first_visit=TRUE
# increment=FALSE
# aa <- create_model(
#   outcome, first_visit, increment, k_fold, tuneLength, models
# )

create_model <- function(
  outcome, first_visit=TRUE, increment=FALSE, k_fold, tuneLength, models
){

  c(data_set, predictive_vars, outcome) %<-% get_data(outcome, first_visit, increment)
  c(train_data, validation_data) %<-% create_validation_set(data_set)
  
  best_model <- train_model(train_data, outcome, k_fold, tuneLength, models)
  
  if( best_model$model_type == 'regression' ){
    best_model$distribution <- data_set[, get(outcome)] %>% quantile
  }else{
    best_model$distribution <- data_set[, get(outcome)] %>% table %>% prop.table()
  }
  validation_results <- eval_validation(validation_data, best_model)
  
  return(list(
    best_model=best_model, predictive_vars=predictive_vars, 
    validation_results=validation_results
  ))
}

get_accuracy <- function(best_model){
  if( best_model$model_type == 'regression' ){
    text <- 'RMSE'
    acc <- best_model$rmse
  }else{
    text <- 'Accuracy'
    acc <- best_model$accuracy
  }
  return(list(acc=acc, text=text))
}

show_stats <- function(best_model, predictive_vars, validation_results, show_vars=TRUE){

  "{outcome}" %>% f %>% print
  "{get_accuracy(best_model)$text}: {get_accuracy(best_model)$acc}" %>% f %>% print
  "{outcome} distribution:" %>% f %>% print
  best_model$distribution %>% print
  "Model Type: {best_model$sel_model}" %>% f %>% print
  "Params: {as.yaml(best_model$params)}" %>% f %>% print
  
  if(show_vars){
    cat('\nPredictive Variables')
    cat(predictive_vars$predictive %>% as.yaml)
  }
  
  return(invisible())
}

get_data <- function(outcome='', first_visit=TRUE, increment=FALSE, clean=TRUE, only_two_years=TRUE){
  
  clinical_data <- read_excel(XLS_PATH) %>% 
    data.table
  
  reinterventions <- read_excel(XLS_PATH, sheet = "Rev surgeries") %>%
    data.table %>%
    .[, .(reinterventions=.N*1.0), `Code of the patient`]
  
  complication_patients <- read_excel(XLS_PATH, sheet='Complications') %>%
    as.data.table %>%
    .[`Complication Type`=='Primary'] %>%
    .[`Reoperation Due to Complication`=='Yes'] %>% 
    .[, unique(`Code of the patient`)]
  
  if( only_two_years ){
    valid_patients <- clinical_data[
      `st1. Date of Stage 1` %>% as.Date() < as.Date('2018-07-31'), 
      `Code of the patient` %>% unique
    ]
    clinical_data %<>% .[`Code of the patient` %in% valid_patients]
  }
  
  clinical_data %<>% 
    .[, had_complication := 'No'] %>% 
    .[`Code of the patient` %in% complication_patients, had_complication := 'Yes'] %>%
    merge(reinterventions, by="Code of the patient", all.x=TRUE, all.y=FALSE)
  
  # predictive_vars <- read_yaml('code/five_years/predictive_vars_{outcome}.yml')
  predictive_vars <- read_yaml('code/five_years/predictive_vars.yaml')  
  
  if( increment ){
    outcome_base <- get_base_outcome(outcome, first_visit)
    clinical_data[, increment := get(outcome) - get(outcome_base)]
    clinical_data[, c(outcome) := NULL]
    outcome <- 'increment'
  }
  
  if( clean ){
    clinical_data %>% 
      clean_data(predictive_vars$predictive) %>% 
      .[, .SD, .SDcols=c(outcome, predictive_vars$predictive)] %>% 
      remove_constant_cols %>% 
      na.omit ->
      data_set
  }else{
    data_set <- clinical_data
  }
  
  return(list(
    data_set,
    predictive_vars,
    outcome
  ))
}

create_validation_set <- function(data_set){
  inds <- sample(1:nrow(data_set), floor(nrow(data_set)*.8), replace=FALSE)
  train_data <- data_set[inds] %>% remove_constant_cols()
  cols_ <- train_data %>% colnames
  validation_data <- data_set[-inds, .SD, .SDcols=cols_] 
  
  return(list(train_data, validation_data))
}

eval_validation <- function(validation_data, best_model){
  
}

plot_scores <- function(data_set, outcome, demographics){
  demo_plots <- list()
  for(demo_var in demographics){
    if( class(data_set[, get(demo_var)]) == 'numeric' ){
      data_set %>% 
        copy() %>% 
        setnames(demo_var, 'demo_var') %>% 
        setnames(outcome, 'outcome') %>% 
        setnames(outcomes_2y[[outcome]], 'outcome_2y') %>% 
        .[, .(demo_var, outcome, outcome_2y)] %>% 
        melt(id.vars='demo_var') %>% 
        ggplot(aes(demo_var, value, color=variable)) +
        geom_point(alpha=0.1) + 
        xlab(demo_var) +
        ylab('score') + 
        scale_color_discrete(labels = c(outcome, outcomes_2y[[outcome]])) +
        geom_smooth(method='lm') ->
        demo_plots[[demo_var]]
    }else{
      data_set %>% 
        copy() %>% 
        setnames(demo_var, 'demo_var') %>% 
        setnames(outcome, 'outcome') %>% 
        setnames(outcomes_2y[[outcome]], 'outcome_2y') %>% 
        .[, .(demo_var, outcome, outcome_2y)] %>% 
        melt(id.vars='demo_var') %>% 
        ggplot(aes(demo_var, value, fill=variable)) +
        geom_boxplot() + 
        xlab(demo_var) +
        ylab('score') + 
        scale_fill_discrete(labels = c(outcome, outcomes_2y[[outcome]])) ->
        demo_plots[[demo_var]]
    }
  }
  
  return(demo_plots)
}

plot_complications <- function(
  complication_type, complications, data_set, demographics
){
  patients_complications_5y <- complications %>%
    .[`Days since surgery` >= 365*2 ] %>% 
    .[`Days since surgery` < 365*5 ] %>% 
    .[`Category of the complication` == complication_type, `Code of the patient`]
  patients_complications_2y <- complications %>%
    .[`Days since surgery` < 365*2 ] %>% 
    .[`Category of the complication` == complication_type, `Code of the patient`]
  
  data_ <- data_set %>%
    copy %>%
    .[, complication_before_2y:='No'] %>%
    .[`Code of the patient` %in% patients_complications_2y, complication_before_2y:='Yes'] %>% 
    .[ followup_2y == 'no_followup', complication_before_2y:=NA] %>%
    .[, complication_2y_5y:='No'] %>%
    .[`Code of the patient` %in% patients_complications_5y, complication_2y_5y:='Yes'] %>% 
    .[ followup_5y == 'no_followup', complication_2y_5y:=NA]
  
  demo_plots <- list()
  for(demo_var in demographics){
    if( class(data_[, get(demo_var)]) == 'numeric' ){
      data_ %>% 
        copy() %>% 
        setnames(demo_var, 'demo_var') %>% 
        .[, .(demo_var, complication_before_2y, complication_2y_5y)] %>% 
        melt(id.vars='demo_var') %>% 
        ggplot(aes(variable, demo_var, fill=value)) +
        geom_boxplot() + 
        coord_flip() +
        ylab(demo_var) +
        xlab(complication_type) + 
        scale_color_discrete(labels = c(outcome, outcomes_2y[[outcome]])) ->
        demo_plots[[demo_var]]
    }else{
      data_ %>% 
        copy() %>% 
        setnames(demo_var, 'demo_var') %>% 
        .[, .(complications_ratio=sum(complication_2y_5y=='Yes')/sum(complication_before_2y=='Yes')), 
          demo_var
        ] %>% 
        ggplot(aes(demo_var, complications_ratio, fill=demo_var)) +
        geom_col() + 
        xlab(demo_var) +
        ylab('Ratio Complications Yes: 2y->5y / Before 2y') +
        theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1)) ->
        demo_plots[[demo_var]]
    }
  }
  
  return(demo_plots)
}

rm_blanks <- . %>% gsub(' ', '_', .)
get_model_info <- . %>% summary %>% coefficients %>% round(3)

variable_effect <- function(data_set, outcome, outcome_2y, demographics){
  
  outcome_ <- outcome %>% rm_blanks
  outcome_2y_ <- outcome_2y %>% rm_blanks
  demographics_ <- sapply(demographics, rm_blanks, USE.NAMES = TRUE)
  
  data_ <- data_set %>% copy
  for( colname in colnames(data_) ){
    setnames(data_, colname, colname %>% rm_blanks)
  }
  
  sel_vars <- c(
    demographics_$demographic, 
    outcome_, 
    outcome_2y_
  )
  
  data_ %>% 
    .[, 
      .SD, 
      .SDcols= sel_vars
    ] %>%
    melt(id.vars=c(demographics_$demographic)) %>% 
    na.omit %>% 
    .[, `5y_vs_2y_effect` := ifelse(variable==outcome_, 1, 0)] %>% 
    .[, variable := NULL ] ->
    model_data
  
  vars_ <- paste(demographics_$demographic, collapse=' + ')
  for(demo_var in demographics_$demographic){
    vars_ <- "{vars_} + {demo_var}*`5y_vs_2y_effect`" %>% f
  }
  model_formula <- "value ~ {vars_}" %>% f %>% as.formula()
  if( class( model_data[, value]) == 'numeric' ){
    lm(model_formula, data=model_data) %>% 
      get_model_info ->
      model_1
  }else{
    model_data %>% 
      copy %>% 
      .[, value := ifelse(value=='Yes', 1, 0)] %>% 
      lm(model_formula, data=.) %>% 
      get_model_info ->
      model_1
  }
  
  sel_vars <- c(
    demographics_$demographic, 
    demographics_$radiologic, 
    outcome_, 
    outcome_2y_
  )
  
  data_ %>% 
    .[, 
      .SD, 
      .SDcols= sel_vars
    ] %>%
    melt(id.vars=c(demographics_$demographic, demographics_$radiologic)) %>% 
    na.omit %>% 
    .[, `5y_vs_2y_effect` := ifelse(variable==outcome_, 1, 0)] %>% 
    .[, variable := NULL ] -> 
    model_data
  
  c(demographics_$demographic, demographics_$radiologic) %>% 
    paste(collapse=' + ') ->
    vars_
  for(demo_var in  demographics_$radiologic){
    vars_ <- "{vars_} + {demo_var}*`5y_vs_2y_effect`" %>% f
  }
  model_formula <- "value ~ {vars_}" %>% f %>% as.formula()
  
  if( class( model_data[, value]) == 'numeric' ){
    lm(model_formula, data=model_data) %>% 
      get_model_info ->
      model_2
  }else{
    model_data %>% 
      copy %>% 
      .[, value := ifelse(value=='Yes', 1, 0)] %>% 
      lm(model_formula, data=.) %>% 
      get_model_info ->
      model_2
  }
  return(list(demographic=model_1, radiology=model_2))
  
}
