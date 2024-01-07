library(questionr)

suggest_cols <- function(colname, dt){
  cols <- colnames(dt)
  cols[ grepl(colname %>% tolower, cols %>% tolower)]
}

get_type <- function(col_data){
  if( na.omit(col_data) %>% uniqueN == 2){
    return('classification')
  }else{
    return('regression')
  }
}

create_model <- function(dt, outcome, step_model=FALSE){
  
  formula <- glue("{outcome} ~ .")
  type <- get_type(dt[, ..outcome])
  if( type == "regression"){
    model <- lm(formula, data=dt %>% na.omit)
  }else{
    model <- glm(formula, data=dt %>% na.omit, 
                 family = 'binomial')
  }
  
  if(step_model){
    model <- step(model, direction = "backward", trace=0)
  }
  
  stats_model <- summary(model)$coefficients[, c(1, 4)] %>% 
    round(4) %>% 
    as.data.frame %>% 
    setDT(keep.rownames = "variable")
  stats_model_ci <- suppressWarnings({suppressMessages(confint(model))}) %>% 
    round(4) %>% 
    as.data.frame %>% 
    setDT(keep.rownames = "variable")
  stats_model <- merge(stats_model, stats_model_ci, by = 'variable') %>% 
    .[, effect := Estimate / mean(dt[, get(outcome) %>% na.omit])] 
  if( type == "regression"){
    setnames(stats_model, "Pr(>|t|)", "p_value")
  }else{
    browser()
    setnames(stats_model, "Pr(>|z|)", "p_value")
    
    odds_r <- odds.ratio(model)[, 1:3] 
    vars_ <- row.names(odds_r)
    odds_r <- odds_r %>% data.table
    setnames(odds_r, '2.5 %', 'OR - 2.5 %')
    setnames(odds_r, '97.5 %', 'OR - 97.5 %')
    odds_r[, varriable := vars_]
    stats_model <- merge(stats_model, odds_r, by = 'variable')
  }
  
  return(stats_model)
}

format_table <- function(result, vars){
  result[, effect := Estimate / mean(dt[, get(outcome) %>% na.omit])] %>%
    .[variable %chin% vars, ] %>%
    mutate(
      p_value = cell_spec(
        p_value, "html", bold = ifelse(p_value < 0.05, TRUE, FALSE)
      )
    ) %>%
    kable(format = "html", escape = F) %>% 
    kable_styling()
}

