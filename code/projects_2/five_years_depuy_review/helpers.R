library(ggplot2)

clean_data <- function(dt){
  
  for(name in colnames(dt)){
    new_name <- gsub(",", ".", name, fixed=TRUE)
    setnames(dt, name, new_name)
  }
  
  try({
    dt[, `Coronal Balance (C7PL to CSVL)` := as.numeric(`Coronal Balance (C7PL to CSVL)`)]
    dt[, `6W. Coronal Balance (C7PL to CSVL)` := as.numeric(`6W. Coronal Balance (C7PL to CSVL)`)]
    dt[, `2Y. Coronal Balance (C7PL to CSVL)` := as.numeric(`2Y. Coronal Balance (C7PL to CSVL)`)]
    dt[, `5Y. Coronal Balance (C7PL to CSVL)` := as.numeric(`5Y. Coronal Balance (C7PL to CSVL)`)]
    
    dt[, `Sagittal Balance` := as.numeric(`Sagittal Balance`)]
    dt[, `6W. Sagittal Balance` := as.numeric(`6W. Sagittal Balance`)]
    dt[, `2Y. Sagittal Balance` := as.numeric(`2Y. Sagittal Balance`)]
    dt[, `5Y. Sagittal Balance` := as.numeric(`5Y. Sagittal Balance`)]
  }, silent=TRUE)
  
  if( 'Major curve Cobb angle' %in% colnames(dt)){
    setnames(dt, 
             "Major curve Cobb angle", "Static Major curve Cobb angle")
  }
  
  dt[, `3CO` := (PSOs == 'Yes') | (PVCR == 'Yes')]
  
  setnames(
    dt, 
    'SRS22 -SI_First Visit', 
    'SRS22 - Self image / Appearance_First Visit'
  )
  
  setnames(
    dt, 
    'SRS22 - Satisfaction_First Visit', 
    'SRS22 - Satisfaction with management_First Visit'
  )
  
  setnames(
    dt, 
    'SRS22 - MH_First Visit', 
    'SRS22 - Mental health_First Visit'
  )
  setnames(
    dt, 
    'SRS22 - Function_First Visit', 
    'SRS22 - Function / Activity_First Visit'
  )
  
  dt[, 
     `Pelvic Fixation` := grepl(
       'Iliac', 
       `Posterior Instrumented Fusion: Upper / Lower Levels`, 
       fixed=TRUE
     )] %>% 
    .[, `Pelvic Fixation` := ifelse(`Pelvic Fixation` == TRUE, 'Yes', 'No')] 
  
  dt[, `ASA classification` := as.factor(`ASA classification`)]
  
  uiv_ <- c('T10', 'T11', 'T12', 'L1')
  dt %>% 
    .[, upper:= substr(`Posterior Instrumented Fusion: Upper / Lower Levels`, 1, 2)] %>% 
    .[, uiv_t10_12_l1 := ifelse(upper %in% uiv_, 'Yes', 'No')]
  
  dt
}

stats_fun <- function(dt, variable){
  if( dt[, class(get(variable))] == 'numeric' ){
    depuy = dt[type=='depuy', get(variable)] %>% na.omit()
    nondepuy = dt[type=='non-depuy', get(variable)] %>% na.omit()

    return( list(
      stats = dt[, .(
        mean=mean(get(variable), na.rm=TRUE),
        sd = sd(get(variable), na.rm=TRUE),
        N = length(get(variable) %>% na.omit)), 
        type],
      p_val = t.test(depuy, nondepuy)$p.val
    ))
  }else{
    res_table_depuy <- table(dt[type=='depuy', get(variable)])
    res_table_nondepuy <- table(dt[type=='non-depuy', get(variable)])
    
    res <- list(
      table_depuy = res_table_depuy, 
      proportion_depuy = prop.table(res_table_depuy),
      table_nondepuy = res_table_nondepuy, 
      proportion_nondepuy = prop.table(res_table_nondepuy)
    )
    
    if( length(res_table_depuy) < 20){
      for(p_val_cat in dt[, unique(get(variable))]){
        p_val_name <- "p_val_{p_val_cat}" %>% f
        
        n_depuy <- dt[type=="depuy", .N]
        n_depuy_var <- dt[type=="depuy", sum(get(variable) == p_val_cat, na.rm = TRUE)]
        n_nondepuy <- dt[type=="non-depuy", .N]
        n_nondepuy_var <- dt[type=="non-depuy", sum(get(variable) == p_val_cat, na.rm = TRUE)]
        res[[p_val_name]] <- prop.test(
          c(n_depuy_var, n_nondepuy_var),
          c(n_depuy, n_nondepuy)
        )$p.val
      }
      
    }
    
    return(res)
  }
}

plot_time <- function(dt, vars_years, middle_point = 6/52){
  res <- dt[, 
            lapply(.SD, function(x) mean(x, na.rm=TRUE)), 
            .SDcols = vars_years, 
            by=c('type')] %>% 
    melt(id.vars = "type") %>% 
    dcast(variable~type) 
  setorder(res, "variable")
  
  p_vals <- c()
  for(var_ in vars_years){
    depuy <- dt[type=='depuy', get(var_)] %>% na.omit()
    nondepuy <- dt[type=='non-depuy', get(var_)] %>% na.omit()
    p_vals <- c(p_vals, t.test(depuy, nondepuy)$p.val)
  }
  
  res[, p_vals := p_vals]
  res[, year:= c(0, middle_point, 2, 5)]
  res[, variable := NULL]
  res %>% melt(id.vars = 'year') ->
    res
  res_type <- res[variable != 'p_vals']
  setnames(res_type, "variable", "type")
  res_p_val <- res[variable=='p_vals']
  setnames(res_p_val, 'value', 'p_val')
  res_p_val[, variable:=NULL]
  res_p_val <- merge(res[variable == 'depuy'], res_p_val, by='year')
  
  ggplot(res_type) +
    geom_line(aes(year, value, color=type)) +
    geom_point(aes(year, value, color=type)) + 
    ggtitle(vars_years[1]) +
    xlim(c(-0.5, 5.5)) +
    expand_limits(y = 0) +
    geom_text(data = res_p_val, 
              aes(year, value, label=round(p_val, 2)),
              vjust = 0, nudge_y = 2) ->
    res_plot
  return(res_plot)
}





