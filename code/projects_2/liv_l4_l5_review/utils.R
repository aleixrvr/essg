calc_table <- function(data_, var_, treatment_n, data_treats, treatment_vals, treatment_) {
  res <- stats_fun(data_[[var_]])
  
  res_treats <- list()
  for(treatment_pos in 1:treatment_n){
    res_treats[[treatment_pos]] <- stats_fun(
      data_treats[[treatment_pos]][[var_]])
  }
  
  if(is.numeric(data_[[var_]])){
    result <- data.frame(Name = var_)
    result$all <- "{res$mean} ({res$sd})" %>% f
    for(treatment_pos in 1:treatment_n){
      result[[treatment_vals[treatment_pos]]] <- "{res_treats[[treatment_pos]]$mean} ({res_treats[[treatment_pos]]$sd})" %>% f
    }
    
    for(n1 in 1:(treatment_n-1)){
      for(n2 in (n1+1):treatment_n){
        p.val <- calc_p_vals(data_, var_, treatment_, treatment_vals[c(n1, n2)])
        if(!is.null(p.val)){
          result[['{treatment_vals[n1]} vs {treatment_vals[n2]} p_val' %>% f]] <- p.val %>% round(4)
        }
      }
    }
  }else{
    result <- data.frame()
    for(cat_res in names(res$table)){
      name <- "{var_}-{cat_res}" %>% f
      result_cat <- data.frame(Name = name)
      result_cat$all <- "{res$table[cat_res]} ({res$proportion[cat_res] %>% round(2)})" %>% f
      for(treatment_pos in 1:treatment_n){
        val_ <- "{res_treats[[treatment_pos]]$table[cat_res]} ({(res_treats[[treatment_pos]]$proportion[cat_res]*100) %>% round(2)}%)" %>% f
        result_cat[[treatment_vals[treatment_pos]]] <- val_
      }
      for(n1 in 1:(treatment_n-1)){
        for(n2 in (n1+1):treatment_n){
          n1_ <- res_treats[[n1]]$table[cat_res]
          n1_ <- ifelse(is.na(n1_), 0, n1_)
          n2_ <- res_treats[[n2]]$table[cat_res]
          n2_ <- ifelse(is.na(n2_), 0, n2_)
          N1 <- data_treats[[n1]][, .N]
          N2 <- data_treats[[n2]][, .N]
          p.val <- prop.test(c(n1_, n2_), c(N1, N2))$p.val %>% round(4)
          result_cat[['{treatment_vals[n1]} vs {treatment_vals[n2]} p_val' %>% f]] <- p.val
        }
      }
      
      if(is.null(result)){
        result <- result_cat
      }else{
        result <- rbind(result, result_cat)
      }
    }
  }
  return(result)
}