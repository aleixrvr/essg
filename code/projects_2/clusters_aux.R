library(data.table)
library(magrittr)


calc_time_p_values <- function(data__, var_, var_6_b, var_2y, var_5y, cluster_name){
  val_ <- data__[[var_]]
  val_6_b <- data__[[var_6_b]]
  val_2y <- data__[[var_2y]]
  val_5y <- data__[[var_5y]]
  value_6_b <- substr(var_6_b, 1, 2)
  
  time_p_vals <- data.frame()
  time_p_vals %<>% 
    rbind(data.frame(
      t0='Preop', t1=value_6_b, p.val=t.test(val_6_b, val_)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0='Preop', t1='2Y', p.val=t.test(val_2y, val_)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0='Preop', t1='5Y', p.val=t.test(val_5y, val_)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0=value_6_b, t1='2Y', p.val=t.test(val_2y, val_6_b)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0=value_6_b, t1='5Y', p.val=t.test(val_5y, val_6_b)$p.val %>% round(4)
    )) %>% 
    rbind(data.frame(
      t0='2Y', t1='5Y', p.val=t.test(val_5y, val_2y)$p.val %>% round(4)
    ))  
  time_p_vals$group <- cluster_name
  
  return(time_p_vals)
}



