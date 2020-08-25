library(data.table)
library(magrittr)
library(ggplot2)

source('code/alif/alif-utils.R')

sel_data <- get_data()

sel_data %>% 
  na.omit ->
  sel_data_reg

log_model <- glm(alif~., data=sel_data_reg, family='binomial') 
sel_data_reg$predictions <- predict(log_model, sel_data_reg, type='response')


sel_data_reg %>% 
  ggplot(aes(predictions, fill=alif, color=alif)) +
  geom_density(alpha = 0.1)

setorder(sel_data_reg, predictions)
View(sel_data_reg)


dt <- sel_data_reg
outcome <- 'done_alif'
covariates <- colnames(dt) %!in% outcome
