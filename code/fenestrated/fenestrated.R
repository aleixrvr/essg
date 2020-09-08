library(data.table)
library(magrittr)
library(ggplot2)
library(zeallot)

source('code/fenestrated/fenestrated-utils.R')
source('code/train.R')

tuneLength <- 3
outcome <- 'fenestrated'

data_sel <- get_data()






log_model <- glm(fenestrated~., data=sel_data_reg, family='binomial') 
sel_data_reg$predictions <- predict(log_model, sel_data_reg, type='response')


sel_data_reg[, quantile(predictions) %>% round(digits = 2), fenestrated]

sel_data_reg[fenestrated==0, predictions] %>% hist

sel_data_reg %>% 
  .[, fenestrated := as.factor(fenestrated) ] %>% 
  ggplot(aes(predictions, fill=fenestrated)) +
  geom_density(alpha = 0.1) +
  facet_grid(fenestrated~., scales = 'free')

setorder(sel_data_reg, predictions)
View(sel_data_reg)


dt <- sel_data_reg
outcome <- 'done_alif'
covariates <- colnames(dt) %!in% outcome
