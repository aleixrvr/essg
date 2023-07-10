library(magrittr)
library(caret)
library(glue)
library(logger)

source('code/basic.R')
source('code/train-utils.R')


# outcome
# dt
# k_fold
# tuneLength


results <- list()
pred_formula <- '{outcome} ~ .' %>% f %>% as.formula

# y_values <- dt[[outcome]]
# type_model <- 'classification'
# if( class(y_values) == 'numeric' ){
#   type_model <- 'regression'
# }
  
trControl <-  trainControl(method = "cv", number = k_fold)

# Linear Model with data partition (train/test)
if( type_model == 'classification' ){
  method <- 'glm'
}else{
  method <- 'lm'
}
model <-  train(pred_formula, data = dt, method = method, trControl = trControl)
results[['lm']] <- model
log_info(paste('LM         -> Done'))
 
# Gradient Boosting Machine
# tuneGrid <- expand.grid(
#   nrounds = c(10,20,30,40,50,100, 500, 1000), 
#   max_depth = c(1, 5, round(sqrt(dim(dt)[2]))), 
#   eta = c(0.1, 0.025, 0.01,0.001), 
#   gamma = c(1, 5), 
#   colsample_bytree = c(.9, 1),
#   min_child_weight = c(0.1, 10),  
#   subsample = c(.9, 1)
# )
# model <-  train(
#   pred_formula, data = dt, method='xgbTree', trControl = trControl, tuneGrid = tuneGrid, verbose = FALSE
# )

results[['boosting']] <-  train(
  pred_formula, data = dt, method='xgbTree', trControl = trControl, tuneLength = tuneLength, verbose = FALSE
)

log_info(f('Boosting          -> Done \n {dim(grid)[1]} GBMs trained'))

 
# Elastic Net
# tuneGrid <- expand.grid(lambda = c(seq(0, 100, 1.5)), alpha = c(seq(0, 0.1, length = 20)))
# model_elnet <- caret::train(
#   pred_formula, data=dt, method='glmnet', trControl = trControl, standardize = FALSE, tuneGrid = tuneGrid
# )

results[['elastic_net']] <- caret::train(
  pred_formula, data=dt, method='glmnet', trControl = trControl, standardize = FALSE, tuneLength = tuneLength
)
log_info(paste('Elastic Net -> Done \n ', number_of_models, 'Elastic Nets trained'))

