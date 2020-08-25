library(magrittr)
library(caret)
library(glue)
library(logger)

source('code/basic.R')
source('code/train-utils.R')

# covariates <- c()
# outcome
# dt


results <- list()
pred_formula <- '{outcome} ~ .' %>% f %>% as.formula

y_values <- dt[[outcome]]
type_model <- 'classification'
if( class(y_values) == 'numeric' ){
  type_model <- 'regression'
}
  
# Creating splits 
inTrain  <-  createDataPartition(y = dt[[outcome]], p = .8, list = FALSE)
training <-  dt[inTrain, ]
test <-  dt[-inTrain, ]
fitControl <-  trainControl(method = "cv", number = 5, search = 'random')

# Linear Model with data partition (train/test)
model <-  caret::train(formula, data = training, method = 'lm', trControl = fitControl)
results[['lm']] <- list(
  model = model,
  accuracy = evaluate_predictions(model, test, outcome)
)
log_info(paste('LM         -> Done'))
 
# Gradient Boosting Machine
grid <- expand.grid(
  n.trees = c(1, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 250, 500), 
  shrinkage = c(0.01, 0.05, 0.1, 0.001),
  n.minobsinnode = c(3, 5, 10,15), 
  interaction.depth = c(1, 5, 10)
) 
model <-  caret::train(
  formula, data = training, method='xgbTree', trControl = fitControl, tuneGrid = grid, verbose = FALSE
)

results[['boosting']] <- list(
  model = model,
  accuracy = evaluate_predictions(model, test, outcome)
)

log_info(f('Boosting          -> Done \n {dim(grid)[1]} GBMs trained'))

 
# Elastic Net
grid <- expand.grid(lambda = c(seq(0, 100, 1.5)), alpha = c(seq(0, 0.1, length = 20)))
model_elnet <- caret::train(
  formula, data=training, method='glmnet', trControl = fitControl, standardize = FALSE, tuneGrid = grid
)
results[['elastic_net']] <- list(
  model = model,
  accuracy = evaluate_predictions(model, test, outcome)
)

log_info(paste('Elastic Net -> Done \n ', number_of_models, 'Elastic Nets trained'))

