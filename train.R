library(magrittr)
library(caret)
library(glue)

source('train-utils.R')

covariates <- c()
outcome <- c()

covariates %>% paste(, collapse = ' + ') %>% paste(outcome, ' ~ ', , sep = '') %>% 
  as.formula -> formula
  
formula            <-   #, constant_features, variant_features
linear_model       <-  lm(formula =  as.formula(formula), data = df)
results_lm[[i]]    <-  linear_model

message(paste('All G        -> ', formula))

# Creating splits 

inTrain            <-  createDataPartition(y = df[, y_name], p = .8, list = FALSE)
df                 <-  data
training           <-  df[inTrain, ]
test               <-  df[-inTrain, ]
fitControl         <-  trainControl(method = "cv", number = 5, search = 'random')

# Linear Model with data partition (train/test)
model_lmwp         <-  caret::train(as.formula(formula), data = training, method = 'lm', trControl = fitControl)
lmwp_pred          <-  predict(model_lmwp, test)
lmwp_prediction    <-  postResample(pred = lmwp_pred, obs = test[, y_name])
results_lmwp[[i]]  <-  lmwp_prediction
lmwp_saved[[i]]    <-  model_lmwp

message(paste('LMWP         -> Done'))
 
# Gradient Boosting Machine
grid_gbm           <-  expand.grid(n.trees = c(1, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 250, 500), shrinkage = c(0.01, 0.05, 0.1, 0.001),n.minobsinnode = c(3, 5, 10,15), interaction.depth = c(1, 5, 10)) 
grid_gbm           <-  grid_gbm[sample(1:dim(grid_gbm)[1], round(dim(grid_gbm)[1] * grid_perc), replace=FALSE), ]
number_of_models   <-  round(dim(grid_gbm)[1])
message('Number of models  -> ', number_of_models)
model_gbm          <-  caret::train(as.formula(formula), data = training, method='gbm',trControl = fitControl, tuneGrid = grid_gbm, verbose  = FALSE)
gbm_pred           <-  predict(model_gbm, test)
gbm_prediction     <-  postResample(pred = gbm_pred, obs   = test[, y_name]) 
results_gbm[[i]]   <-  gbm_prediction
gbm_saved[[i]]     <-  model_gbm

message(paste('GBM          -> Done \n ', number_of_models, 'GBMs trained'))

# Random Forest   
grid_rf            <-  expand.grid(mtry = c(round(sqrt(ncol(data))), 15, 30, 45))                                                                 #,shrinkage=c(0.01,0.02), n.minobsinnode= c(5,6), interaction.depth=5n.trees=c(10,20),,20,50,100,500,1000), shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
number_of_models   <-  dim(grid_rf)[1]
message('Number of models  -> ', number_of_models)
model_rf           <-  caret::train(as.formula(formula), data = training, method = 'rf', trControl = fitControl, tuneGrid = grid_rf, importance = TRUE)
rf_pred            <-  predict(model_rf, test)
rf_prediction      <-  postResample(pred = rf_pred, obs = test[, y_name]) 
results_rf[[i]]    <-  rf_prediction
rf_saved           <-  model_rf

message(paste('RF           -> Done \n ', number_of_models, 'Random Forests trained'))

        
# eXtreme Gradient Boosting Linear
grid_xgbL           <-  expand.grid( nrounds= c(seq(1,50, 5), 100, 200), lambda = c(1, 5, 10, 50, 100), alpha =c(1, 5, 10, 50, 100), eta =c(0.1, 0.05, 0.01))
grid_xgbL           <-  grid_xgbL[sample(1:dim(grid_xgbL)[1], round(dim(grid_xgbL)[1] * grid_perc), replace=FALSE), ]
number_of_models    <-  round(dim(grid_xgbL)[1])
message('Number of models  -> ', number_of_models)
xgbL_trcontrol      <-  trainControl(method="cv",  number = 5,  verboseIter = T, returnData = FALSE, returnResamp = "all",  allowParallel = TRUE)
model_xgbL          <-  train(as.formula(formula), data = training,  method = "xgbLinear", metric = 'RMSE', trControl = xgbL_trcontrol, tuneGrid = grid_xgbL)
xgbL_pred           <-  predict(model_xgbL, test)
xgbL_prediction     <-  postResample(pred = xgbL_pred, obs   = test[, y_name])
results_xgbL[[i]]   <-  xgbL_prediction
xgbL_saved[[i]]     <-  model_xgbL

message(paste('xgbL         -> Done \n ', number_of_models, 'Linear XGBoosts trained'))

                
# eXtreme Graduient Boosting
#grid_xgbT           <-  expand.grid( nrounds = c(10,100,200,500,1000,2000), max_depth = c(1,5,10,round(sqrt(dim(data)[2]))),  eta = c(0.1,0.5,0.01), gamma = c(.1,1,5,10), colsample_bytree = c(.5,.8), min_child_weight = c(.5, 1, 5),  subsample = c(8,.9,1))
grid_xgbT           <-  expand.grid(nrounds = c(10,20,30,40,50,100, 500, 1000), max_depth = c(1, 5, round(sqrt(dim(data)[2]))),  eta = c(0.1, 0.025, 0.01,0.001), gamma = c(1, 5), colsample_bytree = c(.9, 1), min_child_weight = c(0.1, 10),  subsample = c(.9, 1))
grid_xgbT           <-  grid_xgbT[sample(1:dim(grid_xgbT)[1], round(dim(grid_xgbT)[1] * grid_perc), replace = FALSE), ]
number_of_models    <-  round(dim(grid_xgbT)[1])
message('Number of models -> ', number_of_models)
xgbT_trcontrol      <-  trainControl(method="cv",  number = 5,  verboseIter = FALSE, returnData=FALSE, returnResamp = "all",  allowParallel = TRUE )
model_xgbT          <-  train(as.formula(formula), data = training,  method="xgbTree", metric = 'RMSE', trControl = xgbT_trcontrol, tuneGrid = grid_xgbT)
xgbT_pred           <-  predict(model_xgbT, test)
xgbT_prediction     <-  postResample(pred = xgbT_pred, obs = test[,y_name])
results_xgbT[[i]]   <-  xgbT_prediction
xgbT_saved[[i]]     <-  model_xgbT

message(paste('xgbT         -> Done \n ', number_of_models, 'XGBoosts trained'))

               
# GLMNet
model_glmnet        <-  caret::train(as.formula(formula), data = training, method = 'glmnet', trControl = fitControl)
glmnet_pred         <-  predict(model_glmnet, test)
glmnet_prediction   <-  postResample(pred = glmnet_pred, obs = test[,y_name])
results_glmnet[[i]] <-  glmnet_prediction
glmnet_saved[[i]]   <-  model_glmnet

message(paste('glmnet       -> Done \n ', number_of_models, 'GLMnets trained'))
 
# Elastic Net
grid_elnet          <-  expand.grid(lambda = c(seq(0, 100, 1.5)), alpha = c(seq(0, 0.1, length = 20)))
number_of_models    <-  dim(grid_elnet)[1]*grid_perc
grid_elnet          <-  grid_elnet[sample(1:dim(grid_elnet)[1], round(dim(grid_elnet)[1] * grid_perc), replace = FALSE), ]
model_elnet         <-  caret::train(as.formula(formula), data=training, method='glmnet', trControl = fitControl, standardize = FALSE, maxit = 1000000)
elnet_pred          <-  predict(model_elnet, test)
elnet_prediction    <-  postResample(pred = elnet_pred, obs = test[, y_name])
results_elnet[[i]]  <-  elnet_prediction
elnet_saved[[i]]    <-  model_elnet

message(paste('Elastic Net -> Done \n ', number_of_models, 'Elastic Nets trained'))

# Collect resamples and make plots
resample_models       <-     resamples(list(LMWP = model_lmwp, GBM = model_gbm, RF = model_rf, XGBL = model_xgbL, XGBT = model_xgbT,  glmnet = model_glmnet, ELNET = model_elnet)) # LMAICf = model_lmAIC_f,
resamples[[i]]        <-     summary(resample_models)
models[[i]]           <-     list(LMWP = model_lmwp, GBM = model_gbm, RF = model_rf, XGBL = model_xgbL, XGBT = model_xgbT, glmnet = model_glmnet, ELNET = model_elnet) #  LMAICf = model_lmAIC_f,
summary.resample      <-     summary(resample_models)

# Selecting best models from resample using metric specified
if (metrik == 'MAE'){
  commands <- paste('models[[', i, ']]', '$' , names(which.min(summary.resample$statistics$MAE[, 4])), sep = '')
  best_model[[i]]   <- eval(parse(text = commands))
}
if (metrik == 'RMSE'){
  commands <- paste('models[[', i, ']]', '$' , names(which.min(summary.resample$statistics$RMSE[, 4])), sep = '')
  best_model[[i]]   <- eval(parse(text = commands))
}
if (metrik == 'RSquared'){
  commands <- paste('models[[', i, ']]', '$' , names(which.max(summary.resample$statistics$RSquared[, 4])), sep = '')
  best_model[[i]]   <- eval(parse(text = commands))
}