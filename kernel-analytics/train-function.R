
# main function
training_function                <-  function(data, metrik, grid_perc, output_names, constant_features, constant_pre, variant_features, time_periods) {
  # # This is the training function that given the data computes all the models automatically,
  #   if data is correctly loaded. Only need to specify the data, the metric used, the output names and time periods.
  #   The names of the variables need to be exactly as in the 'names.R' file. The technique used to predict is regression.
  #   Arguments:
  #     :data: <data.frame> Data is the dataset that contains all the observations
  #     :metrik: <str> Metric used to select the optimal tuning parameters. i.e. 'MAE', 'RMSE' 
  #     :grid_perc: <float> Percentage of the rows selected on the grids for hypterparameter tuning
  #     :output_names: <list of str> Names sourced from the 'names.R' file
  #     :constant_features: <list of str> Features that are non-changing over time
  #     :constant_pre: <list of str> Features that are non-changing but this time without Surgeon and Site --  Could make this automatic for future datasets
  #     :variant_features: <list of str> Variables that change across time ('Baseline', '1year', '2years')
  #     :time_periods: <list of str> Time periods sourced from 'names.R' as well.
  #   Outputs:
  #     :results: <list of objects> Results obtained from the models
  #     :resamples: <list of caret objects> Resamples done for all the different time 
  #     :best_models: <list of caret obkects> Best models already selected
  #     :XY_Plots, Box_Plots, Parallel_Plots, Splom_Plots: <various plots> Plots generated can be called
  
  
  # Create empty lists to store results
  results_lm     <- list(); results_lmwp   <- list(); results_gbm    <- list(); results_rf     <- list()
  results_xgbL   <- list(); results_xgbT   <- list(); results_lmAIC_f<- list(); results_glmnet <- list()
  results_elnet  <- list()
  lms_saved      <- list(); lmwp_saved     <- list(); gbm_saved      <- list(); rf_saved       <- list()
  xgbL_saved     <- list(); xgbT_saved     <- list(); glmnet_saved   <- list()#; lmAIC_f_saved  <- list()
  elnet_saved    <- list()
  resamples      <- list(); models         <- list();  best_model     <- list(); xy_plots       <- list()
  bw_plots       <- list(); parallel_plots <- list();  dot_plots      <- list(); splom_plots    <- list()
  i              <- 0     ;  variants_name  <- NULL 
  
  # For each output name & each time period select different training datasets to train all the models
  for (y in output_names){
    for (time in time_periods){
      i <- i +1
      count_models  <- 0
      start.clock   <-  Sys.time()
      # Depending on the iteration of the time loop, a different set of variables is selected from the dataset
      if(time == '_0_1.2'){
        predictors_name <- paste(y, 'Baseline', sep = '_')
        y_name          <- paste(y, '1year', sep = '_')
        if (is.null(variant_features) == FALSE){
          variants_name   <- NULL
          for (var in variant_features){
            variants_name  <- c(variants_name, paste(var, 'Baseline', sep = '_'))
          }
        }
        formula_vars   <- c(predictors_name, y_name, constant_pre, variants_name)
        df             <- data[, formula_vars]
        formula_vari   <- c(predictors_name, constant_pre, variants_name)
      }
      if(time == '_0_1'){
        predictors_name   <- paste(y, 'Baseline', sep = '_')
        y_name            <- paste(y, '1year', sep = '_')
        if (is.null(variant_features) == FALSE){
          variants_name     <- NULL
          for (var in variant_features){
            variants_name   <- c(variants_name, paste(var, 'Baseline', sep = '_'))
          }
        }
        formula_vars      <- c(predictors_name, y_name, constant_features, variants_name)
        df                <- data[, formula_vars]
        formula_vari      <- c(predictors_name, constant_features, variants_name)
      }
      if(time == '_0_2.2'){
        predictors_name <- c(paste(y, 'Baseline', sep = '_'))
        y_name          <- paste(y, '2years', sep = '_')
        if (is.null(variant_features) == FALSE){
          variants_name  <- NULL
          for (var in variant_features){
            
            variants_name  <- c(variants_name, paste(var, 'Baseline', sep = '_'))
          }
        }
        formula_vars   <- c(predictors_name, y_name, constant_pre, variants_name)
        df             <- data[, formula_vars]
        formula_vari   <- c(predictors_name, constant_pre, variants_name)
      }
      if(time == '_0_2'){
        predictors_name   <- c(paste(y, 'Baseline', sep = '_'))
        y_name            <- paste(y, '2years', sep = '_')
        if (is.null(variant_features) == FALSE){
          variants_name     <- NULL
          for (var in variant_features){
            variants_name   <- c(variants_name, paste(var, 'Baseline', sep = '_'))
          }
        }
        formula_vars      <- c(predictors_name, y_name,constant_features, variants_name)
        df                <- data[, formula_vars]
        formula_vari      <- c(predictors_name, constant_features, variants_name)
      }
      if(time == '_1_2'){
        predictors_name   <- c(paste(y, 'Baseline', sep = '_'), paste(y, '1year', sep = '_'))
        y_name            <- paste(y, '2years', sep = '_')
        if (is.null(variant_features) == FALSE){
          variants_name     <- NULL
          for (var in variant_features){
            if (grepl(pattern = 'SRS_22r',x = var)){
              variants_name <- c(variants_name, paste(var, 'Baseline', sep = '_'), paste(var, '1year_', sep = '_'))
            }
            else {
              variants_name   <- c(variants_name, c(paste(var, 'Baseline', sep = '_'), paste(var, '1year', sep='_')))
            }
          }
        }
        formula_vars      <- c(predictors_name, y_name,constant_features, variants_name)
        df                <- data[, formula_vars]
        formula_vari      <- c(predictors_name, constant_features, variants_name)
      }
      
      # Linear Model
      formula_vars2      <-  paste(formula_vari, collapse = ' + ')
      formula            <-  paste(y_name, ' ~ ', formula_vars2, sep = '') #, constant_features, variant_features
      linear_model       <-  lm(formula =  as.formula(formula), data = df)
      results_lm[[i]]    <-  linear_model
      
      message(paste('All G        -> ', formula))
      
      # Creating splits 
      set.seed(1)
      inTrain            <-  createDataPartition(y = df[, y_name], p = .8, list = FALSE)
      df                 <-  data
      training           <-  df[inTrain, ]
      test               <-  df[-inTrain, ]
      fitControl         <-  trainControl(method = "cv", number = 5, search = 'random')
      set.seed(1)
      # Linear Model with data partition (train/test)
      model_lmwp         <-  caret::train(as.formula(formula), data = training, method = 'lm', trControl = fitControl)
      lmwp_pred          <-  predict(model_lmwp, test)
      lmwp_prediction    <-  postResample(pred = lmwp_pred, obs = test[, y_name])
      results_lmwp[[i]]  <-  lmwp_prediction
      lmwp_saved[[i]]    <-  model_lmwp
      
      message(paste('LMWP         -> Done'))
      set.seed(1) 
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
      set.seed(1)
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
      
      set.seed(1)        
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
      
      set.seed(1)                
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
      
      set.seed(1)               
      # GLMNet
      model_glmnet        <-  caret::train(as.formula(formula), data = training, method = 'glmnet', trControl = fitControl)
      glmnet_pred         <-  predict(model_glmnet, test)
      glmnet_prediction   <-  postResample(pred = glmnet_pred, obs = test[,y_name])
      results_glmnet[[i]] <-  glmnet_prediction
      glmnet_saved[[i]]   <-  model_glmnet
      
      message(paste('glmnet       -> Done \n ', number_of_models, 'GLMnets trained'))
      set.seed(1) 
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
      
      print(resample_models$timings)
      
      
      # Storing plots to include in the Results object. Can be deleted if size is too big.
      xy_plots[[i]]        <- xyplot(resample_models, metric="MAE") 
      bw_plots[[i]]        <- bwplot(resample_models, metric="MAE")
      parallel_plots[[i]]  <- parallelplot(resample_models,metric="MAE")
      splom_plots[[i]]     <- splom(resample_models,metric="MAE")
      
      message('Resamples done and plots stored')
      
      # Saving them all together
      pdf(paste(y_name, time, metrik, '.pdf', sep =''), width = 15, height = 20)
      print(paste(y_name, time, metrik,sep = ''))
      print(plot(model_rf, main = 'Random Forest performance '))
      print(resampleHist(model_xgbT))
      print(xyplot(resample_models, metric = 'MAE', main =  'XY Plot'))
      print(splom(resample_models, metric = 'MAE'))
      print(bwplot(resample_models, metric = 'MAE', main = 'Box Plot'))
      print(parallelplot(resample_models, metric = "MAE", main = ' Parallel Plot'))
      dev.off()
      end.clock                         <-  Sys.time()
      clock.time.taken                  <-  end.clock - start.clock
      message('Round ', i, ' of ', length(output_names) * length(time_periods),
              '\nTime taken: ',clock.time.taken)
    }
    
  }
  
  results  <- list('lm' = results_lm, 'lmwp' = results_lmwp, 'gbm' = results_gbm, 'rf' = results_rf, 'xgbL' = results_xgbL, 'xgbT' = results_xgbT, 'glmnet' = results_glmnet, 'ElNet' = results_elnet) #'lmAICf' = results_lmAIC_f, 
  results_output <- list('results'=results, 'resamples'= resamples, 'best_models' = best_model, 'XY_plots' = xy_plots, 'Box_Plots' = bw_plots, 'Parallel_Plots' = parallel_plots, 'Splom_Plots' = splom_plots)
  
  return(results_output)
}
