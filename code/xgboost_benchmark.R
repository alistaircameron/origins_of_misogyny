# Comparison point: XGBoost w/ all the features. Run a reduced grid search. 

n <- parallelly::availableCores()
weightedRMSE <- function(data, lev = NULL, model = NULL) {
  stats <- defaultSummary(data, lev = lev, model = model)
  wt_rmse <- function (pred, obs, wts, na.rm = TRUE) 
  sqrt(weighted.mean((pred - obs)^2, wts, na.rm = na.rm))
  res <- wt_rmse(pred = data$pred,
                 obs = data$obs, 
                 wts = data$wvs.weight)
  c(wRMSE = res, stats)
}

# For Rpart:
fitControl <- trainControl(method = 'repeatedcv',
                           number=10,
                           repeats = 3,
                           search = 'grid',
                           summaryFunction = weightedRMSE)

Grid <- expand.grid(cp = seq(0.01, 0.01, 0.01)) # Set this to 0.01 and we're good. ie. nice picture... make it larger, and you get less nodes, make it smaller, you get a deeper tree

r <- 7
set.seed(r) # This is reproducible, but will also give some variation in the parameters across which we search. 

# create hyperparameter grid
small_grid <- expand.grid(
  eta = c(.01, .05, .1),
  max_depth = c(2, 3, 6, 8),
  min_child_weight = c(2000, 5000),
  subsample = c(0.5, .65, .8),
  colsample_bytree = c(0.5, .65, .8),
  optimal_trees = 0,
  min_RMSE = 0,
  valid_RMSE = 0
)

# Random sample of rows. 
xgb_grid <- small_grid[sample(1:nrow(small_grid),5),]  

for(case in c("clustered", "benchmark")){
  if(case == "clustered"){
    load(here("intermediate_data/wvs.ready.RData"))
  }else{
    load(here('intermediate_data/benchmark_wvs.RData'))
  }
  
  df <- gender
  # Bit of a hack to just get the iteration i'm after.
  df$blip <- df[paste0("wvs.train",r)]
  df <- df[,-grep("wvs.train", colnames(df))]
  colnames(df)[ncol(df)] <- "wvs.train"
  
  # Bit of a hack to deal with gender having multiple entries
  df$blip <- df[paste0("wvs.gender",r)]
  df <- df[,-grep("wvs.gender", colnames(df))]
  colnames(df)[ncol(df)] <- "wvs.gender"
  
  joined <- select(filter(df, wvs.train == 1), "wvs.country_code") # Used later for SHAP values and for stratifying. 
  train <- filter(select(df, -c("wvs.country_code", "wvs.weight")), wvs.train == 1)
  test <- filter(select(df, -c("wvs.country_code","wvs.weight")), wvs.train == 0)
  
  wvs.weights <- df[df$wvs.train ==1,"wvs.weight"]
  train <- select(train, -"wvs.train")
  test <- select(test, -"wvs.train")
  
  set.seed(007) # J.B.
  true_hold_out_index <- sample(1:nrow(test),size = 0.15*nrow(df), replace = F)
  hold_out <- test[-true_hold_out_index,]
  test <- test[true_hold_out_index,]
  train_features <- select(train, -wvs.gender) %>% as.matrix()
  test_features <- select(test, -wvs.gender) %>% as.matrix()
  hold_out_features <- select(hold_out, -wvs.gender) %>% as.matrix()
  train_response <- train['wvs.gender']
  train_response <- as.numeric(unlist(train_response)) # don't ask why, just do it!
  test_response <- test['wvs.gender']
  hold_out_response <- hold_out['wvs.gender']
  
  all_countries <- unique(df$wvs.country_code) #used for the cross-validation
  
  for(i in 1:nrow(xgb_grid)) {
    # Stratified, hold-n-countries-out cross-validation
    custom_folds <- list()
    for(j in 1:5){
      countries <- sample(all_countries, size = floor(0.2*length(all_countries))) # 0.2 because i'm putting in the *test* obs. 
      custom_folds[[j]] <- which(joined$wvs.country_code %in% countries)
    }
    
    # create parameter list
    params <- list(
      eta = xgb_grid$eta[i],
      max_depth = xgb_grid$max_depth[i],
      min_child_weight = xgb_grid$min_child_weight[i],
      subsample = xgb_grid$subsample[i],
      colsample_bytree = xgb_grid$colsample_bytree[i]
    )
    
    # train model
    xgb.tune <- xgb.cv(
      params = params,
      data = train_features,
      label = train_response,
      folds = custom_folds,
      objective = "reg:squarederror",
      verbose = F,
      nthreads = n,
      weight = wvs.weights,
      nrounds = 5000,
      early_stopping_rounds = 20
    )
    
    # add min training error and trees to grid
    xgb_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
    xgb_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
    
    save(xgb_grid, file = here(paste0("intermediate_data/xgb_grid_", case, ".RData")))
    print(paste0("run done: ",i))
  }
  
  #### Evaluation on true hold out set.
  # Get the optimal parameters:
  sort_xgb_grid <- xgb_grid[order(xgb_grid$valid_RMSE) , ]
  params <- list(
    eta = sort_xgb_grid$eta[[1]],
    max_depth = sort_xgb_grid$max_depth[[1]],
    min_child_weight = sort_xgb_grid$min_child_weight[[1]],
    subsample = sort_xgb_grid$subsample[[1]],
    colsample_bytree = sort_xgb_grid$colsample_bytree[[1]],
    nrounds = sort_xgb_grid$optimal_trees[[1]]
  )
  
  opt.trees <- sort_xgb_grid$optimal_trees[[1]]
  set.seed(007) # This has stuffed me over in the past. 
  xgb.fit.final <- xgboost(
    params = params,
    data = train_features,
    label = train_response,
    nrounds = opt.trees,
    weight = wvs.weights,
    eval_metric = "rmse",
    objective = "reg:squarederror",
    nthreads = parallelly::availableCores(),
    verbose = F
  )
  
  # Predictions (doing on hold_out; could consider doing on train for the surrogate trees; BUT MUST BE SAME FOR GPBoost)
  tf <- hold_out_features
  tf <- cbind(tf,as.data.frame(predict(xgb.fit.final, newdata = hold_out_features)))
  colnames(tf)[ncol(tf)] <- "predictions"
  
  # Predictions on hold_out
  tr <- as.data.frame(hold_out_response[,1])
  tr$predictions <- predict(xgb.fit.final, newdata = hold_out_features)
  
  tr$residuals = tr[,1] - tr$predictions
  RMSE = sqrt(mean(tr$residuals^2))
  
  y_test_mean = mean(tr[,1])
  tss =  sum((tr[,1] - y_test_mean)^2 ) # Calculate total sum of squares
  rss =  sum(tr$residuals^2) # Calculate residual sum of squares
  rsq  =  1 - (rss/tss) # Calculate R-squared
  
  # Importance
  xgb.imp <- xgb.importance(model = xgb.fit.final)
  
  if(case == "clustered"){
    save(list = c("xgb.imp", "RMSE", "rsq", 'xgb.fit.final'), file = here(paste0("intermediate_data/cluster_xgb_initial.RData")))
  }else{
    save(list = c("xgb.imp", "RMSE", "rsq", 'xgb.fit.final'), file = here(paste0("intermediate_data/benchmark_xgb.RData")))
  }
} 

rm(list = ls())
gc()