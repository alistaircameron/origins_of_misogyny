# XGBoost-ing; Grid Search.

n <- parallelly::availableCores()
# create hyperparameter grid
full_xgb_grid <- expand.grid(
  eta = c(.01, .05, .1),                    # controls the learning rate
  max_depth = c(2, 3, 6, 8, 10, 12),                 # tree depth. Note that when we've got large n or p, it is OK to use deeper trees. https://bradleyboehmke.github.io/HOML/gbm.html
  min_child_weight = c(200, 2000, 5000),                    # minimum no. of obs in each child node (given large sample size, and max_depth, min_child_weight is basically irrelevant. eg. w/ 40,000 obs, need 9 splits to get to 150 observations in each node {ie. 40,000/(2^8) ~= 150})
  subsample = c(0.5, .65, .8),                       # percent training data to sample for each tree
  colsample_bytree = c(0.5, .65, .8),                 # percent columns to sample for each tree
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0,                    # a place to dump results
  valid_RMSE = 0
)

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
Grid <- expand.grid(cp = seq(0.01, 0.01, 0.01))

r <- 7 # JB
set.seed(r) # This is reproducible, but will also give some variation in the parameters across which we search. 
xgb_grid <- full_xgb_grid[sample(1:nrow(full_xgb_grid),50),] # random search across the parameters.

load(here('intermediate_data/wvs.ready.RData'))
df <- gender

# Bit of a hack to just get the iteration i'm after and to deal with gender having multiple entries
df$blip <- df[paste0("wvs.train",r)]
df <- df[,-grep("wvs.train", colnames(df))]
colnames(df)[ncol(df)] <- "wvs.train"
df$blip <- df[paste0("wvs.gender",r)]
df <- df[,-grep("wvs.gender", colnames(df))]
colnames(df)[ncol(df)] <- "wvs.gender"
colnames(df)[grep('wvs.country_code',colnames(df))] <- 'wvs.country.f'
joined <- select(filter(df, wvs.train == 1), "wvs.country.f") # Used later for SHAP values and for stratifying.
train <- filter(select(df, -c("wvs.country.f", "wvs.weight")), wvs.train == 1)
test <- filter(select(df, -c("wvs.country.f","wvs.weight")), wvs.train == 0)
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
train_response <- train["wvs.gender"]
train_response <- as.numeric(unlist(train_response))
test_response <- test["wvs.gender"]
hold_out_response <- hold_out["wvs.gender"]
all_countries <- unique(df$wvs.country.f) # used for cross-validation

# Stratified, hold-n-countries-out cross-validation; debate exists about whether this stratification should be different for each parameter grid.
custom_folds <- list()
for(j in 1:5){
  countries <- sample(all_countries, size = floor(0.2*length(all_countries))) # 0.2 because i'm putting in the *test* obs. 
  custom_folds[[j]] <- which(joined$wvs.country.f %in% countries)
}

for(i in 1:nrow(xgb_grid)) {
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
    nrounds = 10000,
    early_stopping_rounds = 50
  )
  
  # add min training error and trees to grid
  xgb_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  xgb_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
  
  save(xgb_grid, file = here("intermediate_data/xgb_grid_cluster.RData"))
  print(paste0("run done: ",i))
}

# Add validation RMSE
for(i in 1:nrow(xgb_grid)){
  params <- list(
    eta = xgb_grid$eta[[i]],
    max_depth = xgb_grid$max_depth[[i]],
    min_child_weight = xgb_grid$min_child_weight[[i]],
    subsample = xgb_grid$subsample[[i]],
    colsample_bytree = xgb_grid$colsample_bytree[[i]],
    nrounds = xgb_grid$optimal_trees[[i]]
  )
  opt.trees <- xgb_grid$optimal_trees[[i]]
  
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
    verbose = 0
  )
  
  tr <- as.data.frame(test_response[,1])
  tr$predictions <- predict(xgb.fit.final, newdata = test_features)
  tr$residuals = tr[,1] - tr$predictions
  xgb_grid$valid_RMSE[i] = sqrt(mean(tr$residuals^2))
  
  save(xgb_grid, file = here("intermediate_data/gender_valid_cluster.RData"))
  print(paste0("run done: ",i))
  
}


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

set.seed(007)
xgb.fit.final <- xgboost(
  params = params,
  data = train_features,
  label = train_response,
  nrounds = opt.trees,
  weight = wvs.weights,
  eval_metric = "rmse",
  objective = "reg:squarederror",
  nthreads = parallelly::availableCores(),
  verbose = 0
)

# Predictions on hold_out.
tf <- hold_out_features
tf <- cbind(tf,as.data.frame(predict(xgb.fit.final, newdata = hold_out_features)))
colnames(tf)[ncol(tf)] <- "predictions"
tr <- as.data.frame(hold_out_response[,1])
tr$predictions <- predict(xgb.fit.final, newdata = hold_out_features)

tr$residuals = tr[,1] - tr$predictions
RMSE = sqrt(mean(tr$residuals^2))

y_test_mean = mean(tr[,1])
tss =  sum((tr[,1] - y_test_mean)^2 ) # Calculate total sum of squares
rss =  sum(tr$residuals^2) # Calculate residual sum of squares
rsq  =  1 - (rss/tss) # Calculate R-squared

# This isn't pretty code, but it runs...
load(here('intermediate_data/wvs.ready.RData'))
df <- gender

# Bit of a hack to just get the iteration i'm after.
df$blip <- df[paste0("wvs.train",r)]
df <- df[,-grep("wvs.train", colnames(df))]
colnames(df)[ncol(df)] <- "wvs.train"

# Bit of a hack to deal with gender having multiple entries
df$blip <- df[paste0("wvs.gender",r)]
df <- df[,-grep("wvs.gender", colnames(df))]
colnames(df)[ncol(df)] <- "wvs.gender"

colnames(df)[grep('wvs.country_code',colnames(df))] <- 'wvs.country.f'

# Replace the outcome variable with the predicted values in the original training data and clean up
data <- filter(df, wvs.train == 0)
data <- data[-true_hold_out_index,]
data <- select(data, -c("wvs.country.f", "wvs.train"))
data <- select(data, -wvs.gender) # remove the original y-variable
data$predictions <- tf$predictions # Get the predictions from the RF

# Actually do it. 
colnames(data) <- make.names(colnames(data)) # was having issues with colnames... this can probably be removed. 
other.vars <- setdiff(colnames(data), c('predictions', "wvs.weight"))

pred.recipe <- recipes::recipe(data) %>%
  recipes::update_role(all_of(other.vars), new_role = "predictor") %>% 
  recipes::update_role(wvs.weight, new_role = "performance var") %>% 
  recipes::update_role(predictions, new_role = "outcome")

minsplit <- 5* nrow(data)/100 # Restrict leaf.nodes to be at least 5% of the sample. To prevent the algo. from splitting off just 1-country at a time.

set.seed(007)
rp <- train(pred.recipe, 
            data = data, 
            method = 'rpart', 
            trControl = fitControl, 
            metric = 'wRMSE', 
            maximize = F, 
            tuneGrid = Grid, 
            control = rpart.control(minsplit = minsplit, 
                                    minbucket = minsplit, 
                                    maxcompete = 4, 
                                    maxsurrogate = 5, 
                                    xval = 10, 
                                    maxdepth = 30)
)

# R-squared & wRMSE of rp model. 
rp.wrmse <- rp$results$wRMSE
rp.rsq <- rp$results$Rsquared

# Getting the countries in each node, and printing it in a pretty way
# Getting nodes and summaries for the trees:
ineq.tree <- rp$finalModel

ineq.tree$frame$lnode.original <- rownames(ineq.tree$frame) # get the true leaf-node
newdf <- filter(ineq.tree$frame, ineq.tree$frame$var == "<leaf>") # filter out the leaf-nodes
newdf$temp <- 1:nrow(newdf)
newdf <- newdf[order(newdf$yval),] # re-order 
newdf$lnode <- 1:nrow(newdf) # make the l.node from smallest to largest
newdf <- newdf[order(newdf$temp),] # re-order back again
newdf$temp <- NULL
newdf.yvals <- select(newdf, c("lnode", "yval")) # me coming back later; i now want the y-vals!
newdf <- select(newdf, c("lnode", "lnode.original")) # just keep these two columns
a <- rownames(ineq.tree$frame) # grab the rownames
ineq.tree$frame$order <- 1:nrow(ineq.tree$frame) # make the order the same as the original.

ineq.tree$frame <- merge(newdf, ineq.tree$frame, by = "lnode.original", all = T) # merge
ineq.tree$frame <- arrange(ineq.tree$frame, order)

rownames(ineq.tree$frame) <- a
ineq.leaf.nodes <- as.data.frame(rownames(ineq.tree$frame)[ineq.tree$where]) 

# Replace the *inequality* variable with the predicted values in the original data and clean up
data <- filter(df, wvs.train == 0)
data <- data[-true_hold_out_index,]

what.where <- cbind(data$wvs.country.f, ineq.leaf.nodes)
colnames(what.where)
ww <- as.data.frame(unique(what.where))
colnames(ww) <- c("country", "leaf.node")

www <- ww
colnames(www) <- c("country", "lnode.original")
www <- na.omit(merge(www, ineq.tree$frame[c("lnode", "lnode.original")], by = "lnode.original", all = T))
ww <- www
ww$lnode.original <- NULL
colnames(ww) <- c("country","leaf.nodes")

# Print the bloody thing.
node.function <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nNode", x$frame$lnode)
}

save(list = c("ineq.tree"), file = here("intermediate_data/ineq_tree.RData"), envir = .GlobalEnv)

# # For printing, change the variable names:
load(here("intermediate_data/ineq_tree.RData"))
ineq.tree$frame$var # Print 'em all

# Change 'em all
new.names <- ineq.tree$frame$var
new.names[1] <- "Temperate - No Dry Season"
new.names[2] <- "Kinship Intensity ii"
new.names[3] <- "Population in 1400"
new.names[7] <- "Temperate"
new.names[8] <- "Patrilineal"
new.names[9] <- "Absolute longitude"
new.names[13] <- "Kinship Intensity ii"
new.names[14] <- "Folklore - 'work' words from LIWC"
new.names[17] <- "Male"

ineq.tree$frame$var <- new.names

png(here(paste0("graphs/surrogate_tree.png")), 
    width = 1600,
    height = 1200, 
    res = 600  
)
rpart.plot(ineq.tree, node.fun = node.function, type = 5, fallen.leaves = F)
dev.off()

colnames(newdf.yvals) <- c("leaf.nodes", "yval")
ww <- merge(ww, newdf.yvals, by = "leaf.nodes")

save(list = c("sort_xgb_grid", "RMSE", "rsq", 'rp.rsq', 'rp.wrmse', 'ww', 'joined', 'xgb.fit.final'), file = here("intermediate_data/cluster_xgb.RData"), envir = .GlobalEnv)
rm(list = ls())