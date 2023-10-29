# Shapley Values
# Using the same seed, recreate the grid.
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
Grid <- expand.grid(cp = seq(0.01, 0.01, 0.01))
r <- 7
set.seed(r) # This is reproducible, but will also give some variation in the parameters across which we search. 
xgb_grid <- full_xgb_grid[sample(1:nrow(full_xgb_grid),50),] # random search across the parameters.

load(here('intermediate_data/wvs.ready.RData'))
df <- gender

# Bit of a hack to just get the iteration i'm after.
df$blip <- df[paste0("wvs.train",r)]
df <- df[,-grep("wvs.train", colnames(df))]
colnames(df)[ncol(df)] <- "wvs.train"
df$blip <- df[paste0("wvs.gender",r)]
df <- df[,-grep("wvs.gender", colnames(df))]
colnames(df)[ncol(df)] <- "wvs.gender"
colnames(df)[grep('wvs.country_code',colnames(df))] <- 'wvs.country.f'
joined <- select(filter(df, wvs.train == 1), "wvs.country.f")
train <- filter(select(df, -c("wvs.country.f", "wvs.weight")), wvs.train == 1) # Lose the weight mate
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
train_response <- as.numeric(unlist(train_response)) # don't ask why, just do it!
test_response <- test["wvs.gender"]
hold_out_response <- hold_out["wvs.gender"]
all_countries <- unique(df$wvs.country.f) #used for the cross-validation


# NEW STUFF.
load(here("intermediate_data/cluster_xgb.RData"))
shap_values <- shap.values(xgb.fit.final, 
                           as.matrix(hold_out_features))
sv <- shap_values$mean_shap_score
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(hold_out_features)) # as above.
save(list = c("shap_values","shap_long"), file = here("intermediate_data/mean_shap_vals.RData"))

# Extracting the top features as measured by shap_values
features <- unique(shap_long[,2])[1:10]
features <- data.frame(lapply(features, as.character), stringsAsFactors=FALSE)
features <- unlist(features)

# To print things nicely, need to change the feature names, and define which things to put in bold. So:
new.labels <- c("Temperate", "Male", "Folklore: challenge motifs", "Education", "Temperate - no dry season", "Marriage - bride price", "Muslim", "Age", "Town size", "Household size")
bold_me <- c(1,3,5,6)
bold_me <- 11 - bold_me  # flipped for graphing.

# Change variable names for pretty printing.
train_features_index <- c()
for(i in 1:length(features))(train_features_index[i] <- grep(features[i], colnames(hold_out_features)))
colnames(hold_out_features)[train_features_index] <- new.labels

shap_values$shap_score <- as.data.frame(shap_values$shap_score)
for(i in 1:length(features))(train_features_index[i] <- grep(features[i], colnames(shap_values$shap_score)))
colnames(shap_values$shap_score)[train_features_index] <- new.labels

features <- new.labels
print(shap.plot.summary.wrap2(as.data.frame(as.matrix(shap_values$shap_score)[,features]), # Ugly, but worky.
                              as.matrix(hold_out_features)[,features],
                              dilute = 1) + ylab("SHAP Values")) # Dilute = 10 or 100.

MakeExp <- function(x,y){
  exp <- vector(length = 0, mode = "expression")
  for (i in seq_along(x)) {
    if (i %in% y) exp[[i]] <- bquote(bold(.(x[i])))
    else exp[[i]] <- x[i]
  }
  return(exp)
}

shap.plot.summary1 <-  function (data_long, x_bound = NULL, dilute = FALSE, scientific = FALSE, 
                                 my_format = NULL) 
{
  
  label_format = "%.2f"
  if (!is.null(my_format)) 
    label_format <- my_format
  N_features <- data.table::setDT(data_long)[, data.table::uniqueN(variable)]
  if (is.null(dilute)) 
    dilute = FALSE
  nrow_X <- nrow(data_long)/N_features
  if (dilute != 0) {
    dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute))))
    set.seed(1234)
    data_long <- data_long[sample(nrow(data_long), min(nrow(data_long)/dilute, 
                                                       nrow(data_long)/2))]
  }
  x_bound <- if (is.null(x_bound)) 
    max(abs(data_long$value)) * 1.1
  else as.numeric(abs(x_bound))
  plot1 <- ggplot(data = data_long) + coord_flip(ylim = c(-x_bound, 
                                                          x_bound)) + geom_hline(yintercept = 0) + ggforce::geom_sina(aes(x = variable, 
                                                                                                                          y = value, color = stdfvalue), method = "counts", maxwidth = 0.7, 
                                                                                                                      alpha = 0.7) + geom_text(data = unique(data_long[, c("variable", 
                                                                                                                                                                           "mean_value")]), aes(x = variable, y = -Inf, label = sprintf(label_format, 
                                                                                                                                                                                                                                        mean_value)), size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold", vjust = -0.5) + 
    scale_color_gradient(low = "#FFE28A", high = "#6600CC", 
                         #        scale_color_gradient(low = "#FFCC33", high = "#6600CC", 
                         breaks = c(0, 1), labels = c(" Low", "High "), guide = guide_colorbar(barwidth = 12, 
                                                                                               barheight = 0.3)) + theme_bw() + theme(axis.line.y = element_blank(), 
                                                                                                                                      axis.ticks.y = element_blank(), legend.position = "none", 
                                                                                                                                      legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
                                                                                                                                      axis.title.x = element_text(size = 10)) + scale_x_discrete(limits = rev(levels(data_long$variable)), 
                                                                                                                                                                                                 labels = MakeExp(rev(features),bold_me)) + # rev(features)) + 
    labs(y = "SHAP value (impact on model output)", x = "", 
         color = "Feature value  ")
  return(plot1)
}

shap.plot.summary.wrap3 <- function (shap_score, X, top_n, dilute = FALSE) 
{
  if (missing(top_n)) 
    top_n <- dim(X)[2]
  if (!top_n %in% c(1:dim(X)[2])) 
    stop("supply correct top_n")
  shap_long2 <- shap.prep(shap_contrib = shap_score, X_train = X, 
                          top_n = top_n)
  shap.plot.summary1(shap_long2, dilute = dilute)
}

print(shap.plot.summary.wrap3(as.data.frame(as.matrix(shap_values$shap_score)[,features]), # Ugly, but worky.
                              as.matrix(hold_out_features)[,features],
                              dilute = 1) + ylab("SHAP Values"))

png(
  here("graphs/xgb_shap.png"),
  width     = 5,
  height    = 3.25,
  units     = "in",
  res       = 1200,
  pointsize = 4
)
print(shap.plot.summary.wrap3(as.data.frame(as.matrix(shap_values$shap_score)[,features]), # Ugly, but worky.
                        as.matrix(hold_out_features)[,features],
                        dilute = 1) + ylab("SHAP Values"))
dev.off()
rm(list = ls())