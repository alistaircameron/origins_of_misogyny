source(here("code/helper_functions.R"))
# Individual characteristics, repeat of preprocessing as per XGBoost.
  # sole difference: weighting not performed natively within gpboost, so, select a random sample, with replacement, such that each country is equally weighted. 


ivs_df <- load_ivs()
ivs_df <- select(ivs_df, -"F199") # EVS doessn't include this.
gender_norms <- c("D059", "E233", "D060", "C001", "D078", "F120")
princ.comp <- principal(select(ivs_df,all_of(gender_norms)), nfactors = 1)
ivs_df$gender <- -1*(as.numeric(princ.comp$scores)) # -1 so that the correlations are possible. 
colnames(ivs_df[ , colSums(is.na(ivs_df)) == 0]) 
df_0 <- select(ivs_df, -c("counter", "citizen", "m_immigrant", "f_immigrant", "parent_immigrant", "immigrant", "parents_cohabit")) # Remove the useless stuff --- don't know why I didn't do this in stata??
var <- "gender"
load(here("intermediate_data/deep.dets.RData"))
df <- filter(df_0, c_code_n %in% deep.dets$c_code_n) # Just keep the countries for which there are heritage variables.

final.df <- data.frame() # Create an empty df
for (iter in 5:7) {
  # 1. keep if wave = 5 & !missing(gender)
  temp <- df %>% 
    filter(wave == iter, !is.na(get(var)))
  # 2. Get the weights, corrected for the fact that some individuals don't respond to each question (and therefore, the   true sample size is not eg. 1000, even though the weights are made as if each individual in the survey responds to each question)
  temp1 <- df %>% 
    filter(wave == iter, !is.na(get(var))) %>% 
    group_by(country) %>% 
    summarise(w_gender = 1000/sum(weight)) %>% # The WVS is meant to give 1000 ppl weights
    ungroup()
  # 3. Merge the first two datasets
  temp2 <- left_join(temp, temp1, by = "country")
  # 4. Get the corrected weight & rbind the dataframes
  temp2$weight_gender <- temp2$w_gender * temp2$weight
  final.df <- rbind(final.df, temp2)
}

final.df <- final.df %>% 
  group_by(country) %>% 
  mutate(no_waves = length(unique(wave))) %>% 
  ungroup

final.df$weight_gender <- final.df$weight_gender / final.df$no_waves
colnames(final.df)[grep("weight_gender", colnames(final.df))] <- "final_weight"
df <- dplyr::select(final.df, -c(intersect(colnames(final.df),c("government_wave", "gender_wave", "inequality_wave", "no_waves", "w_gender", "gender"))))
df1 <- as.data.frame(sapply(df,as.numeric))
# Rescuing my strings: 
df1$country  <- df$country
df1$country_code <- df$country_code
df <- df1
rm(df1)

# Labelling categorical variables as factors in human-readable form.
ordered_factors <- c()
for (fac in ordered_factors) {
  df[[fac]] <- factor(df[[fac]], ordered = T)
  colnames(df)[colnames(df) == paste0(fac)] <- paste0(fac,".f")
}

# Below is from WVS, need to check again for the IVS. 
unordered_factors <- c("religion", "s_occupation", "s_employment", "employment", "occupation", "sector", "urban", 'wave', 'country')
unordered_factors <- intersect(unordered_factors, colnames(df))

for (fac in unordered_factors) {
  df[[fac]] <- factor(df[[fac]], ordered = F)
  colnames(df)[colnames(df) == paste0(fac)] <- paste0(fac,".f")
}

# Remove features with zero or near-zero variance. Note that I've used the Caret defaults.
nzv <- nearZeroVar(df, names = T, foreach = T, allowParallel = T) # The variables that I will remove.
nzv <- setdiff(nzv, c("government_wave", "inequality_wave","gender_wave", "finaldata")) # don't let it remove these guys. 
df <- select(df, -all_of(nzv)) 

# Remove if there are any missing values and check correlations
for.corr.matrix <- select(df[ , colSums(is.na(df)) == 0], -all_of(intersect(colnames(df[ , colSums(is.na(df)) == 0]), c(gender_norms, "final_weight", "weight", "S001", "c_code_n")))) # In '-c', remove stuff you don't want to be used.

# Remove highly correlated features. (note that this has to be done prior to one-hot-encoding, and prior to merging with the IVS, because, of course the IVS are highly correlated after you've bloody well merged!)
highly_correlated <- findCorrelation(
  cor(for.corr.matrix[,sapply(for.corr.matrix, is.numeric)]), # only do this to the numeric guys. 
  cutoff = 0.8, # Again, this is probably conservative.
  verbose = F,
  names = T,
  exact = T
)
highly_correlated <- setdiff(highly_correlated, c("weight_gender", "weight_inequality", "weight_government"))
df <- select(df, -all_of(highly_correlated))

# Just get latest wave that we've got data for, and merge.
for (i in colnames(df)) {  colnames(df)[colnames(df) == paste0(i)] <- paste0("wvs.",i) } # relabel everything so it starts with wvs
df <- rename(df, "c_code_n" = "wvs.c_code_n")
wvs <- merge(df, deep.dets, by = "c_code_n") # merge with dds
wvs <- select(wvs, -c(c_code_n, wvs.country.f, country))
wvs <- rename(wvs, "wvs.country.f" = "wvs.country_code")

# Get a list of factors with just two levels (ie. the dummy variables); because, I want these excluded from the one hot encoding.
temp <- wvs[,sapply(wvs, is.factor)] # grab the factors
binary <- c() # make an empty list

# Add guys to the list if they've got just two levels 
for(i in 1:ncol(temp)){
  if(length(levels(temp[,i])) == 2){
    binary <- c(binary, colnames(temp[i]))
  }
}

# One-Hot Encode the categorical variables.
binary.plus <- union(binary, c("wvs.country.f"))
dummies_model <- dummyVars( ~ ., 
                            data=select(wvs, -c(binary.plus)))
dummies_model[["facVars"]] # Which are the factor variables that are being one-hot encoded
wvs <- cbind(as.data.frame(predict(dummies_model, newdata = wvs)),wvs[,c(binary.plus)]) # do the one-hot encoding and add back in the binary variables

for(i in binary)(wvs[[i]] <- as.numeric(wvs[[i]]))
wvs <- select(wvs, -c("wvs.S001"))

# Train Test Split
set.seed(007) # JB
gender_norms <- paste0("wvs.", gender_norms) # rename the gender_norms
for(i in 1:10){
  ti <- runif(nrow(wvs))
  trainIndex <- as.data.frame(which(ti <= 0.65))
  colnames(trainIndex)[1] <- "index"
  wvs$wvs.gender <- NA
  wvs$wvs.gender[-(unlist(trainIndex))] <- -1*(as.numeric((principal(select(wvs[-(unlist(trainIndex)),],all_of(gender_norms)), nfactors = 1))$scores)) # PCA with just the test data
  wvs$wvs.gender[(unlist(trainIndex))] <- -1*(as.numeric((principal(select(wvs[(unlist(trainIndex)),],all_of(gender_norms)), nfactors = 1))$scores)) # PCA with just the train data
  wvs$wvs.train <- 1
  wvs$wvs.train[-unlist(trainIndex)] <- 0
  colnames(wvs)[colnames(wvs) == "wvs.train"] <- paste0("wvs.train",i)
  colnames(wvs)[colnames(wvs) == "wvs.gender"] <- paste0("wvs.gender",i)
}

colnames(wvs)[grep("wvs.weight_",colnames(wvs))] <- "wvs.weight"
wvs$wvs.weight <- NULL
colnames(wvs)[grep("wvs.final_weight", colnames(wvs))] <- "wvs.weight"

# Now, remove countries that have less than 500 respondents per wave. A cut-off had to be chosen as some countries have very few responses per wave. 
# Group the OHE variables.
wvs.5 <- wvs %>% 
  group_by(wvs.country.f) %>% 
  filter(wvs.wave.f.5 == 1) %>%
  mutate(n = n()) %>% 
  ungroup
wvs.6 <- wvs %>% 
  group_by(wvs.country.f) %>% 
  filter(wvs.wave.f.6 == 1) %>%
  mutate(n = n()) %>% 
  ungroup
wvs.7 <- wvs %>% 
  group_by(wvs.country.f) %>% 
  filter(wvs.wave.f.7 == 1) %>%
  mutate(n = n()) %>% 
  ungroup
wvs <- rbind(wvs.5,wvs.6,wvs.7)
wvs <- filter(wvs, n >= 500)

# The crux of the matter; GPBoost doesn't handle weights internally. The below ensures each country has equal representation. 
df <- data.frame()
for(wave in 5:7){
  for(country in unique(wvs$wvs.country.f)){
    df1 <- wvs %>% 
      filter(wvs.country.f == country & get(paste0("wvs.wave.f.",wave)) == 1)
    if(nrow(df1)>0){
      df1 <- df1[
        sample(x = 1:nrow(df1),
               size = 1000,
               replace = TRUE,
               prob = df1$wvs.weight)
        ,]
      df <- rbind(df,df1)
    }
  }
}

gender <- select(df,-c(all_of(gender_norms), "wvs.government", "wvs.inequality"))
save(gender, file = here("intermediate_data/gpb.ready.RData"))






#######################
# GPBoost Grid Search #
#######################
rsq <- function (x, y) cor(x, y) ^ 2
n <- parallelly::availableCores()
params <- list(objective = "regression_l2", num_threads = n)

var <- "gender"
load(here("intermediate_data/gpb.ready.RData"))
gender$train <- gender$wvs.train1
for(i in 1:10)(gender[[paste0("wvs.train",i)]] <- NULL)
gender$gender <- gender$wvs.gender1
for(i in 1:10)(gender[[paste0("wvs.gender",i)]] <- NULL)

gender$wvs.wave.f.5 <- gender$wvs.wave.f.6 <- gender$wvs.wave.f.7 <- NULL # Remove waves. 
gender$wvs.weight <- gender$n <- gender$country_code <- NULL

test <- select(filter(gender, train == 0), -train)

# Splitting the test into validation and hold out sets.
set.seed(007) # J.B.
true_hold_out_index <- sample(1:nrow(test),size = 0.15*nrow(gender), replace = F)
true_hold_out <- test[-true_hold_out_index,]
test <- test[true_hold_out_index,]
train <- select(filter(gender, train == 1), -train)

X <- train %>%
  select(-c(starts_with("wvs."),var))

random_effects <- train %>% 
  select(-c("wvs.country.f", var)) %>% 
  select(starts_with("wvs."))

y <- select(train, var)

group_data <- train %>%
  select(starts_with("wvs.country.f")) %>% 
  as.matrix()

Xtest <- test %>%
  select(-c(starts_with("wvs."),var))

random_effects_test <- test %>% 
  select(-c("wvs.country.f", var)) %>% 
  select(starts_with("wvs."))

ytest <- select(test, var)
group_data_test <- test %>%
  select(starts_with("wvs.country.f")) %>% 
  as.matrix()

# The GPModel defines how we handle the random effects. 
gp_model <- GPModel(group_data = group_data,  
                    group_rand_coef_data = as.matrix(random_effects), 
                    ind_effect_group_rand_coef = c(rep(1,ncol(random_effects))),
                    drop_intercept_group_rand_effect = c(TRUE)
)

dtrain <- gpb.Dataset(as.matrix(X), label = as.matrix(y))
param_grid = expand.grid("learning_rate" = c(0.1, 0.05, 0.01),
                         "min_data_in_leaf" = c(500, 2000, 10000),
                         "bagging_fraction" = c(0.5, 0.67, 0.8, 1),
                         "max_depth" = c(1, 3, 5, 6, 8, 9, 12),
                         "feature_fraction" = c(0.5, 0.67, 0.8, 1),
                         "min_gain_to_split" = c(0, 0.01, 0.1),
                         "num_leaves" = 2^17,
                         optimal_trees = 0,
                         min_RMSE = 0,
                         valid_RMSE = 0
)

opt_params <- param_grid[sample(1:nrow(param_grid),50),] # random search across the parameters.
opt_params <- opt_params[order(opt_params$max_depth),] # re-order

for(i in 1:nrow(opt_params)){
  set.seed(007) # So, that no matter how you run it in parallel, output will be the same. 
  bst <- gpb.cv(params = params,
                data = dtrain,
                gp_model = gp_model,
                nrounds = 10000, 
                nfold = 5,
                eval = "l2",
                learning_rate = opt_params$learning_rate[i],
                max_depth = opt_params$max_depth[i],
                min_data_in_leaf = opt_params$min_data_in_leaf[i],
                bagging_fraction = opt_params$bagging_fraction[i],
                feature_fraction = opt_params$feature_fraction[i],
                num_leaves = opt_params$num_leaves[i],
                min_gain_to_split = opt_params$min_gain_to_split[i],
                early_stopping_rounds = 50,
                verbose = 0,
                use_gp_model_for_validation = TRUE)
  
  opt_params$optimal_trees[i] <- bst$best_iter
  opt_params$min_RMSE[i] <- bst$best_score
  
  # Testing on the validation set.
  system.time(gpbst <- gpboost(data = as.matrix(X),
                               label = as.matrix(y),
                               gp_model = gp_model,
                               nrounds = opt_params$optimal_trees[i],
                               learning_rate = opt_params$learning_rate[i],
                               max_depth = opt_params$max_depth[i],
                               min_data_in_leaf = opt_params$min_data_in_leaf[i],
                               bagging_fraction = opt_params$bagging_fraction[i],
                               colsample_bytree = opt_params$feature_fraction[i],
                               num_leaves = opt_params$num_leaves[i],
                               objective = "regression_l2",
                               verbose = 0,
                               eval_freq = 50,
                               num_threads = n,
                               use_gp_model_for_validation = TRUE))
  
  pred <- predict(gpbst,
                  data = as.matrix(Xtest),
                  group_data_pred = group_data_test,
                  group_rand_coef_data_pred = as.matrix(random_effects_test),
                  predict_var= TRUE,
                  pred_latent = TRUE
  )
  
  pred$predictions <- pred$random_effect_mean + pred$fixed_effect  # Sum to obtain a single prediction
  
  # Getting back RMSE
  squared.error <- (pred$predictions - ytest)^2
  mse <- sum(squared.error) / nrow(squared.error)
  opt_params$valid_RMSE[i] <- sqrt(mse)
  save(opt_params, file = here(paste0("intermediate_data/gpb_opt_params.RData")))
  print(paste0("Completed run no. ", i))
}

opt_params <- opt_params[order(opt_params$valid_RMSE),]
cor(opt_params$min_RMSE, opt_params$valid_RMSE) # Close to 1, overfitting to training not an issue.

save(opt_params, file = here("intermediate_data/gpb_opt_params.RData"))




# # # Taking it to the hold-out set. # # # 

opt_params <- opt_params[order(opt_params$valid_RMSE),] # re-order 
i <- 1
system.time(gpbst <- gpboost(data = as.matrix(X),
                             label = as.matrix(y),
                             gp_model = gp_model,
                             nrounds = opt_params$optimal_trees[i],
                             learning_rate = opt_params$learning_rate[i],
                             max_depth = opt_params$max_depth[i],
                             min_data_in_leaf = opt_params$min_data_in_leaf[i],
                             bagging_fraction = opt_params$bagging_fraction[i],
                             colsample_bytree = opt_params$feature_fraction[i],
                             num_leaves = opt_params$num_leaves[i],
                             objective = "regression_l2",
                             verbose = 1,
                             eval_freq = 50,
                             num_threads = n,
                             use_gp_model_for_validation = TRUE))

# Hold Out Set
X_ho <- true_hold_out %>%
  select(-c(starts_with("wvs."),var))

random_effects_ho <- true_hold_out %>% 
  select(-c("wvs.country.f", var)) %>% 
  select(starts_with("wvs."))

y_ho <- select(true_hold_out, var)

group_data_ho <- true_hold_out %>%
  select(starts_with("wvs.country.f")) %>% 
  as.matrix()

pred <- predict(gpbst,
                data = as.matrix(X_ho),
                group_data_pred = group_data_ho,
                group_rand_coef_data_pred = as.matrix(random_effects_ho),
                predict_var= TRUE,
                pred_latent = TRUE # Now needed for pred$predictions
)

pred$predictions <- pred$random_effect_mean + pred$fixed_effect  # Sum them to obtain a single prediction

# Getting back RMSE
squared.error <- (pred$predictions - y_ho)^2
mse <- sum(squared.error) / nrow(squared.error)
rmse <- sqrt(mse)
r2 <- rsq(y_ho,pred$predictions)

print(paste0("GPBoost RMSE is: ",rmse, " and GPBoost r-squared is: ", r2))

gpb.imp <- gpb.importance(gpbst, percentage = TRUE)
# png(here('graphs/gpb_importance.png'))
# print(gpb.plot.importance(gpb.imp, top_n = 10L, measure = "Gain"))
# dev.off()




                  #######################
                   # SHAP for GPBoost #
                  #######################
shap_values <- shap.values(gpbst, 
                           as.matrix(X_ho))

shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(X_ho)) # as above.

# Take the top features as measured by shap_values
features <- unique(shap_long[,2])[1:10]
features <- data.frame(lapply(features, as.character), stringsAsFactors=FALSE)
features <- unlist(features)

feature_names <- c("Marriage - bride price", "Temperate", "Temperate - no dry season", "UV exposure", "Folklore - tentative", "Folklore - purity", "Distance to regional fontier", "Near coast", "Folklore - purity is virtuous", "Folklore - feel")

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
                         breaks = c(0, 1), labels = c(" Low", "High "), guide = guide_colorbar(barwidth = 12, 
                                                                                               barheight = 0.3)) + theme_bw() + theme(axis.line.y = element_blank(), 
                                                                                                                                      axis.ticks.y = element_blank(), legend.position = "none", 
                                                                                                                                      legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
                                                                                                                                      axis.title.x = element_text(size = 10)) + scale_x_discrete(limits = rev(levels(data_long$variable)), 
                                                                                                                                                                                                 labels = rev(feature_names)) + 
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

png(
  here("graphs/gpb_shapley.png"),
  width     = 5,
  height    = 3.25,
  units     = "in",
  res       = 1200,
  pointsize = 4
)
print(shap.plot.summary.wrap3(as.data.frame(as.matrix(shap_values$shap_score)[,features]),
                        as.matrix(X_ho)[,features],
                        dilute = 1) + ylab("SHAP Values"))
dev.off()

rm(list = ls())