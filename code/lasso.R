load(here("intermediate_data/lasso_ready.RData"))
df$wvs.country.f <- as.character(df$country_code)

# The nodes.
nodes <- read.csv(here("intermediate_data/country_export.csv"))
colnames(nodes) <- c('wvs.country.f', 'node1', 'node2', 'node3')

joined <- left_join(df, nodes, by = "wvs.country.f")
joined <- select(joined, -c("country_code", 'c_code_n', 'government', 'inequality'))

gender_norms <- c("D059", "E233", "D060", "C001", "D078", "F120") # used later.
df0 <- select(joined, -c('country.f', 'wave.f', 'final_weight', 'S001')) # well, have to drop one of the weights. probably the weight_final is the better weight to use. but it is the more convoluted weight. So i drop it.

df0$`Lower Education` <- ifelse(df0$education == 1, 1,0)
df0$`Mid Education` <- ifelse(df0$education == 2, 1,0)
df0$`Higher Education` <- ifelse(df0$education == 3, 1,0)
df0 <- select(df0, -'education')

# one-hot encode
dummy <- dummyVars(" ~ .", 
                   data=select(df0, c('religion.f', 'employment.f') )
                   )
ohe <- data.frame(predict(dummy, newdata=df0))
df0 <- cbind(select(df0, -c('religion.f', 'employment.f')), ohe)

keys <- c("religion.f.0" ,    "religion.f.1"  ,   "religion.f.2"  ,   "religion.f.3"  ,   "religion.f.5",  "religion.f.6" ,    "religion.f.7"  ,   "religion.f.8"  ,   "religion.f.9", "employment.f.1"  , "employment.f.2" ,  "employment.f.3"  , "employment.f.4"  , "employment.f.5" ,  "employment.f.6"  , "employment.f.7"  , "employment.f.8", 'religion_country', 'male', 'population', 'age', 'hh_size', 'no_children')
values <- c("Not religious" ,    "Roman Catholic"  ,   "Protestant"  ,   "Orthodox"  ,   "Muslim",  "Hindu" ,    "Buddhist"  ,   "Other Christian"  ,   "Other religion", "Employed full time"  , "Employed part time" ,  "Self employed"  , "Retired"  , "Housewife" ,  "Students"  , "Unemployed"  , "Employed - other", '% Same Religion', 'Male', 'Town size', 'Age', 'H.H. size', 'No. children')

# Rename columns
for(i in 1:length(keys)) {colnames(df0)[grep(keys[i],colnames(df0))] <- values[i]}

df0$node <- df0$node1
df0 <- select(df0, -c('node1', 'node2', 'node3'))

# Re-order the columns.
a <- colnames(df0)
b <- c('Male', 'Lower Education', 'Mid Education', 'Higher Education', 'Age', 'income', 'married', 'Town size',  'H.H. size', 'No. children', 'minority', '% Same Religion', "Not religious" ,    "Roman Catholic"  ,   "Protestant"  ,   "Orthodox"  ,   "Muslim",  "Hindu" ,    "Buddhist"  ,   "Other Christian"  ,   "Other religion" , "Employed full time"  , "Employed part time" ,  "Self employed"  , "Retired"  , "Housewife" ,  "Students"  , "Unemployed"  , "Employed - other", 'node', 'weight', "C001" ,         "D059"     ,     "D060"    ,      "D078"      ,    "E233", 'F120', 'wvs.country.f')
df0 <- df0[,b]

lower <- c('minority', 'income', 'married')
upper <- c('Minority', 'Income', 'Married')

for(i in 1:length(lower)){ colnames(df0)[grep(lower[i], colnames(df0))] <- upper[i]}

# Create a place to dump results. 
latex_table = list()
node_results <- list()
r2train <- list()
r2test <- list()

suppressWarnings({ suppressMessages({
set.seed(007) # J.B.
for(n in 1:length(unique(df0$node))){
  node_results[[n]] <- list()
  
  right <- filter(df0, node == n)
  right <- 
    right %>% 
    mutate(weight = importance_weights(weight))
  
  princ.comp <- principal(
    select(
      right,
      all_of(gender_norms)), 
    nfactors = 1)
  right$misogyny <- -as.numeric(princ.comp$scores)
  
  right <- select(right, -all_of(gender_norms))
  
  # Train/test split
  right_split <- initial_split(right, strata = 'wvs.country.f') # using defaults, proportion is 0.75
  right_train <- training(right_split)
  right_test <- testing(right_split)
  
  # Now, for the Elastic Net:
  # Prep the data (i've done most of this by hand, but got to normalise.)
  right_recipe = recipe(misogyny ~ .,data = right) %>% 
    #    update_role(wvs.country.f, new_role = "stratifier") %>% 
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors() & all_numeric()) %>% # Normalise the numeric varaibles.
    step_dummy(all_predictors() & all_nominal())    # Create dummies for categorical variables. I did this manually, so probably no need?
  
  # Cross validations:
  right_cv = right %>%  vfold_cv(v = 10)
  
  # Tune range of λ 
  lambdas = 10^seq(from = 3, to = -3, length = 100)
  
  # Define the model
  right_net = linear_reg(penalty = tune(),
                         mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  # Define the workflow
  workflow_net = workflow() %>%
    add_model(right_net) %>%
    add_recipe(right_recipe) %>%
    add_case_weights(weight)

  alphas = 1 # Lasso
  cv_net = 
    workflow_net %>%
    tune_grid(
      right_cv,
      grid = expand_grid(mixture = alphas, 
                         penalty = lambdas),
      metrics = metric_set(rmse, rsq, mae)
    )
  
  lowest_rmse <- cv_net %>%
    select_best("rmse")
  
  final_net <- finalize_workflow(
    workflow_net,
    lowest_rmse
  )
  
  test_prediction <- fit(final_net, right_train) %>% 
    predict(right_test)
  train_prediction <- fit(final_net, right_train) %>% 
    predict(right_train)
  
  preds_train <- as.data.frame(cbind(train_prediction, right_train$misogyny))
  preds_test <- as.data.frame(cbind(test_prediction, right_test$misogyny))
  
  colnames(preds_test) <- c('prediction', 'actual')
  colnames(preds_train) <- c('prediction', 'actual')
  
  rsq_train <- rsq(preds_train, 'prediction', 'actual')
  rsq_test <- rsq(preds_test, 'prediction', 'actual')
  
  r2train[[n]] <- rsq_train
  r2test[[n]] <- rsq_test
  
  coeffs_train <- final_net %>% 
    fit(right_train) %>%
    pull_workflow_fit() %>%
    tidy()
  coeffs_train$abs_value <- abs(coeffs_train$estimate)
  
  coeffs_test <- final_net %>% 
    fit(right_test) %>%
    pull_workflow_fit() %>%
    tidy()
  coeffs_test$abs_value <- abs(coeffs_test$estimate)
  
  results_lasso <- list()
  
  # Last fit performs the final fit on the entire training set and is then evaluated on the test set.
  results_lasso[[1]] <- last_fit(
    final_net,
    right_split
  ) %>%
    collect_metrics()
  
  # Running on test.
  var_imp <- final_net %>%
    fit(right_test) %>%
    pull_workflow_fit() %>%
    vi(lambda = lowest_rmse$penalty) %>%
    mutate(
      Importance = abs(Importance),
      Variable = fct_reorder(Variable, Importance)
    )
  
  # Remove the country dummies from the variable importance
  var_imp <- var_imp[-grep('wvs.country.f', var_imp$Variable),]
  
  results_lasso[[2]]     <- arrange(coeffs_test, -abs_value)
  node_results[[n]][[1]] <- results_lasso
  node_results[[n]][[2]] <- lowest_rmse$penalty
  node_results[[n]][[3]] <- results_lasso[[1]]$.estimate[2]
  
  # Tidy the workflow.
  latex_table[[n]] <- final_net %>%
    fit(right_test) %>%
    pull_workflow_fit() %>% 
    tidy()
}

term <- latex_table[[1]]$term
term <- term[-grep('wvs.country.f', term)]

# Create the table
ltable <- as.data.frame(term)

for (i in 1:length(latex_table)){
  for_join <- as.data.frame(latex_table[[i]][,c(1,2)])
  colnames(for_join) <- c('term', paste0("Node ",i))
  ltable <- left_join(ltable, for_join)
}

# Want to order things so that it is the same as the pretty figure.
ltable <- ltable[term != "(Intercept)",] %>% 
  mutate(abs = abs(`Node 1`)) %>% 
  arrange(desc(abs)) %>% 
  select(-abs)

# Round the table: 
ltable[,2:ncol(ltable)] <- round(ltable[,2:ncol(ltable)],3)

# Add performance metrics
lambda <- c("lambda")
r2 <- c("R-squared")
n <- c("N")
cfe <- c("Country F.E.")

for(i in 1:length(latex_table)){
  lambda <- append(lambda,round(latex_table[[i]][["penalty"]][1],3))
  r2 <- append(r2,round(r2test[[i]][[".estimate"]],3))
  n <- append(n, nrow(df0[df0$node == i,]))
  cfe <- append(cfe, "Y")
}

ltable <- rbind(ltable, cfe, lambda, n, r2)
ltable[ltable == 0 | is.na(ltable) ] <- '--'

# Save the coefficients
print(xtable(ltable, type = "latex"), booktabs = T, include.rownames = F, file = here("intermediate_data/lasso_coeffs.tex"))

ltable <- as.data.frame(term)
for (i in 1:length(latex_table)){
  for_join <- as.data.frame(latex_table[[i]][,c(1,2)])
  colnames(for_join) <- c('term', paste0(i))
  #  colnames(for_join) <- c('term', paste0("Node ",i))
  ltable <- left_join(ltable, for_join)
}

lt <- ltable[2:nrow(ltable),2:ncol(ltable)]
rownames(lt) <- ltable[2:nrow(ltable),1]

# Remplace those forced to zero with "NA"
for(i in 1:ncol(lt)){
  for(j in 1:nrow(lt)){
    if(lt[j,i] == 0 || is.na(lt[j,i])){
      lt[j,i] <- NA
    }
  }
}

# Remove all zeroed rows and order by the first node.
lt <- lt[rowSums(is.na(lt)) != ncol(lt), ]
lt <- lt %>% 
  mutate(temp = abs(lt$`1`)) 

df <- lt[order(lt$temp, na.last = F), ] %>% 
  select(-temp) %>% 
  as.matrix() %>% 
  t() %>% 
  melt()

colnames(df) <- c("x", "y", "value")
df[df$x == 8,"x"] <- "8,10" # Note that Node 8 is actually Node 8,10
save(df, file = here("intermediate_data/lasso_heatmap.RData"))

# Heatmap.
ggplot(df, aes(x = factor(x), y = y, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  coord_fixed(ratio = 0.5) + 
  guides(fill = guide_colourbar(label = TRUE,
                                ticks = FALSE)
  ) +
  labs(title = "",
       x = "Node",
       y = "",
       fill = "Coefficient Value")+
  theme_minimal() 
ggsave("graphs/lasso_heatmap.png")
}) })