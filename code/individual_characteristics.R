source(here("code/helper_functions.R"))
# Population weights and incorporating multiple waves per country.
# Use the 1,000 population weights to ensure each individual is weighted such that the sample is representative of the population, and each country has sum(weights) = 1,000
# Keep just the WVS if a country has observations in the 'same' wave in the WVS and the EVS.
# Thus, we include all the observations, and each country is weighted equivalently.


ivs_df <- load_ivs()
ivs_df <- select(ivs_df, -"F199")
gender_norms <- c("D059", "E233", "D060", "C001", "D078", "F120") # IVS
princ.comp <- principal(select(ivs_df,all_of(gender_norms)), nfactors = 1)
ivs_df$gender <- as.numeric(princ.comp$scores)

colnames(ivs_df[ , colSums(is.na(ivs_df)) == 0]) 
df_0 <- select(ivs_df, -c("counter", "citizen", "m_immigrant", "f_immigrant", "parent_immigrant", "immigrant", "parents_cohabit")) # Remove the useless stuff.

for(case in c("clustered", "benchmark")){
  if(case == "clustered"){
    load(here("intermediate_data/deep.dets.RData")) 
  }else{
    load(here("intermediate_data/dd_benchmark.RData"))  # For the benchmark case of all deep dets.
  }
  
  df <- filter(df_0, c_code_n %in% deep.dets$c_code_n) # Just keep the countries for which we have heritage variables.
  df <- df[!is.na(df$gender),]
  df$S001 <- as.numeric(as.character(df$S001))
  countries <- unique(df$country)
  five <- six <- seven <- list()
  length(five) <- length(six) <- length(seven) <- length(countries)
  
  for(i in 1:length(countries)){
    temp <- filter(df, df$country == countries[i] & df$wave == 5)
    five[i] <- length(unique(temp$S001))
    temp <- filter(df, df$country == countries[i] & df$wave == 6)
    six[i] <- length(unique(temp$S001))
    temp <- filter(df, df$country == countries[i] & df$wave == 7)
    seven[i] <- length(unique(temp$S001))
  }
  
  df <- df[!(df$country %in% countries[unlist(seven) == 2] & df$S001 == 1),]
  final.df <- data.frame() # Create an empty df
  for (iter in 5:7) {
    # 1. keep if wave = 5 & !missing(inequality)
    temp <- df %>% 
      filter(wave == iter , !is.na(gender))
    # 2. Get the weights, corrected for the fact that some individuals don't respond to each question (and therefore, the   true sample size is not eg. 1000, even though the weights are made as if each individual in the survey responds to      each question)
    temp1 <- df %>% 
      filter(wave == iter, !is.na(gender)) %>% 
      group_by(country) %>% 
      summarise(w_inequality = 1000/sum(weight)) %>% # The WVS is meant to give 1000 ppl weights
      ungroup()
    # 3. Merge the first two datasets
    temp2 <- left_join(temp, temp1, by = "country")
    # 4. Get the corrected weight
    temp2$weight_inequality <- temp2$w_inequality * temp2$weight
    # Rbind the dataframes.
    final.df <- rbind(final.df, temp2)
  }
  
  final.df <- final.df %>% 
    group_by(country) %>% 
    mutate(no_waves = length(unique(wave))) %>% 
    ungroup
  
  final.df$weight_inequality <- final.df$weight_inequality / final.df$no_waves
  
  colnames(final.df)[grep("weight_inequality", colnames(final.df))] <- "final_weight"
  
  df <- select(final.df, -c("government_wave", "gender_wave", "inequality_wave", "no_waves", "w_inequality", "gender"))
  
  df1 <- as.data.frame(sapply(df,as.numeric))
  
  # Rescuing my strings: 
  df1$country  <- df$country
  df1$country_code <- df$country_code
  df <- df1
  
  # Letting R know that categorical variables are factors and labeling them as such in human-readable form.
  ordered_factors <- c()
  for (fac in ordered_factors) {
    df[[fac]] <- factor(df[[fac]], ordered = T)
    colnames(df)[colnames(df) == paste0(fac)] <- paste0(fac,".f")
  }
  
  unordered_factors <- c("religion", "s_occupation", "s_employment", "employment", "occupation", "sector", "urban", 'wave', 'country')
  unordered_factors <- intersect(unordered_factors, colnames(df))
  
  for (fac in unordered_factors) {
    df[[fac]] <- factor(df[[fac]], ordered = F)
    colnames(df)[colnames(df) == paste0(fac)] <- paste0(fac,".f")
  }
  
  # Save for lasso.
  save(df, file = here("intermediate_data/lasso_ready.RData"))
  
  # Remove features with zero or near-zero variance. Note that I've used the Caret defaults.
  nzv <- nearZeroVar(df, names = T, foreach = T, allowParallel = T) # The variables that I will remove.
  nzv <- setdiff(nzv, c("government_wave", "inequality_wave","gender_wave", "finaldata")) # don't let it remove these guys. 
  df <- select(df, -all_of(nzv))
  
  # Remove if there are any missing values and check correlations
  for.corr.matrix <- select(df[ , colSums(is.na(df)) == 0], -all_of(intersect(colnames(df[ , colSums(is.na(df)) == 0]), c(gender_norms, "final_weight", "weight", "S001", "c_code_n")))) # In '-c', remove stuff you don't want to be used.
  
  # Remove highly correlated features.
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
  
  if(case != "clustered"){
    deep.dets$c_code_n <- as.numeric(as.character(deep.dets$c_code_n.f))
    deep.dets$c_code_n.f <- NULL
  }
  
  wvs <- merge(df, deep.dets, by = "c_code_n") # merge with dds
  wvs <- select(wvs, -c(c_code_n, wvs.country.f, country))

  # Exclude dummy variables from the one hot encoding.
  temp <- wvs[,sapply(wvs, is.factor)] # grab the factors
  binary <- c() # make an empty list
  
  # Add guys to the list if they've got just two levels 
  for(i in 1:ncol(temp)){
    if(length(levels(temp[,i])) == 2){
      binary <- c(binary, colnames(temp[i]))
    }
  }
  
  # One-Hot Encode the categorical variables.
  binary.plus <- union(binary, c("wvs.wave.f", "wvs.country_code"))
  dummies_model <- dummyVars( ~ ., 
                              data=select(wvs, -c(all_of(binary.plus))))
  dummies_model[["facVars"]] # Which are the factor variables that are being one-hot encoded
  wvs <- cbind(as.data.frame(predict(dummies_model, newdata = wvs)),wvs[,c(binary.plus)]) # do the one-hot encoding and add back in the binary variables
  
  # I now know that i can't have factor variables in. So turn these binary guys into numeric ones: 
  for(i in binary)(wvs[[i]] <- as.numeric(wvs[[i]]))
  wvs <- select(wvs, -c("wvs.S001", "wvs.wave.f"))
  
  # Train Test Split
  set.seed(007) # For reproducibility
  gender_norms <- c("D059", "E233", "D060", "C001", "D078", "F120")
  gender_norms <- paste0("wvs.", gender_norms) # rename the gender_norms
  
  # The following performs the train/test split a number of times (in essence, varying the seed set to do the split). I later check that results are robust to the split. They are. 
  for(i in 1:10){
    ti <- runif(nrow(wvs))
    trainIndex <- as.data.frame(which(ti <= 0.65)) # Train/test split
    colnames(trainIndex)[1] <- "index"
    
    # Gender
    wvs$wvs.gender <- NA
    wvs$wvs.gender[-(unlist(trainIndex))] <- as.numeric((principal(select(wvs[-(unlist(trainIndex)),],all_of(gender_norms)), nfactors = 1))$scores) # PCA with just the test data
    wvs$wvs.gender[(unlist(trainIndex))] <- as.numeric((principal(select(wvs[(unlist(trainIndex)),],all_of(gender_norms)), nfactors = 1))$scores) # PCA with just the train data
    wvs$wvs.gender <- -1*(wvs$wvs.gender) # Flip it so that bigger = more misogyny.
    wvs$wvs.train <- 1
    wvs$wvs.train[-unlist(trainIndex)] <- 0
    colnames(wvs)[colnames(wvs) == "wvs.train"] <- paste0("wvs.train",i)
    colnames(wvs)[colnames(wvs) == "wvs.gender"] <- paste0("wvs.gender",i)
  }
  
  colnames(wvs)[grep("wvs.weight_",colnames(wvs))] <- "wvs.weight"
  wvs$wvs.weight <- NULL
  colnames(wvs)[grep("wvs.final_weight", colnames(wvs))] <- "wvs.weight"
  
  save(wvs, file = here("intermediate_data/pca_analysis.Rdata")) # For analysing the Principal Components.
  
  gender <- select(wvs,-c(all_of(gender_norms), "wvs.government", "wvs.inequality"))
  
  if(case == "clustered"){
    save(gender, file = here("intermediate_data/wvs.ready.RData"))
  }else{
    gender[,setdiff(grep('country_code', colnames(gender)), grep('wvs.country_code', colnames(gender)))] <- NULL
    save(gender, file = here('intermediate_data/benchmark_wvs.RData'))
  }
}

rm(list = ls())
gc()