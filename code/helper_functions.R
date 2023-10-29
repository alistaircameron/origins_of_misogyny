
# #  Your copy of ivs.dta is ordered differently to that we run our analysis on (Stata's 'sort' command produces irreproducible within group ordering https://www.stata.com/manuals/dsort.pdf), but is otherwise identical.
# The following extracts an ordering that will be consistent for any random stata sort:
# new <- read_dta(here("intermediate_data/ivs.dta"))
# old <- read_dta("secondary_filepath/ivs.dta")
# old$id <- 1:nrow(old)
# new <- new[do.call(order, new), ]
# old <- old[do.call(order, old), ]
# identical(new,old[colnames(new)])
# original_order <- old$id
# save(original_order, file = here("raw_data/order_vector.RData"))
  
# load ivs, and return same ordering.
load_ivs <- function() {
  ivs <- read_dta(here("intermediate_data/ivs.dta"))
  ivs <- ivs[do.call(order, ivs), ]
  load(here('raw_data/order_vector.RData'))
  ivs <- ivs[order(original_order), ]
  return(ivs)
}



# Variation explained by country fixed effects and by individuals' demographics.
var_explained <- function(verbose = FALSE){
  load(here('intermediate_data/wvs.ready.RData'))
  gender$country_fac_ <- as.factor(gender$wvs.country_code)
  country_explained <- lm(wvs.gender7 ~ country_fac_, data = gender) # use no. 7, as this is the iteration used for the main analysis.
  
  print(paste0("The share of variation explained by country fixed effects is: ", round(summary(country_explained)$r.squared,2))) # 0.35
  
  ics <- grep("wvs.", colnames(gender))
  ics <- colnames(gender)[grep("wvs.", colnames(gender))][1:27]
  a <- paste("wvs.gender7 ~ ",paste(ics, collapse="+"),sep = "")
  individual_explained <- lm(a, data = gender)
  print(paste0("The share of variation explained by individuals' demographics is: ", round(summary(individual_explained)$r.squared,2))) # 0.24
  
  if(verbose != FALSE){
    print(summary(country_explained))
    print(summary(individual_explained))
  }
  rm(list = ls())
}


# Analyse the Principal Component & Return Scree Plot.
scree_plot <- function(){
  gender_norms <- c("D059", "E233", "D060", "C001", "D078", "F120") # IVS
  gender_norms <- paste0("wvs.", gender_norms) # rename the gender_norms
  
  load(here("intermediate_data/pca_analysis.Rdata"))
  df <- wvs %>% 
    filter(wvs.train7 == 1) %>%  # The iteration used in the final analysis.
    select(gender_norms)
  
  pc1 <-  principal(df, nfactors = 1) # Just for the first principal component.
  pc1_scores <- pc1$scores
  
  cors <- cor(df, pc1_scores[,1]) # Correlation of each variable with the first principal component.

  nf = ncol(df) -1 
  pcn <-  principal(df, nfactors = nf) # Return all the principal components
  pcn_scores <- pcn$scores
  
  # Scree plot, using the eigenvalues:
  var_explained <- pcn[["values"]] / nf 
  
  
  png(
    here("graphs/scree_plot.png"),
    width     = 5,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  print(
    qplot(c(1:length(var_explained)), var_explained) +
    geom_line() +
    xlab("Principal Component") +
    ylab("Variance Explained") +
    #  ggtitle("Scree Plot") +
    ylim(0, 1) +
    xlim(1,length(var_explained)) +
    theme_bw()
  )
  dev.off()

  print("Scree plot saved in graphs folder")
  qp <- qplot(c(1:length(var_explained)), var_explained) + 
    geom_line() + 
    xlab("Principal Component") + 
    ylab("Variance Explained") +
    #  ggtitle("Scree Plot") +
    ylim(0, 1) +
    xlim(1,length(var_explained)) +
    theme_bw()
  
  rm(list = setdiff(ls(), c('qp', 'var_explained')))
  return(qp)
}






# Compare XGBoost's prediction accuracy on the full dataset, and on the centre-most variables only. 
compare_performance <- function(){
  load(here("intermediate_data/cluster_xgb_initial.Rdata"))
  load(here("intermediate_data/xgb_grid_clustered.RData"))
  
  for(x in ls()){
    assign(paste0("clust_",x),get(x))  
  }
  
  load(here("intermediate_data/benchmark_xgb.Rdata"))
  load(here("intermediate_data/xgb_grid_benchmark.RData"))
  
  # These differences are so small they're meaningless.
  print(paste0("% Difference in RMSE is: ", round(1 - clust_RMSE/RMSE,3)))
  print(paste0("% Difference in R2 is: ", round(1 - clust_rsq/rsq,3)))
  rm(list = ls())
}







# Return the features in the same cluster as the most important features.
# Extract the most important feature. Load the clusters. Find the feature, and it's cluster no. Filter for the same cluster.


other_features <- function(){
  load(here("intermediate_data/mean_shap_vals.RData"))
  features <- unique(shap_long[,2])[1:10]
  features[,1] <- sapply(features[,1], as.character)
  features <- unlist(features)
  
  load(here("intermediate_data/deep.dets.RData")) # Load the clusters
  features_in_clusters <- list()
  length(features_in_clusters) <- 10
  
  for(i in 1:10){
    posn <- grep(features[[i]], feature_names)
    if(length(posn) > 0){
      cluster_no <- width$groups_rand_index[[opt_k]][posn] # Groups_rand_index gives the group that each feature is in. [[opt_k]] is the best number of groups (according to Silhouette).
      other_features <- which(width$groups_rand_index[[opt_k]] == cluster_no)
      features_in_clusters[[i]] <- feature_names[other_features]
      
    }
  }
  
  for(i in 1:10) (  cat("Feature: ", features[i],"...","\n",features_in_clusters[[i]],"\n","\n")  )
}



# The dendrogram
dendrogram <- function(){

  load(here("intermediate_data/cluster_data.Rdata")) # The clusters
  load(here("intermediate_data/mean_shap_vals.Rdata")) # most important deep dets.
  load(here("intermediate_data/deep.dets.Rdata")) # Everything else.
  
  sv <- shap_values$mean_shap_score
  most_imp <- names(sv)[1:10]
  most_imp <- most_imp[-grep("wvs.", most_imp)]
  all_clusters <- colnames(deep.dets[3:ncol(deep.dets)])
  
  pc <- pairwise.complete %>% 
    as.data.frame()
  pc <- pc %>% 
    select(most_imp) %>% 
    filter(rownames(pc) %in% most_imp)
  
  pc <- round(pc,3) # needs ranking to be in terms of absolute correlation.
  
  rownames(pc) <- c('d', 'b', 'c', 'a')
  pc <- pc[order(row.names(pc)), ]
  
  colnames(pc) <- rownames(pc) <- c("Temperate", 'Folklore: challenge motifs', 'Temperate - no dry season', 'Bride Price')
  
  # Dendrogram.
  `%notin%` <- Negate(`%in%`)
  
  distance_matrix = 1 - abs(pairwise.complete)
  d <- as.dist(distance_matrix)
  
  # Ward Hierarchical Clustering
  fit <- hclust(d, method="ward.D") # No need to set seed.
  
  # Remove labels of less important deep dets.
  for(i in 1:length(fit$labels)){
    if(fit$labels[i] %notin% most_imp){
      fit$labels[i] <- ""
    }
  }
  
  # Change labels.
  replacement_labels <- c("--Temperate", 'Challenge--', '--T. no dry', '--Bride price')
  for(i in 1:length(most_imp)){
    for(j in 1:length(fit$labels)){
      if(most_imp[i] == fit$labels[j]){
        fit$labels[j] <- replacement_labels[i]
      }
    }
  }

  png(
    here("graphs/dendrogram.png"),
    width     = 5, # Changing this, alon with the height can help squeeze in longer variable names.
    height    = 5,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  print(fviz_dend(fit, k = 78,                 # k=78, coz that's the measured no. of best groups.
            cex = 0.5,                 # label size
            color_labels_by_k = TRUE,  # color labels by groups
            horiz = F,
            type = "circular"
  )
  )
  dev.off()
  
  ddgm <- fviz_dend(fit, k = 78,                 # k=78, coz that's the measured no. of best groups.
                    cex = 0.5,                 # label size
                    color_labels_by_k = TRUE,  # color labels by groups
                    horiz = F,
                    type = "circular")
  rm(list = setdiff(ls(), "ddgm"))
  return(ddgm)
}