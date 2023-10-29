if(!exists("deep.dets")){
  source(here("code/import_heritage.R"))
}

suppressWarnings({
# Spearman Rank correlation.
newdf <- deep.dets[,which(sapply(deep.dets,is.numeric))] # need the numeric variables only
factors <- setdiff(setdiff(colnames(deep.dets), colnames(newdf)), c('country', 'country_code', 'c_code_n.f'))
newdf[,factors] <- lapply(deep.dets[,factors],as.numeric)
newdf2 <- newdf # copy. used in loop.

for(i in 1:ncol(newdf)){
  newdf[i] <- rank(newdf[i], na.last = T, ties.method = 'average')
  newdf[which(is.na(newdf2[i])), i] <- NA
}

# This gives spearman rank correlations. 
pairwise.complete <- cor(newdf, use = "pairwise.complete.obs", method = 'spearman') %>% 
  as.matrix()
save(pairwise.complete, file = here("intermediate_data/cluster_data.RData")) # used as an input later. 
distance_matrix = 1 - abs(pairwise.complete)
d <- as.dist(distance_matrix)

# Ward Hierarchical Clustering
fit <- hclust(d, method="ward.D") # No need to set seed.
silh <- list()
features <- list()
groups_rand_index <- list()
length(groups_rand_index) <- length(silh) <- length(features) <- 150
width <- list(silh = silh, features = features, groups_rand_index = groups_rand_index)

# Now, vary the number of clusters. You want to maximise `sil_width`. 
for(i in 2:length(features)){
  
  # plot(fit) # display dendogram
  groups <- cutree(fit, k=i) # cut tree into k clusters # No need to set seed.
  gs <- as.data.frame(groups) # So you can read the clusters off easily
  gs$row_nos <- 1:nrow(gs)
  sil <- silhouette(groups, d)
  silly <- cbind(sil[,1],sil[,2],sil[,3]) %>% as.data.frame()
  colnames(silly) <- c('cluster', 'neighbour', 'sil_width')
  silly$row_nos <- 1:nrow(silly)
  fs <- silly %>% 
    group_by(cluster) %>% 
    arrange(desc(sil_width), .by_group = T) %>% 
    filter(row_number()==1)
  
  # overall average silhouette width is the mean of average silhouette widths of the different clusters.
  wth <- silly %>% 
    group_by(cluster) %>% 
    summarise(mean = mean(sil_width))
  
  width[["silh"]][i] <- mean(wth$mean)
  width[['features']][[i]] <- filter(gs, row_nos %in% fs$row_nos) %>% rownames()
  width[['groups_rand_index']][i] <- silly[,1:2] # used later to check Rand_Index
}

# Optimal number of clusters.
n <- unlist(width[['silh']]) %>% 
  as.data.frame()
n$nos <- 1:nrow(n)
n <- n[order(n$., decreasing = TRUE), ]
opt_k <- n[1,'nos'] + 1 # as the 1st element of the 'width' list is empty.
features <- width$features[opt_k]

# Save a full copy of the data; all deep determinants. This is the benchmark case. 
dd1 <- rename(deep.dets, "c_code_n" = "c_code_n.f")
save(deep.dets, file = here("intermediate_data/dd_benchmark.RData"))
deep.dets <- select(deep.dets, all_of(c("country", "c_code_n.f", features[[1]]))) # keep only the centres of the clusters

feature_names <- fit$labels
deep.dets <- rename(deep.dets, "c_code_n" = "c_code_n.f")
save(deep.dets, width, opt_k, feature_names, file = here("intermediate_data/deep.dets.RData"))



# Robustness / Quality of clustering.
# The adjusted rand index is the standard test. Since adjustedRandIndex(a,b) requires vectors of the same length, I randomly permute some variables, and check if the clusters are robust.

# Store original optimal parameters
width_orig <- width
opt_k_orig <- opt_k
drop_everything = F

source(here("code/import_heritage.R"))

# As above:
newdf <- deep.dets[,which(sapply(deep.dets,is.numeric))]
factors <- setdiff(setdiff(colnames(deep.dets), colnames(newdf)), c('country', 'country_code', 'c_code_n.f'))
newdf[,factors] <- lapply(deep.dets[,factors],as.numeric)
newdf2 <- newdf # copy. used in loop.
for(i in 1:ncol(newdf)){
  newdf[i] <- rank(newdf[i], na.last = T, ties.method = 'average')
  newdf[which(is.na(newdf2[i])), i] <- NA
}

# New: randomly permuting to compare the clustering robustness.
results <- vector("list", 50)
set.seed(007) # JB
for(run in 1:length(results)){
  newdf <- newdf2
  j <-  sample(1:ncol(newdf), run, replace = F)
  for(s in j) (newdf[,s] <- sample(newdf[,s],nrow(newdf), replace = F) )
  pairwise.complete <- cor(newdf, use = "pairwise.complete.obs", method = 'spearman') %>% 
    as.matrix()
  distance_matrix = 1 - abs(pairwise.complete)
  d <- as.dist(distance_matrix)
  fit <- hclust(d, method="ward.D") # No need to set seed.
  silh <- list()
  features <- list()
  groups_rand_index <- list()
  length(groups_rand_index) <- length(silh) <- length(features) <- 150 # extending beyond 150 is not meaningful (you'd essentially not be clustering then, as groups would be on n = 1)
  width <- list(silh = silh, features = features, groups_rand_index = groups_rand_index)
  
  for(i in 2:length(features)){
    groups <- cutree(fit, k=i) # cut tree into k clusters # No need to set seed.
    gs <- as.data.frame(groups) # So you can read the clusters off easily
    gs$row_nos <- 1:nrow(gs)
    sil <- silhouette(groups, d) # No need to set seed
    silly <- cbind(sil[,1],sil[,2],sil[,3]) %>% as.data.frame()
    colnames(silly) <- c('cluster', 'neighbour', 'sil_width')
    silly$row_nos <- 1:nrow(silly)
    fs <- silly %>% 
      group_by(cluster) %>% 
      arrange(desc(sil_width), .by_group = T) %>% 
      filter(row_number()==1)
    wth <- silly %>% 
      group_by(cluster) %>% 
      summarise(mean = mean(sil_width))
    width[["silh"]][i] <- mean(wth$mean)
    width[['features']][[i]] <- filter(gs, row_nos %in% fs$row_nos) %>% rownames()
    width[['groups_rand_index']][i] <- silly[,1:2] # used later to check Rand_Index
  }
  # Finding the 'best' number of clusters.
  n <- unlist(width[['silh']]) %>% 
    as.data.frame()
  n$nos <- 1:nrow(n)
  n <- n[order(n$., decreasing = TRUE), ]
  opt_k <- n[1,'nos'] + 1
  
  # Now, compare groupings of original data with permuted data. 
  ari <- adjustedRandIndex(width[["groups_rand_index"]][[opt_k]], width_orig[["groups_rand_index"]][[opt_k_orig]])
  ami <- AMI(width[["groups_rand_index"]][[opt_k]], width_orig[["groups_rand_index"]][[opt_k_orig]])
  results[[run]] <- list(ari = ari, ami = ami, no_clusters = opt_k, 
                         features = width[['features']][[opt_k]],
                         data = newdf) # ditto
}

save(results, file = here("intermediate_data/rand_index_permuting.RData"))
rm(list = setdiff(ls(), "drop"))
})