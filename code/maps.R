# Maps
load(here("intermediate_data/cluster_xgb.RData"))

# I want leaf nodes to be increasing in node mean. So, order the leaf nodes by mean predicted value
temp <- select(ww[!duplicated(ww[,c('leaf.nodes')]),], c("leaf.nodes", "yval")) # unique leaf-nodes
temp <- temp[order(temp$yval),]   # order by increasing y-value
temp$ordered_lnode <- 1:nrow(temp)    # create a new 'ordered_leaf_node' variable
temp <- temp[,c("ordered_lnode", "leaf.nodes")]
ww <- merge(ww,temp, by = "leaf.nodes")
ww <- ww[order(ww$country, ww$ordered_lnode),] # This makes sure the row has the smallest node is recorded before the second smallest node and so on and so forth.

temp <- ww %>% 
  group_by(country) %>% 
  summarise(unique_nodes = length(unique(ordered_lnode))) # how many unique nodes is each country in?

# Get separate columns of the country and each leaf node that it is in.
if(max(temp$unique_nodes) == 1) (my.list <- list(first_node = NULL)
)
if(max(temp$unique_nodes) == 2) (my.list <- list(first_node = NULL, second_node = NULL)
)
if(max(temp$unique_nodes) == 3) (my.list <- list(first_node = NULL, second_node = NULL, third_node = NULL)
)
if(max(temp$unique_nodes) == 4) (my.list <- list(first_node = NULL, second_node = NULL, third_node = NULL, fourth_node = NULL)
)
for(i in 1:max(temp$unique_nodes)){
  my.list[[i]] <- ww %>% 
    group_by(country) %>% 
    filter(row_number() == i) %>% 
    as.data.frame() %>% 
    ungroup()
  
  my.list[[i]]$order.character <- as.character(my.list[[i]]$ordered_lnode)
  colnames(my.list[[i]])[4] <- paste0("ordered_lnode_", i)
  my.list[[i]] <- my.list[[i]] %>% 
    select(country,paste0("ordered_lnode_",i), order.character)
}

# Create final database which just has one row per country. 
final <- ww %>% 
  group_by(country) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# Grabs the node numbers
for(i in 1:max(temp$unique_nodes)){
  final <- left_join(final, my.list[[i]], by = "country")
  colnames(final)[ncol(final)] <- paste0("lnode_",i)
}

# Rename things
if(is.null(final$lnode_2)){ # if lnode2 is null, each country is in a single node
  final$l.nodes <- final$lnode_1
  final <- final[order(final$ordered_lnode),] # Order everything.
} else if(is.null(final$lnode_3)){ # if lnode3 is null, each country is in at most, 2 nodes
  final$l.nodes <- paste0(final$lnode_1,", ",final$lnode_2) # concatenate the column names
  final$l.nodes[is.na(final$lnode_2)] <- final$lnode_1[is.na(final$lnode_2)] # where node 2 is empty, replace it with node 1
  final <- final[order(final$ordered_lnode, final$ordered_lnode_2),] # Order everything.
} else{
  final$l.nodes <- paste0(final$lnode_1,", ",final$lnode_2, ", ", final$lnode_3)
  final$temporary <- paste0(final$lnode_1,", ",final$lnode_2)
  final$l.nodes[is.na(final$lnode_3)] <- final$temporary[is.na(final$lnode_3)] # where node 3 is empty, replace l.nodes with concatenation of lnodes_2 & 3
  final$l.nodes[is.na(final$lnode_2)] <- final$lnode_1[is.na(final$lnode_2)] # where node 2 is empty, replace l.nodes with node 1
  final <- final[order(final$ordered_lnode, final$ordered_lnode_2, final$ordered_lnode_3),] # Order everything.
}

final <- final[order(-final$ordered_lnode, -final$ordered_lnode_2),] # Order everything.
ww <- final %>% 
  select(country, l.nodes)
ls <- unique(ww$l.nodes) # used later in mapping
levels <- ls
levels[1] <- ls[2]
levels[2] <- ls[1]

# I now have the leaf nodes.

world.map <- map_data("world") # Get the shape(?) files. 
world.map <- left_join(world.map, iso3166, by = c('region' = 'mapname'))
colnames(ww) <- c("country","leaf.node")
world.map.joined <- left_join(world.map, ww , by = c('a3' = 'country'))
df <- unique(world.map.joined[c("leaf.node","a3")])
df <- na.omit(df)

# Correct inconsistencies in country namings across data sources
print(setdiff(ww$country, df$a3)) # CHN, FIN, NOR, GBR, ROM
world.map$a3 <- ifelse(world.map$region == "UK", "GBR", world.map$a3)
world.map$a3 <- ifelse(world.map$region == "China", "CHN", world.map$a3)
world.map$a3 <- ifelse(world.map$region == "Finland", "FIN", world.map$a3)
world.map$a3 <- ifelse(world.map$region == "Norway", "NOR", world.map$a3)
world.map$a3 <- ifelse(world.map$region == "Romania", "ROM", world.map$a3)
world.map.joined <- left_join(world.map, ww , by = c('a3' = 'country'))
world.map.joined$`leaf node` <- factor(world.map.joined$leaf.node, levels = c("NA",levels))

png(
  here("graphs/gender_cluster_xgb_map.png"),
  width     = 5,
  height    = 3.25,
  units     = "in",
  res       = 1200,
  pointsize = 4
)
print(ggplot() +
  geom_polygon(data = world.map.joined, aes(x = long, y = lat, group = group, fill = `leaf node`), colour = "gray", size = 0.1) +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank()
  ) +
  scale_fill_brewer(palette = "YlOrBr"))
dev.off()
gender <- final

country_summary <- as.data.frame(matrix(data = unique(c(gender$country)), ncol = 1))
colnames(country_summary) <- "country"

colnames(gender) <- paste0("gender_", colnames(gender))
if(any(colnames(gender) == "gender_lnode_3")){
  country_summary <- left_join(country_summary, gender[,c("gender_country", "gender_ordered_lnode_1", "gender_ordered_lnode_2", "gender_ordered_lnode_3")], by = c("country" = "gender_country"))
} else if(any(colnames(gender) == "gender_lnode_2")){
  country_summary <- left_join(country_summary, gender[,c("gender_country", "gender_ordered_lnode_1", "gender_ordered_lnode_2")], by = c("country" = "gender_country"))
} else {
  country_summary <- left_join(country_summary, gender[,c("gender_country", "gender_ordered_lnode_1")], by =   c("country" = "gender_country"))
}

# I also want the mean misogyny by country
load(here("intermediate_data/pca_analysis.Rdata"))
temp <- wvs %>% 
  select(wvs.country_code, wvs.gender7) %>% # 7 JB.
  group_by(wvs.country_code) %>% 
  summarise(mean(wvs.gender7))

colnames(temp) <- c("country", "mean_misogyny")
country_summary <- left_join(country_summary, temp, by = "country")

# Converting to something readible
write.csv(country_summary,here("intermediate_data/country_export.csv"), row.names = FALSE) # for lasso.
country_summary$country_name = countrycode(country_summary$country, origin = 'iso3c', destination = 'country.name')
country_summary$country_name[country_summary$country == "ROM"] <- "Romania"
country_summary$country <- country_summary$country_name
nodes = c(unique(country_summary$gender_ordered_lnode_1),unique(country_summary$gender_ordered_lnode_2))
nodes = sort(unique(nodes))
all_nodes = list()

long = 0
for(n in nodes){
  nodes_1 = country_summary$country[country_summary$gender_ordered_lnode_1 == nodes[n]]
  nodes_2 = country_summary$country[country_summary$gender_ordered_lnode_2 == nodes[n]]
  nodes_3 = country_summary$country[country_summary$gender_ordered_lnode_3 == nodes[n]]
  
  all_nodes[[n]] = setdiff(unique(c(nodes_1,nodes_2, nodes_3)), NA)
  
  if(length(all_nodes[[n]] > long)) (long = length(all_nodes[[n]]))
  all_nodes[[n]] <- sort(all_nodes[[n]])
}

table <- data.frame(lapply(all_nodes, function(x) {
  x <- unlist(x)
  length(x) <- max(lengths(all_nodes))
  return(x)
}))

cnames <- list()
for(l in 1:length(all_nodes))(cnames[l] = paste0("Node ", l))
colnames(table) <- cnames

# save it as a latex table.
print(xtable(table, type = "latex"), file = here("intermediate_data/country_table.tex"))





# Map of the most important deep determinants. Note that the countries aren't the same fed into XGBoost.
suppressWarnings({
  for(i in 1:4){ # this is so ugly, unfortunately it needs to be done like this.
  
  world.map <- map_data("world") # Get the country border cordinates. 
  ISO <- iso3166  # This is the ISO values from the maps package. "region" in world.map is the same as "sovereignty" in the iso3166 dataframe
  
  # ADD ISO3 Values.
  world.map <- left_join(world.map, ISO, by = c('region' = 'mapname'))
  world.map$ison <- countrycode(world.map$a3, "iso3c", "iso3n")
  
  # there's an issue with the china naming and with the UK:
  world.map[which(world.map$region == "UK"), "ison"] <- 826
  world.map[which(world.map$region == "China"), "ison"] <- 156
  
  # The most important deep determinants
  load(here('intermediate_data/mean_shap_vals.RData'))
  imp.dds <- unique(shap_long$variable)  %>% as.vector()
  imp.dds <- imp.dds[-grep("wvs.", imp.dds)][0:4]
  
  load(here("intermediate_data/deep.dets.RData"))
  deep.dets <- deep.dets[,c("c_code_n", imp.dds[i])]
  for_merge <- na.omit(deep.dets)
  for_merge$country <- as.numeric(as.character(for_merge$c_code_n))
  
  # What countries don't have the same coding across datasets?
  sort(setdiff(for_merge$c_code_n, world.map$ison)) # 246, 578
  g <- filter(deep.dets, c_code_n == c(246, 578)) # Finland & Norway
  
  world.map$a3 <- ifelse(world.map$region == "China", "CHN", world.map$a3)
  world.map$a3 <- ifelse(world.map$region == "Finland", "FIN", world.map$a3)
  world.map$a3 <- ifelse(world.map$region == "Norway", "NOR", world.map$a3)
  world.map$ison <- countrycode(world.map$a3, "iso3c", "iso3n")
  world.map[which(world.map$a3 == "ROM"), "ison"] <- 642  # Problem with Romania
  world.map[which(world.map$region == "UK"), "ison"] <- 826
  
  world.map.joined <- left_join(world.map, for_merge , by = c('ison' = 'country'))
  
  if(i == 1){
    world.map.joined <- rename(world.map.joined, Temperate = Har_kgatemp_aa)
    print(ggplot() +
            geom_polygon(data = world.map.joined, aes(x = long, y = lat, group = group, fill = Temperate), colour = "gray", size = 0.1) +
            labs(fill = "Share") + # Comment this out for title to be 'Temperate'
            theme(
              axis.text.x=element_blank(), #remove x axis labels
              axis.ticks.x=element_blank(), #remove x axis ticks
              axis.text.y=element_blank(),  #remove y axis labels
              axis.ticks.y=element_blank(),  #remove y axis ticks
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              rect = element_blank()
            ) +
            #  scale_fill_gradientn(colours = c("darkred", "orange", "#FFCC00", "#FFFF99", "#FFFFCC"), na.value = "white")
            scale_fill_gradientn(colours = c("darkred", "#fd8d3c", "#fecc5c", "#ffffb2" ), 
                                 na.value = "white", 
                                 limits = c(0, 1),
                                 breaks= seq(0, 1, by = 0.25)
            )
    )
    ggsave(here(paste0("graphs/imp_var_",i,".png")), device = "png")
  }
  
  if(i == 2){
    world.map.joined <- rename(world.map.joined, `Folklore - Challenge` = Fl_challenge_competition_aa)
    print(ggplot() +
            geom_polygon(data = world.map.joined, aes(x = long, y = lat, group = group, fill = `Folklore - Challenge`), colour = "gray", size = 0.1) +
            labs(fill = "Share") +
            theme(
              axis.text.x=element_blank(), #remove x axis labels
              axis.ticks.x=element_blank(), #remove x axis ticks
              axis.text.y=element_blank(),  #remove y axis labels
              axis.ticks.y=element_blank(),  #remove y axis ticks
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              rect = element_blank()
            ) +
            scale_fill_gradientn(colours = c("darkred", "#fd8d3c", "#fecc5c", "#ffffb2" ), 
                                 na.value = "white", 
                                 limits = c(0, 0.1),
                                 breaks= seq(0, 0.1, by = 0.025)
            )
    ) 
    ggsave(here(paste0("graphs/imp_var_",i,".png")), device = "png")
  }
  
  if(i == 3){
    world.map.joined <- rename(world.map.joined, `Temperate - no dry season` = Har_cultccf_aa)
    print(ggplot() +
            geom_polygon(data = world.map.joined, aes(x = long, y = lat, group = group, fill = `Temperate - no dry season`), colour = "gray", size = 0.1) +
            labs(fill = "Share") +
            theme(
              axis.text.x=element_blank(), #remove x axis labels
              axis.ticks.x=element_blank(), #remove x axis ticks
              axis.text.y=element_blank(),  #remove y axis labels
              axis.ticks.y=element_blank(),  #remove y axis ticks
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              rect = element_blank()
            ) +
            scale_fill_gradientn(colours = c("darkred", "#fd8d3c", "#fecc5c", "#ffffb2" ), 
                                 na.value = "white", 
                                 limits = c(0, 1),
                                 breaks= seq(0, 1, by = 0.25)
            )
    )
    ggsave(here(paste0("graphs/imp_var_",i,".png")), device = "png")
  }
  
  if(i == 4){
    world.map.joined <- rename(world.map.joined, `Bride Price` = marriage_2_ea)
    print(ggplot() +
            geom_polygon(data = world.map.joined, aes(x = long, y = lat, group = group, fill = `Bride Price`), colour = "gray", size = 0.1) +
            labs(fill = "Share") +
            theme(
              axis.text.x=element_blank(), #remove x axis labels
              axis.ticks.x=element_blank(), #remove x axis ticks
              axis.text.y=element_blank(),  #remove y axis labels
              axis.ticks.y=element_blank(),  #remove y axis ticks
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              rect = element_blank()
            ) +
            scale_fill_gradientn(colours = c("darkred", "#fd8d3c", "#fecc5c", "#ffffb2" ), 
                                 na.value = "white", 
                                 limits = c(0, 1),
                                 breaks= seq(0, 1, by = 0.25)
            )
    )
    ggsave(here(paste0("graphs/imp_var_",i,".png")), device = "png")
  }
}
})

rm(list = ls())