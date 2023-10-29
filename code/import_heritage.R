# Read in data.
source(here("code/deep_determinants_r.R")) # or: load(here("intermediate_data/other_dds.RData"))
source(here("code/helper_functions.R"))
ivs <- load_ivs()
dd_df <- read_dta(here("intermediate_data/deep_determinants.dta"))
colnames(other_dds)[which(colnames(other_dds)=="ison")] <- "c_code_n"
dd_df <- left_join(dd_df, other_dds, by = "c_code_n")

# remove contemporay variables and duplicates (keep the one with less missing values)
if(sum(is.na(dd_df$AG13_wb_eca)) >= sum(is.na(dd_df$Enk_cont_eca_aa))){
  dd_df <- select(dd_df,-Enk_cont_eca_aa)
} else {
  dd_df <- select(dd_df,-AG13_wb_eca)
}

dd_df <- select(dd_df, -c("gdp_pc_19","gini_18", "AG13_oecd", "AG13_opec", "Har_disa25_aa", 'AG13_africa', 'AG13_europe', 'AG13_asia', 'AG13_oceania', 'AG13_americas', "AG13_western", "AG13_wb_ssa", "AG13_wb_mena", "AG13_wb_eca", "AG13_wb_sas", "AG13_wb_eap", "AG13_wb_nam", "AG13_wb_lac", "Enk_cont_ssa_aa", "Enk_cont_mena_aa", "Enk_cont_sas_aa", "Enk_cont_eap_aa", "Enk_cont_nam_aa", "Enk_cont_lac_aa", "GAY_Speakers (%)"))

# Turn to individual characteristics
set.seed(007)
gender_norms <- c("D059", "E233", "D060", "C001", "D078", "F120") # IVS
princ.comp <- principal(select(ivs,all_of(gender_norms)), nfactors = 1)
ivs$gender <- as.numeric(princ.comp$scores)

dds <- filter(dd_df, country_code %in% unique(ivs[!is.na(ivs$gender),]$country_code))
dds.spare <- dds # dirty method to save clean copy of country_code

# Round to two sig figs.
numerics <- unlist(lapply(dds, is.numeric)) # where are the numeric guys?
numerics[which(colnames(dds)==c("c_code_n", "country_code"))] <- FALSE # don't round the identifiers.
dds[ , numerics] <- signif(dds[ , numerics], 2)

dds$PD_language_family <- replace(dds$PD_language_family, dds$PD_language_family == "", NA) # is a string
dds$PD_language_family <-  as.factor(dds$PD_language_family) 
dds$GAY_Language <- as.factor(dds$GAY_Language)
dds <- as.data.frame(sapply(dds,as.numeric)) # making all data of type numeric
dds$country <- dds.spare$country
dds$c_code_n <- dds.spare$c_code_n
dds$country_code <- dds.spare$country_code

# some cases, na is indicative of 0.
replace_zero <- c("AG13_legor_uk", "AG13_legor_fr", "AG13_legor_so", "AG13_legor_ge", "AG13_legor_sc", "AG13_africa", "AG13_europe", "AG13_asia", "AG13_oceania", "AG13_americas", "AG13_oecd", "AG13_opec", "AG13_western", "AG13_wb_ssa", "AG13_wb_mena", "Har_arab_empire", "Har_mineuro", "Har_majeuro", "NP_colony_oeu", "NP_colony_prt", "NP_colony_fra", "NP_colony_gbr", "NP_colony_esp", "Enk_cont_lac_aa", "Enk_cont_nam_aa", "Enk_cont_eap_aa", "Enk_cont_sas_aa", "Enk_cont_eca_aa", "Enk_cont_mena_aa", "Enk_cont_ssa_aa","AG13_wb_eca", "AG13_wb_sas", "AG13_wb_eap", "AG13_wb_nam", "AG13_wb_lac", "Enk_colony_esp",  "Enk_colony_gbr",  "Enk_colony_fra" )
for(element in intersect(replace_zero, colnames(dds))){dds[,element][is.na(dds[,element])] = 0}

# Letting R know that categorical variables are factors and labeling them as such in human-readable form.
ordered_factors <- c()
for (fac in ordered_factors) {
  dds[[fac]] <- factor(dds[[fac]], ordered = T)
  colnames(dds)[colnames(dds) == paste0(fac)] <- paste0(fac,".f")
}
unordered_factors <- c("AG13_legor_uk", "AG13_legor_fr", "AG13_legor_so", "AG13_legor_ge", "AG13_legor_sc", "AG13_africa", "AG13_europe", "AG13_asia", "AG13_oceania", "AG13_americas", "AG13_oecd", "AG13_opec", "AG13_western", "AG13_wb_ssa", "AG13_wb_mena", "Har_arab_empire", "c_code_n", "Har_mineuro", "Har_majeuro", "PD_language_family", 'PD_drop', "NP_colony_oeu", "NP_colony_prt", "NP_colony_fra", "NP_colony_gbr", "NP_colony_esp", "Enk_cont_lac_aa", "Enk_cont_nam_aa", "Enk_cont_eap_aa", "Enk_cont_sas_aa", "Enk_cont_eca_aa", "Enk_cont_mena_aa", "Enk_cont_ssa_aa", 'AG13_climate', "AG13_wb_eca", "AG13_wb_sas", "AG13_wb_eap", "AG13_wb_nam", "AG13_wb_lac", 'Har_colony')
for (fac in intersect(unordered_factors, colnames(dds))) {
  dds[[fac]] <- factor(dds[[fac]], ordered = F)
  colnames(dds)[colnames(dds) == paste0(fac)] <- paste0(fac,".f")
}

# Order the data by number of missing observations
dd <- as.data.frame(colSums(is.na(dds))) %>% 
  arrange(colSums(is.na(dds)))

dd$count <- nrow(dds)
dd$pc.missing <- round(dd[[1]]/dd[[2]],2)

dd <- dd %>% as.data.frame()
colnames(dd) <- c("missing","count","percent")

#dd %>% count(missing) # count by group (ie. continent)
dd$numbers <- 1:nrow(dd)

# Allowing XGBoost to handle missing data internally: we don't want XGBoost to be making predictions from a feature set that has too many missing datapoints. How many is "too many"? That is subjective, we choose to keep all deep.dets with less than 10% of countries missing.
dd <- dd[dd$percent <= 0.10,]
deep.dets <- select(dds, c(rownames(dd)))
missing_data <- deep.dets %>%
  is.na() %>%
  colSums() %>% 
  as.data.frame  

missing_data$pc <- missing_data$. / nrow(deep.dets)
missing_data <- arrange(missing_data, -pc)
whichmissing <- as.data.frame(rowSums(is.na(deep.dets))) # Which countries have data missing., and how many obs.
rownames(whichmissing) <- deep.dets$country
whichmissing$pc <- whichmissing$`rowSums(is.na(deep.dets))`/(ncol(deep.dets)-3) # -3 because we've got 3 variables which just label countries. Unclear if i should account for each variable as 1 or as less than this, eg. marriage_1; marriage_2 ... marriage_10 etc all come from 'marriage'. 
whichmissing <- arrange(whichmissing,whichmissing$pc)
whichmissing$index <- 1:nrow(whichmissing)

to_keep <- rownames(whichmissing[whichmissing$pc <= 0.1,])
deep.dets <- filter(deep.dets, country %in% to_keep)

# Remove features with near-zero-variance.
deep.dets <- select(deep.dets, -nearZeroVar(deep.dets))
deep.dets <- select(deep.dets,-which(duplicated(t(deep.dets))))

if(!exists("drop_everything")){
  rm(list = setdiff(ls(), c("deep.dets")))
}