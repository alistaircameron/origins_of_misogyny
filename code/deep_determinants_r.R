                          # # # Folklore # # # 
# Michalopoulos, Stelios; Xue, Melanie Meng, 2021, "Replication Data for: 'Folklore'", https://doi.org/10.7910/DVN/IXOHKB, Harvard Dataverse, V2
# Replication files: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IXOHKB 
# This is ethnicity based data; that is, the folklore at the ethnicity level. So, the ancestry adjustment is done at the ethnicity-level, rather than the country-level migration flows. 

countries_ready <- read_dta(here("raw_data/folklore.dta"))
a <- as.data.frame(colnames(countries_ready))
vars_to_use <- c("cntry", colnames(countries_ready)[9067:ncol(countries_ready)], "nmbr_language", colnames(countries_ready)[which(colnames(countries_ready) == "malebias"):which(colnames(countries_ready) == "challenge_competition")], colnames(countries_ready)[grep("equal", colnames(countries_ready)) ])
folklore <- select(countries_ready, all_of(vars_to_use))

# for those that aren't %, turn into %
folklore[,c(colnames(countries_ready)[9067:ncol(countries_ready)], colnames(countries_ready)[grep("equal", colnames(countries_ready)) ])] <- folklore[,c(colnames(countries_ready)[9067:ncol(countries_ready)], colnames(countries_ready)[grep("equal", colnames(countries_ready)) ])]/countries_ready$motifs_total

names <- colnames(folklore)
for(i in 2:length(names)) (names[i] <- paste0("Fl_", names[i], "_aa"))
colnames(folklore) <- names
folklore$ison <- countrycode(folklore$cntry, "iso3c", "iso3n")
folklore <- select(folklore, -"cntry")


      # # # Authors: Oana Borcan, Ola Olsson & Louis Putterman # # # 
# Title: Transition to agriculture and first state presence: A global analysis
# Explorations in Economic History, 2021
# https://www.sciencedirect.com/science/article/pii/S001449832100022X
# Dataset name: BOP
df <- read_dta(here("raw_data/bop.dta"))
df$ison <- countrycode(df$iso3, origin = 'iso3c', destination = 'iso3n')
df <- select(df, c('ison', 'origtime2', 'age2000', 'firsts2', 's01n_mig1', 'agyears'))
names <- colnames(df)
for(i in 2:length(names)) (names[i] <- paste0("BOP_", names[i]))
colnames(df) <- names
df <- rename(df, "BOP_s01n_mig1_aa" = "BOP_s01n_mig1" )
BOP <- df

      # Gendered Languages Data #
# The Grammatical Origins of Gender Roles
# Victor Gay, Estefania Santacreu-Vasut and Amir Shoham
# BEHL Working Paper Seires 
# No. WP2013-03
# https://victorgay.netlify.app/10.1.1.393.1923.pdf

gay <- read_excel(here("raw_data/Gay 2013.xlsx"))
gay$ison <- countrycode(gay$Country, "country.name", "iso3c")
gay$ison <- ifelse(gay$Country == "Columbia", "COL", gay$ison) # fixing missing guys
gay$ison <- ifelse(gay$Country == "Kyrgystan", "KGZ", gay$ison) # fixing missing guys
gay$ison <- countrycode(gay$ison, "iso3c", "iso3n")
gay <- select(gay, -"Country")

colnames(gay) <- paste0("GAY_",colnames(gay)) # Identify where it's from.
colnames(gay)[ncol(gay)] <- "ison"

gay[gay=="n/a"] <- NA # Replace excel n/a with real NA in entire dataframe
gay <- rename(gay, "GAY_Speakers (%)" = "GAY_(%)")

for(var in setdiff(colnames(gay), "GAY_Language")){gay[[var]] <- as.numeric(gay[[var]])}

gay$GAY_GII <- rowSums(cbind(gay$GAY_NGII, gay$GAY_SBII, gay$GAY_GAII, gay$GAY_GPII), na.rm = T)
gay$GAY_GIIV1 <- rowSums(cbind(gay$GAY_NGII, gay$GAY_SBII, gay$GAY_GAII), na.rm = T)
gay$GAY_GIIV2 <- rowSums(cbind(gay$GAY_NGII, gay$GAY_SBII, gay$GAY_GPII), na.rm = T)

      # Giuliano & Nunn 2021 - Understanding Cultural Persistence and Change #
# Review of Economic Studies, 88 (5): 1541-1581. https://academic.oup.com/restud/article/88/4/1541/5956732#271187126
# Replication data: https://scholar.harvard.edu/nunn/pages/data-0
# They only offer ancestry-adjusted data.
df <- read_dta(here("raw_data/gn21_table1.dta"))
GN21 <- select(df, c('isocode', colnames(df)[5]))
colnames(GN21) <- c('ison', 'GN21_instability_aa')
GN21$ison <- countrycode(GN21$ison, "iso3c", "iso3n") # Unfortunately, YUG is there.



                  # # # Joining the data. # # #
other_dds <- full_join(BOP, folklore, by = "ison")
other_dds <- full_join(other_dds, gay, by = "ison")
other_dds <- full_join(other_dds, GN21, by = "ison")
other_dds <- other_dds[!is.na(other_dds$ison),] # Remove the rows where is.na(ison) == T
save(other_dds, file = here("intermediate_data/other_dds.RData"))
if(!exists("drop_everything")){
  rm(list = setdiff(ls(), 'other_dds'))
}