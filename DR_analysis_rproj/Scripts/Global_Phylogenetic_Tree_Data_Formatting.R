#### -----
# GOAL: format the nextstrain output files for making figures with ggtrees
# 1. import tree and cleaned data
# 2. select relevant columns
# 3. rename samples
# 4. change strain names in tree file
# 
# Author: Allie Kreitman
# Script made: 2023-03-29
# Script last updated: 2023-02-29
### -----

# load libraries
library(tidyverse)
library(treeio)
library(ape)

# load data
# tree
tree <- treeio::read.newick("Data_Reanalysis/ncov_results/global_tree/tree.nwk")
# metadata
metadata_raw <- read_tsv("Data_Reanalysis/ncov_results/global_tree/metadata_with_nextclade_qc.tsv")
# metadata_subsampled <- read_tsv("Data_Reanalysis/ncov_results/global_tree/20241213_DR_GlobalBackgroundTree_subsampled_metadata.tsv")
# caribbean countries
caribbean_countries <- c(
  # Independent Nations
  "Antigua and Barbuda", "The Bahamas", "Barbados", "Cuba", "Dominica", 
  "Dominican Republic", "Grenada", "Haiti", "Jamaica", 
  "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
  "Trinidad and Tobago",
  
  # British Overseas Territories
  "Anguilla", "Bermuda", "British Virgin Islands", "Cayman Islands", 
  "Montserrat", "Turks and Caicos Islands",
  
  # French Overseas Territories
  "Guadeloupe", "Martinique", "Saint Barthélemy", "Saint Martin",
  
  # Dutch Territories
  "Aruba", "Curaçao", "Sint Maarten", "Bonaire", 
  "Saba", "Sint Eustatius","Curacao",
  
  # U.S. Territories
  "Puerto Rico", "U.S. Virgin Islands", 
  
  # central america
  "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama"
)

#samples in tree
samples_in_tree <- tree$tip.label

#format metadata
metadata <- metadata_raw %>% 
  # remove samples filtered out
  filter(strain %in% samples_in_tree) %>%
  # rename strain
  mutate(strain_renamed = ifelse(grepl("^DR[0-9]{4}$", strain) == T, paste0("DominicanRepublic_", strain), strain)) %>% 
  #select relevant strains
  select(strain, date, region, country, Nextstrain_clade, Nextclade_pango, strain_renamed) 

# rename tree
tree <- rename_taxa(tree = tree, data = metadata, key = strain, value = strain_renamed)

metadata <- metadata %>% 
  mutate(GL_yn = ifelse(grepl("^DR[0-9]{4}$", strain) == T, "y", "n")) %>% 
  # make DR_yn
  mutate(DR_yn = ifelse(country == "Dominican Republic", "y", "n")) %>% 
  mutate(DR_yn_numeric = ifelse(DR_yn == "y", 1, 0)) %>% 
  select(-strain) %>% 
  rename(strain = strain_renamed) %>% 
  # make dominican republic its own region
  mutate(region = ifelse(country %in% caribbean_countries, "Caribbean", region))

#fully align names - not sure why this is necessary but it is buggy without! 
metadata <- left_join(data.frame(strain = tree$tip.label), metadata, by="strain")

#clean workspace
rm(metadata_raw, samples_in_tree)

