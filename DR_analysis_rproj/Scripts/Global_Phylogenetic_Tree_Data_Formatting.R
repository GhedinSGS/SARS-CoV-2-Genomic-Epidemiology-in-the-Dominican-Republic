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
tree <- read.newick("Data_Reanalysis/ncov_results/global_tree/tree.nwk")
# metadata
metadata_raw <- read_tsv("Data_Reanalysis/ncov_results/global_tree/metadata_with_nextclade_qc.tsv")

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
  select(-strain) %>% 
  rename(strain = strain_renamed)

#clean workspace
rm(metadata_raw, samples_in_tree)

