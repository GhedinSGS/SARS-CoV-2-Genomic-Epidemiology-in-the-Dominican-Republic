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
tree <- read.newick("Data/Global_Phylogenetic_Tree.nwkk")
# metadata
metadata_raw <- read_tsv("Data/Global_Phylogenetic_Tree_metadata_with_nextclade_qc.tsv")

#samples in tree
samples_in_tree <- tree$tip.label

#format metadata
metadata <- metadata_raw %>% 
  # remove samples filtered out
  filter(strain %in% samples_in_tree) %>% 
  #select relevant strains
  select(strain, date, region, country, gisaid_epi_isl, Nextstrain_clade, Nextclade_pango) %>% 
  #change samples for this study gisaid_epi_isl number to say "SGS"
  mutate(gisaid_epi_isl = ifelse(is.na(gisaid_epi_isl)==TRUE, strain, gisaid_epi_isl)) %>% 
  # add strain rename column
  mutate(strain_renamed = paste0(gsub(" ", "", country), "/", gsub("-", "", date), "/", gsub("_", "", gisaid_epi_isl)))

# rename tree
tree_renamed <- rename_taxa(tree = tree, data = metadata, key = strain, value = strain_renamed)

#drop 3 samples that were repeated under different names
samples_to_drop <- c("DominicanRepublic/20201116/EPIISL9670835", "DominicanRepublic/20201116/EPIISL9670837", "DominicanRepublic/20210215/EPIISL9671868")
tree_renamed <- drop.tip(tree_renamed, samples_to_drop)

# remove old strain name from metadata
metadata <- metadata %>% 
  #remove old strain name
  select(-strain) %>% 
  # renamed new strain to just be called strain
  rename(strain = strain_renamed) %>% 
  # remove 3 repeat samples
  filter(!strain %in% samples_to_drop)

metadata <- metadata %>% 
  mutate(GL_yn = ifelse(grepl("EPI_ISL_", gisaid_epi_isl)==FALSE, "y", "n"))

#fully align names - not sure why this is necessary but it is buggy without! 
metadata <- left_join(tree_labels, metadata, by="strain")

#clean workspace
rm(tree, metadata_raw, samples_in_tree)

