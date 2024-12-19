# The function of this script is to format the A lineage tree output and tree for figures

#load libraries
library(tidyverse)
library(ggtree)
library(treeio)
library(cowplot)

#load metadata
metadata_raw <- read_tsv("Data_Reanalysis/ncov_results/A_lineage/metadata_with_nextclade_qc.tsv")

#load nwk from nextstrain
tree_raw <- treeio::read.newick("Data_Reanalysis/ncov_results/A_lineage/tree.nwk")

#metadata formatting
metadata <- metadata_raw %>% 
  #make samples for this study have "SGS" as their gisaid_epi_isl
  mutate(gisaid_epi_isl = ifelse(is.na(gisaid_epi_isl)==TRUE & grepl("DR*", strain)==TRUE, "SGS", gisaid_epi_isl)) %>% 
  #rename strains for plotting on the tree
  mutate(strain_renamed = paste0(gsub(" ", "", country), sep="/", gisaid_epi_isl, sep="/", gsub("-", "", date)), .after=strain) %>% 
  mutate(DR_yn = ifelse(country == "Dominican Republic", "y", "n")) %>% 
  mutate(DR_yn_numeric = ifelse(DR_yn == "y", 1, 0)) %>% 
  mutate(GL_yn = ifelse(grepl("^DR[0-9]{4}$", strain) == T, "y", "n")) %>% 
  mutate(GL_yn_numeric = ifelse(GL_yn == "y", 1, 0)) %>% 
  select(strain, strain_renamed, date, region, country, division, gisaid_epi_isl, Nextstrain_clade, pango_lineage, DR_yn, DR_yn_numeric, GL_yn ,GL_yn_numeric)

#rename tree
tree <- rename_taxa(tree = tree_raw, data = metadata, key = strain, value = strain_renamed)

#remove original strain name from metadata
metadata <- metadata %>% 
  select(-strain) %>% 
  rename("strain" = "strain_renamed")

#clean up environment
rm(metadata_raw, tree_raw)

