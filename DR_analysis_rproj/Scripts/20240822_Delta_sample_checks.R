library(tidyverse)
library(here)

setwd("/Users/alliekreitman/Documents/github_repos/SARS-CoV-2-Genomic-Epidemiology-in-the-Dominican-Republic")

global_data <- read_tsv("Data/Global_Phylogenetic_Tree_metadata_with_nextclade_qc.tsv")

early_delta_samples <- global_data %>% 
  filter(Nextstrain_clade == "21J (Delta)") %>% 
  arrange(date)

sample_list <- early_delta_samples %>% 
  select(strain) %>% 
  mutate(strain = paste0("hCoV-19/", strain))

write_tsv(sample_list, "Data/delta_sample_list.txt", col_names=F)
