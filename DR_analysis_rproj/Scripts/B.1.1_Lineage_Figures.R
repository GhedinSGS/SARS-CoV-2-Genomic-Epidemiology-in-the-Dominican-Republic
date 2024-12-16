#### -----
# GOAL: Make trees of the B.1.1 clades
# 
# Author: Allie Kreitman
# Script made: 2023-03-29
# Script last updated: 2023-02-29
### -----

# load libraries
#load libraries
library(tidyverse)
library(cowplot)
library(treeio)
library(ggtree)
library(ape)

# load data from previous script
source("Scripts/Global_Phylogenetic_Tree_Data_Formatting.R")

# ---------- TREE FIGURE SETUP -------------------------------

# set region colors
region_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#4da890", "#F0E442", "#0072B2", "#D55E00", "#d61e6e")
names(region_colors)<-c("Asia", "North America","South America","Europe", "Africa", "Oceania","Dominican Republic", "Caribbean")

options(ignore.negative.edge=TRUE)

# tree branch colors
tree_branch_colors <- "#2b2b2b"

# ----------------  B.1.1 SUBTREES --------------
node_1 <- getMRCA(tree_renamed,c("DominicanRepublic/20201126/DR0184", "DominicanRepublic/20201116/DR0131"))
node_2 <- getMRCA(tree_renamed,c("DominicanRepublic/20210129/DR0266", "DominicanRepublic/20201127/DR0193"))
node_3 <- getMRCA(tree_renamed,c("DominicanRepublic/20200914/DR0071", "DominicanRepublic/20201028/DR0110"))
node_4 <- getMRCA(tree_renamed,c("DominicanRepublic/20201117/DR0141", "DominicanRepublic/20201006/DR0084"))

# set up 4 subtrees for each B.1.1 cluster
B.1.1_subtree_1 <- tree_subset(tree_renamed, node = node_1, levels_back = 1) %>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=GL_yn), size=3)+
  scale_color_manual(values = region_colors)+
  scale_shape_manual(values=c(16, 17))+
  geom_tiplab(size=4, hjust=-0.1)+
  #geom_text(aes(label=node), size=2)+
  hexpand(0.9)
B.1.1_subtree_1 <- B.1.1_subtree_1 %>% 
  collapse(83) %>% 
  collapse(70) %>% 
  collapse(93) %>% 
  collapse(74) %>% 
  collapse(81)


B.1.1_subtree_2 <- tree_subset(tree_renamed, node = node_2, levels_back = 1) %>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=GL_yn), size=3)+
  scale_color_manual(values = region_colors)+
  scale_shape_manual(values=c(16, 17))+
  geom_tiplab(size=4, hjust=-0.1)+
  geom_text(aes(label=node), size=2)+
  hexpand(0.9)
B.1.1_subtree_2 <- B.1.1_subtree_2 %>% 
  collapse(175) %>% 
  collapse(149) %>% 
  collapse(174)+
  geom_point2(aes(subset=(node == 149)), shape = 18, size = 3, color = region_colors["Africa"])+
  geom_cladelab(node=149, label="Africa", fontsize=4)
  # mark heterogeneous cluster at 149


B.1.1_subtree_3 <- tree_subset(tree_renamed, node = node_3, levels_back = 0) %>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=GL_yn), size=3)+
  scale_color_manual(values = region_colors)+
  scale_shape_manual(values=c(16, 17))+
  geom_tiplab(size=4, hjust=-0.1)+
  #geom_text(aes(label=node), size=2)+
  hexpand(0.9)
B.1.1_subtree_3 <- B.1.1_subtree_3 %>% 
  collapse(3570) %>% 
  collapse(3267) %>% 
  collapse(3113) %>% 
  collapse(2678) %>% 
  collapse(2428) %>% 
  collapse(2333) %>% 
  collapse(2335) %>% 
  collapse(2339) %>% 
  collapse(2343) %>% 
  collapse(2348) %>% 
  collapse(2353) %>% 
  collapse(2358) %>% 
  collapse(2364) %>% 
  collapse(2371) %>% 
  collapse(2381) %>% 
  collapse(2477) %>% 
  collapse(2533) %>% 
  collapse(2597) %>% 
  collapse(2789) %>% 
  collapse(2989) %>% 
  collapse(2331) %>% 
  collapse(2329)

B.1.1_subtree_4 <- tree_subset(tree_renamed, node = node_4, levels_back = 3) %>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=GL_yn), size=3)+
  scale_color_manual(values = region_colors)+
  scale_shape_manual(values=c(16, 17))+
  geom_tiplab(size=4, hjust=-0.1)+
  #geom_text(aes(label=node), size=2)+
  hexpand(0.9)


legend <- get_legend(ggtree(tree_renamed, color = tree_branch_colors, size = 0.1) %<+% 
                       metadata+
                       geom_tippoint(aes(color=region, shape = GL_yn), size=4)+
                       scale_color_manual(values = region_colors)
                       )

plot_layout <- plot_grid(B.1.1_subtree_1 + theme(legend.position="none"), 
                         B.1.1_subtree_2 + theme(legend.position = "none"), 
                         B.1.1_subtree_3 + theme(legend.position = "none"), 
                         B.1.1_subtree_4 + theme(legend.position = "none"),
                         nrow = 2, ncol = 2, 
                         labels = c("A", "B", "C", "D"))
plot_layout <- plot_grid(plot_layout, 
                         legend, 
                         ncol = 2, 
                         rel_widths = c(8,1))
