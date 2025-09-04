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
library(phytools)
library(tidytree)

# load data from previous script
source("Scripts/Global_Phylogenetic_Tree_Data_Formatting.R")

# ---------- TREE FIGURE SETUP -------------------------------

# set region colors
region_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#4da890", "#F0E442", "#0072B2", "#d61e6e")
names(region_colors)<-c("Asia", "North America","South America","Europe", "Africa", "Oceania", "Caribbean")

options(ignore.negative.edge=TRUE)

# tree branch colors
tree_branch_colors <- "#2b2b2b"
# size of text labels
text_label_size <- 2.75
# ----------------  B.1.1 SUBTREES --------------
node_1 <- match("DominicanRepublic_DR0184", tree$tip.label) #cluster of 5 DR samples
node_2 <- match("DominicanRepublic_DR0129", tree$tip.label) #cluster of 3 DR samples (OLD0085)
node_3 <- match("DominicanRepublic_DR0071", tree$tip.label) #cluster of 3 DR samples
node_4 <- match("DominicanRepublic_DR0131", tree$tip.label) #cluster of 4 DR samples (OLD 141)

##### SUBTREE 1 #####
# create a subtree
B.1.1_subtree_1 <- tree_subset(tree, node = node_1, levels_back = 5)
# plot the subtree
B.1.1_subtree_1_plot <- B.1.1_subtree_1%>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(fill = region, alpha = DR_yn, stroke = DR_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  # geom_text(aes(label=node), size=4)+
  hexpand(.3)

B.1.1_subtree_1_plot
##### SUBTREE 2 #####
# create a subtree
B.1.1_subtree_2 <- tree_subset(tree, node = node_2, levels_back = 1)
# plot the subtree
B.1.1_subtree_2_plot <- B.1.1_subtree_2%>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(fill = region, alpha = DR_yn, stroke = DR_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  # geom_text(aes(label=node), size=4)+
  hexpand(.3)
B.1.1_subtree_2_plot <- B.1.1_subtree_2_plot %>% 
  ggtree::collapse(node = 72, 'mixed', fill=region_colors[["Caribbean"]], alpha = .5) +
  geom_cladelab(node = 72, label = "Martinique (9)", align = TRUE, 
                offset = 0, barsize = 0, fontsize = 3)

B.1.1_subtree_2_plot
##### SUBTREE 3 #####
# create a subtree
B.1.1_subtree_3 <- tree_subset(tree, node = node_3, levels_back = 4)
# plot the subtree
B.1.1_subtree_3_plot <- B.1.1_subtree_3%>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(fill = region, alpha = DR_yn, stroke = DR_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  # geom_text(aes(label=node), size=4)+
  hexpand(.3)
B.1.1_subtree_3_plot
##### SUBTREE 4 ####
# create a subtree
B.1.1_subtree_4 <- tree_subset(tree, node = node_4, levels_back = 4)
# plot the subtree
B.1.1_subtree_4_plot <- B.1.1_subtree_4%>% 
  ggtree(color = tree_branch_colors, size = 0.3) %<+% 
  metadata+
  geom_tippoint(aes(fill = region, alpha = DR_yn, stroke = DR_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  # geom_text(aes(label=node), size=4)+
  theme(legend.position = "top")+
  hexpand(.3)
B.1.1_subtree_4_plot


# legend <- get_legend(ggtree(tree, color = tree_branch_colors, size = 0.1) %<+% 
#                        metadata+
#                        geom_tippoint(aes(color=region, shape = GL_yn), size=4)+
#                        scale_color_manual(values = region_colors)
#                        )
legend = cowplot::get_plot_component(B.1.1_subtree_4_plot, 'guide-box-top', return_all = TRUE)

plot_layout <- plot_grid(B.1.1_subtree_1_plot + theme(legend.position="none"), 
                         B.1.1_subtree_2_plot + theme(legend.position = "none"), 
                         B.1.1_subtree_4_plot + theme(legend.position = "none"), 
                         B.1.1_subtree_3_plot + theme(legend.position = "none"),
                         nrow = 2, ncol = 2, 
                         labels = c("A", "B", "C", "D"), 
                         rel_heights = c(1, 0.8))
plot_layout <- plot_grid(plot_layout, 
                         legend, 
                         nrow = 2, 
                         rel_heights = c(8,1))
plot_layout
ggsave("Figures/FIG3_B.1.1_layout_udpated_v1.svg", plot_layout, height = 8.5, width = 16, unit = "in")
