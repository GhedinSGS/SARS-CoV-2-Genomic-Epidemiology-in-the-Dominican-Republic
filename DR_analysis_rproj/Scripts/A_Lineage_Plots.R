#load libraries
library(tidyverse)
library(ggtree)
library(treeio)
library(cowplot)
library(phytools)
library(tidytree)

# load data
source("Scripts/A_Lineage_Tree_Data_Wrangling.R")

# set colors
region_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#4da890", "#F0E442", "#0072B2", "#D55E00", "#d61e6e")
names(region_colors)<-c("Asia", "North America","South America","Europe", "Africa", "Oceania","Dominican Republic", "Caribbean")

tree_branch_color <- "#5c5c5c"

# size of text labels
text_label_size <- 2.75

# get node numbers for study samples
A2_node_number <- match("DominicanRepublic/SGS/20210216", tree$tip.label) #A2 lineage sample
A2.5_node_number <- match("DominicanRepublic/SGS/20210215", tree$tip.label) # A2.5 lineage sample

options(ignore.negative.edge=TRUE)
text_offset = 0.00004
# plot of full tree
tree_figure <- ggtree(tree, color = tree_branch_color, size = 0.1) %<+%
  metadata+
  geom_tippoint(aes(fill = region, alpha = GL_yn, stroke = GL_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  #highlight clade 1 with Dominican Republic Sample
  geom_hilight(node = A2.5_node_number, fill="yellow", alpha = 0.25)+
  geom_cladelab(node = A2.5_node_number, label = "C", offset.text = text_offset)+
  #Highlight clade 2 with Dominican Republic Sample
  geom_hilight(node = A2_node_number, fill="yellow", alpha = 0.25)+
  geom_cladelab(node = A2_node_number, label = "B", offset.text = text_offset)+
  # add legend
  theme(legend.position = "top")

# tree subset #1 (C)
A2.5_subtree <- tree_subset(tree, node = getParent(tree, A2.5_node_number), levels_back=3)
subtree_1 <- A2.5_subtree %>% 
  ggtree(color = "grey", size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(fill = region, alpha = GL_yn, stroke = GL_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  hexpand(.3)
  # geom_text(aes(label=node), size = 3) # use this line to get node labels

subtree_1_Collapsed <- subtree_1 %>% 
  # nodes to collapse
  ggtree::collapse(node = 166) %>% 
  ggtree::collapse(node = 148) %>% 
  ggtree::collapse(node = 131) %>% 
  ggtree::collapse(node = 187) %>% 
  ggtree::collapse(node = 122) %>% 
  ggtree::collapse(node = 198) +
  # reformat labels
  geom_point2(aes(subset=(node==166)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 166, label=paste0("USA (", length(getDescendants(A2.5_subtree, 166)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==148)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 148, label=paste0("USA (", length(getDescendants(A2.5_subtree, 166)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==131)), shape=16, size=2, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 131, label=paste0("Panama (", length(getDescendants(A2.5_subtree, 131)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==187)), shape=16, size=2, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 187, label=paste0("Panama (", length(getDescendants(A2.5_subtree, 187)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==122)), shape=16, size=2, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 122, label=paste0("Panama (", length(getDescendants(A2.5_subtree, 122)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==198)), shape=16, size=2, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 198, label=paste0("Panama (", length(getDescendants(A2.5_subtree, 198)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)

# tree subset #2 (B)
#A.2 sample
A2_subtree <- tree_subset(tree, node = getParent(tree, A2_node_number), levels_back=5)
subtree_2 <- A2_subtree %>% 
  ggtree(color = "grey", size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(fill = region, alpha = GL_yn, stroke = GL_yn_numeric), shape = 21, size=3)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = region_colors)+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  hexpand(.3)
  # geom_text(aes(label=node), size = 3) # use this line to get node labels

subtree_2_Collapsed <- subtree_2 %>%
  ggtree::collapse(node = 63) %>% 
  ggtree::collapse(node = 59) +
  geom_point2(aes(subset=(node==63)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 63, label=paste0("USA (", length(getDescendants(A2_subtree, 63)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==59)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 59, label=paste0("USA (", length(getDescendants(A2_subtree, 59)), ")", sep=""), fontsize=text_label_size, offset = 0.000006)

legend = cowplot::get_plot_component(tree_figure, 'guide-box-top', return_all = TRUE)

subtree_layout <- cowplot::plot_grid(subtree_2_Collapsed+theme(legend.position="none"), 
                                     subtree_1_Collapsed + theme(legend.position="none"), 
                                     ncol = 1, labels = c("B", "C"), rel_heights = c(1, 1.75))
trees_layout <- cowplot::plot_grid(tree_figure + theme(legend.position = "none"), 
                                   subtree_layout, 
                                   nrow = 1, rel_widths = c(1,2.5), 
                                   labels = c("A", ""))
final_layout <- cowplot::plot_grid(trees_layout, legend, nrow = 2, rel_heights = c(10, 1))
ggsave(filename = "Figures/A_lineage.svg", final_layout, height = 8.5, width = 13, units = "in")

