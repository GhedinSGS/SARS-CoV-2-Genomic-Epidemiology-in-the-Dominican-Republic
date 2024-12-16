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

options(ignore.negative.edge=TRUE)

# plot of full tree
tree_figure <- ggtree(tree, color = tree_branch_color, size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=DR_yn, alpha = DR_yn, size = DR_yn))+ #set metadata for color, shape, alpha, size
  scale_color_manual(values = region_colors)+ #change color to region colors
  scale_shape_manual(values=c(1, 17), labels = c("No", "Yes"))+ #set shape values
  scale_size_manual(values=c(2,3), guide = 'none')+ #set sizes
  scale_alpha_discrete(range = c(0.5, 0.95), guide = 'none')+ #set opacity values
  #highlight clade 1 with Dominican Republic Sample
  geom_hilight(node = 2782, fill="yellow", alpha = 0.25)+
  geom_cladelab(node = 2782, label = "C")+
  #Highlight clade 2 with Dominican Republic Sample
  geom_hilight(node = 2941, fill="yellow", alpha = 0.25)+
  geom_cladelab(node = 2941, label = "B")


# tree subset #1 (A)
#clade 2783
subtree_1 <- tree_subset(tree, node = 2782, levels_back=0) %>% 
  ggtree(color = "grey", size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=DR_yn, alpha = DR_yn, size = DR_yn))+
  scale_color_manual(values = region_colors)+
  scale_shape_manual(values=c(1, 17))+
  scale_size_manual(values=c(1,2))+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  hexpand(.3)

subtree_1_Collapsed <- subtree_1 %>% ggtree::collapse(node = 101) %>% 
  ggtree::collapse(node = 156) %>% 
  ggtree::collapse(node = 124) %>% 
  ggtree::collapse(node = 122) %>% 
  ggtree::collapse(node = 141) +
  geom_point2(aes(subset=(node==101)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 101, label="USA (23)", fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==156)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 156, label="USA (18)", fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==124)), shape=16, size=2, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 124, label="Panama (7)", fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==141)), shape=16, size=2, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 141, label="Panama (7)", fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==122)), shape=16, size=2, alpha = 0.5, color=region_colors[["Europe"]])+
  geom_cladelab(node = 122, label="Portugal (3)", fontsize=text_label_size, offset = 0.000006)

# tree subset #2 (B)
#clade 2941
subtree_2 <- tree_subset(tree, node = 2941, levels_back=1) %>% 
  ggtree(color = "grey", size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(color = region, shape=DR_yn, alpha = DR_yn, size = DR_yn))+
  scale_color_manual(values = region_colors)+
  scale_shape_manual(values=c(1, 17))+
  scale_size_manual(values=c(1,2))+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  geom_tiplab(size=text_label_size, hjust=-0.1)+
  hexpand(.3)

subtree_2_Collapsed <- subtree_2 %>%
  ggtree::collapse(node = 46) %>% 
  ggtree::collapse(node = 49)+ #16 samples
  geom_point2(aes(subset=(node==46)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 46, label="USA (4)", fontsize=text_label_size, offset = 0.000006)+
  geom_point2(aes(subset=(node==49)), shape=16, size=2, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 49, label="USA (16)", fontsize=text_label_size, offset = 0.000006)

legend <- get_legend(tree_figure + 
                       theme(legend.position = "bottom", legend.title = element_text(size = 10)) + 
                       labs(color = "Region", shape = "From Dominican Republic")
)

subtree_layout <- cowplot::plot_grid(subtree_2_Collapsed+theme(legend.position="none"), 
                                     subtree_1_Collapsed + theme(legend.position="none"), 
                                     ncol = 1, labels = c("B", "C"), rel_heights = c(1, 1.75))
trees_layout <- cowplot::plot_grid(tree_figure + theme(legend.position = "none"), 
                                   subtree_layout, 
                                   nrow = 1, rel_widths = c(1,3), 
                                   labels = c("A", ""))
final_layout <- cowplot::plot_grid(trees_layout, legend, nrow = 2, rel_heights = c(10, 1))
