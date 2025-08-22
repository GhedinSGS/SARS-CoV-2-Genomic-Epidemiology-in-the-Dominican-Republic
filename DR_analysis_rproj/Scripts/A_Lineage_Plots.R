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

collapsed_clade_point_size <- 3
options(ignore.negative.edge=TRUE)
text_offset = 0.00001
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
  # geom_text(aes(label=node), size = 3) # use this line to get node labels

# larger clade holding both A samples from this study
sample_clade <- tree_subset(tree, node = getParent(tree, A2.5_node_number), levels_back = 10)
tree_figure_clade <- sample_clade %>% 
  ggtree(color = tree_branch_color, size = 0.1) %<+%
    metadata+
    geom_tippoint(aes(fill = region, alpha = GL_yn, stroke = GL_yn_numeric), shape = 21, size=3)+
    scale_alpha_discrete(range = c(0.5, 0.95))+
    scale_fill_manual(values = region_colors)+
    #highlight clade 1 with Dominican Republic Sample
    #geom_hilight(node = A2.5_node_number, fill="yellow", alpha = 0.25)+
    geom_cladelab(node = A2.5_node_number, label = "C", offset.text = text_offset)+
    #Highlight clade 2 with Dominican Republic Sample
    #geom_hilight(node = A2_node_number, fill="yellow", alpha = 0.25)+
    geom_cladelab(node = A2_node_number, label = "B", offset.text = text_offset)+
    # add legend
    theme(legend.position = "top")
    # geom_text(aes(label=node), size = 3) # use this line to get node labels
tree_figure_clade

label_offset = 0.000003
tree_figure_clade_collapsed <- tree_figure_clade %>%
  ggtree::collapse(node = 1543) %>% 
  ggtree::collapse(node = 962) %>% 
  ggtree::collapse(node = 953) %>% 
  ggtree::collapse(node = 1500) %>% 
  ggtree::collapse(node = 1402) %>% 
  ggtree::collapse(node = 929) %>% 
  ggtree::collapse(node = 1518) %>% 
  ggtree::collapse(node = 1544) %>% 
  ggtree::collapse(node = 1320) %>% 
  ggtree::collapse(node = 1393) %>% 
  ggtree::collapse(node = 1061) %>% 
  ggtree::collapse(node = 941) %>% 
  ggtree::collapse(node = 1308) %>%
  ggtree::collapse(node = 1665) +
  # reformat labels
  geom_point2(aes(subset=(node==1543)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 1543, label=paste0("(", length(getDescendants(sample_clade, 1543)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1544)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 1544, label=paste0("(", length(getDescendants(sample_clade, 1544)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==962)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 962, label=paste0("(", length(getDescendants(sample_clade, 962)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==953)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 953, label=paste0("(", length(getDescendants(sample_clade, 953)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1665)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 1665, label=paste0("(", length(getDescendants(sample_clade, 1665)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1402)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Europe"]])+
  geom_cladelab(node = 1402, label=paste0("(", length(getDescendants(sample_clade, 1402)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1518)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Europe"]])+
  geom_cladelab(node = 1518, label=paste0("(", length(getDescendants(sample_clade, 1518)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1500)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Europe"]])+
  geom_cladelab(node = 1500, label=paste0("(", length(getDescendants(sample_clade, 1500)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==929)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 929, label=paste0("(", length(getDescendants(sample_clade, 929)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1320)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 1320, label=paste0("(", length(getDescendants(sample_clade, 1320)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1393)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 1393, label=paste0("(", length(getDescendants(sample_clade, 1393)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1061)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["Caribbean"]])+
  geom_cladelab(node = 1061, label=paste0("(", length(getDescendants(sample_clade, 1061)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==941)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 941, label=paste0("(", length(getDescendants(sample_clade, 941)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==1308)), shape=17, size=collapsed_clade_point_size, alpha = 0.5, color=region_colors[["North America"]])+
  geom_cladelab(node = 1308, label=paste0("(", length(getDescendants(sample_clade, 1308)), ")", sep=""), fontsize=text_label_size, offset = label_offset)
  # geom_text(aes(label=node), size = 3) # use this line to get node labels
tree_figure_clade_collapsed
# collapse clades

##############################################################################################
### updated version with full sized triangles ###
##############################################################################################

tree_figure_collapsed_triangles <- tree_figure_clade %>%
  ggtree::collapse(node = 1543, 'max', fill=region_colors[["North America"]], alpha=0.5) %>% 
  ggtree::collapse(node = 962, 'max', fill=region_colors[["Caribbean"]], alpha=0.5) %>% 
  ggtree::collapse(node = 953, 'max', fill=region_colors[["Caribbean"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1500, 'max', fill=region_colors[["Europe"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1402, 'max', fill=region_colors[["Europe"]], alpha=0.5) %>% 
  ggtree::collapse(node = 929, 'max', fill=region_colors[["Caribbean"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1518, 'max', fill=region_colors[["Europe"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1544, 'max', fill=region_colors[["North America"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1320, 'max', fill=region_colors[["Caribbean"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1393, 'max', fill=region_colors[["Caribbean"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1061, 'max', fill=region_colors[["Caribbean"]], alpha=0.5) %>% 
  ggtree::collapse(node = 941, 'max', fill=region_colors[["North America"]], alpha=0.5) %>% 
  ggtree::collapse(node = 1308, 'max', fill=region_colors[["North America"]], alpha=0.5) %>%
  ggtree::collapse(node = 1665, 'max', fill=region_colors[["North America"]], alpha=0.5)
# geom_text(aes(label=node), size = 3) # use this line to get node labels
tree_figure_collapsed_triangles



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
trees_layout <- cowplot::plot_grid(tree_figure_collapsed_triangles + theme(legend.position = "none"), 
                                   subtree_layout, 
                                   nrow = 1, rel_widths = c(1,2.5), 
                                   labels = c("A", ""))

final_layout <- cowplot::plot_grid(trees_layout, legend, nrow = 2, rel_heights = c(10, 1))
final_layout
ggsave(filename = "Figures/A_lineage_v4.svg", final_layout, height = 8.5, width = 13, units = "in")

# try to make a piechart version of the tree: 
# 
# library(ggtree)
# library(reshape2)
# library(ggplot2)
# 
# tr <- tree
# p <- ggtree(tree, color = tree_branch_color, size = 0.1) %<+%
#   metadata +
#   geom_tippoint(aes(fill = region), shape = 21, size = 3) +
#   scale_fill_manual(values = region_colors)
# 
# # Nodes to collapse
# collapse_nodes <- c(4170, 3523, 2690, 2679)
# 
# # Collapse those nodes
# for (n in collapse_nodes) {
#   p <- collapse(p, node = n)
# }
# 
# ## create list to hold all pie charts
# pies = list()
# for (i in 1:collapse_nodes) {
#   curr_dat = metadata %>% 
#     filter(strain %in% getDescendants(tree, collapse_nodes[i]))
#   ## create a ggplot object for each pie chart
#   pies[[i]] =  ggplot(curr_dat, aes(y = value, fill = variable, x="")) + 
#     geom_bar(stat = "identity") +
#     coord_polar("y", start=0) +
#     theme_void() + scale_fill_brewer(palette = "Set1", guide = F)
# }
# # give them the appropriate names and plot on tree
# names(pies) = 1:15
# inset(p, pies, width=0.1, height=0.1, hjust=0)
# ##########


