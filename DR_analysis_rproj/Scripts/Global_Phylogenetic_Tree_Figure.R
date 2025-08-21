#### -----
# GOAL: Make a tree figure of the whole newick tree
# 
# Author: Allie Kreitman
# Script made: 2023-03-29
# Script last updated: 2024-12-17
### -----

#load libraries
library(tidyverse)
library(cowplot)
library(treeio)
library(ggtree)
library(phytools)
library(here)

setwd(here())
# load data from previous script
source("Scripts/Global_Phylogenetic_Tree_Data_Formatting.R")

# ---------- TREE FIGURE SETUP -------------------------------


# clade_colors <- c("#771155","#5c5c5c", "#AA4488", "#CC99BB", "#114477", 
#                   "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
#                   "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", 
#                   "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122")
# names(clade_colors) <- c("20F", "20A", "20I (Alpha, V1)", "20B", "20D", 
#                          "20G", "20C", "20E (EU1)", "21J (Delta)", "20H (Beta, V2)",
#                          "21D (Eta)", "21F (Iota)", "21C (Epsilon)", "20J (Gamma, V3)", "19B", 
#                          "21I (Delta)", "21B (Kappa)", "19A", "21G (Lambda)", "21A (Delta)")

clade_colors <- c("#0050dc", "#00b31d",  "#eed41f", "#ff7a00",  "#ea229d", 
                  "#5d5e5d",  "#eb2f16", "#45b0cf",  "#8413b6", "#9aa65a",  
                  "#d48bd3", "#e1a17b",  "#895437", "black", "#a9cfad", "#aff7fa", "#087175", "#351557", "#cc462f", "#ebe88f", "#bfbfbf") 

names(clade_colors) <- c("20A", "20I (Alpha, V1)", "20B", 
                         "20G", "20C", "20E (EU1)", "21J (Delta)",
                         "21F (Iota)", "21C (Epsilon)", "20J (Gamma, V3)", "19B", 
                         "19A", "21A (Delta)", 
                         "20F", "20D","21I (Delta)", "21D (Eta)", "20H (Beta, V2)", "21B (Kappa)", "21G (Lambda)")

options(ignore.negative.edge=TRUE)

# tree branch colors
tree_branch_colors <- "#2b2b2b"

collapsed_clade_point_size = 5
# size of text labels
text_label_size <- 4
label_offset = 0.000003

background_alpha = 0.7

metadata <- metadata %>% 
  mutate(GL_yn_numeric = ifelse(GL_yn == "y", 1, 0)) %>% 
  mutate(sample_in_tree = ifelse(strain %in% tree$tip.label, "y", "n"))
metadata$GL_yn_numeric <- as.numeric(metadata$GL_yn_numeric)

metadata <- metadata %>% 
  mutate(Nextstrain_clade = case_when(Nextstrain_clade == "20I" ~ "20I (Alpha, V1)",
                                      Nextstrain_clade == "20E" ~ "20E (EU1)",
                                      Nextstrain_clade == "21J" ~ "21J (Delta)",
                                      Nextstrain_clade == "21F" ~ "21F (Iota)",
                                      Nextstrain_clade == "21C" ~ "21C (Epsilon)",
                                      Nextstrain_clade == "20J" ~ "20J (Gamma, V3)",
                                      Nextstrain_clade == "21A" ~ "21A (Delta)",
                                      Nextstrain_clade == "21I" ~ "21I (Delta)",
                                      Nextstrain_clade == "21D" ~ "21D (Eta)",
                                      Nextstrain_clade == "20H" ~ "20H (Beta, V2)",
                                      Nextstrain_clade == "21B" ~ "21B (Kappa)",
                                      Nextstrain_clade == "21G" ~ "21G (Lambda)",
                                      .default = Nextstrain_clade))

# ------- Plot BARGRAPH OF LINEAGES ------------
bargraph_figure <- ggplot(data = metadata %>% 
         group_by(region, Nextstrain_clade) %>% 
         summarise(count = n()) %>% 
         ungroup(), 
       aes(x = region, y = count, fill = Nextstrain_clade))+
  theme_bw()+
  geom_col()+
  scale_fill_manual(values = clade_colors)+
  xlab("Region") +
  ylab("Number of Sequences")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1) )
bargraph_figure
# --------- PLOT TREE --------------------- 
 
tree_figure <- ggtree(tree, color = tree_branch_colors, size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(fill=Nextstrain_clade, alpha = GL_yn, stroke = GL_yn_numeric), shape=21, size=4)+
  scale_alpha_discrete(range = c(background_alpha, 0.95), name = "This Study")+
  scale_fill_manual(values = clade_colors)+
  guides(fill = guide_legend(nrow = 5), stroke = guide_legend(nrow = 2, byrow=TRUE))+
  theme(legend.position = "bottom")
  # geom_text(aes(label=node), size = 3)+ # use this line to get node labels
  # guides(fill = guide_legend(override.aes = list(alpha = .5))) # Sets legend symbols to opaque
tree_figure

tree_figure_simplified <- tree_figure %>% 
  ggtree::collapse(node = 5425) %>% 
  ggtree::collapse(node = 4016) %>% 
  ggtree::collapse(node = 4825) %>% 
  ggtree::collapse(node = 3390) %>% 
  ggtree::collapse(node = 5067) %>% 
  ggtree::collapse(node = 4184) %>% 
  ggtree::collapse(node = 3507) %>% 
  ggtree::collapse(node = 3675) %>% 
  ggtree::collapse(node = 3663) %>% 
  ggtree::collapse(node = 5085) %>%
  ggtree::collapse(node = 5177) %>% 
  ggtree::collapse(node = 4481) %>% 
  ggtree::collapse(node = 4397) %>% 
  ggtree::collapse(node = 5353) %>% 
  ggtree::collapse(node = 3612) %>% 
  ggtree::collapse(node = 5030) %>% 
  ggtree::collapse(node = 4827) %>% 
  ggtree::collapse(node = 5135) +
  geom_point2(aes(subset=(node==5425)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20I (Alpha, V1)"]])+
  geom_cladelab(node = 5425, label=paste0("(", length(getDescendants(tree, 5425)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==4016)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20E (EU1)"]])+
  geom_cladelab(node = 4016, label=paste0("(", length(getDescendants(tree, 4016)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==4825)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20G"]])+
  geom_cladelab(node = 4825, label=paste0("(", length(getDescendants(tree, 4825)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==3390)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["19B"]])+
  geom_cladelab(node = 3390, label=paste0("(", length(getDescendants(tree, 3390)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==5067)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20F"]])+
  geom_cladelab(node = 5067, label=paste0("(", length(getDescendants(tree, 5067)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==4184)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20A"]])+
  geom_cladelab(node = 4184, label=paste0("(", length(getDescendants(tree, 4184)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==3507)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20A"]])+
  geom_cladelab(node = 3507, label=paste0("(", length(getDescendants(tree, 3507)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==3675)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20A"]])+
  geom_cladelab(node = 3675, label=paste0("(", length(getDescendants(tree, 3675)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==3663)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20A"]])+
  geom_cladelab(node = 3663, label=paste0("(", length(getDescendants(tree, 3663)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==5085)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20B"]])+
  geom_cladelab(node = 5085, label=paste0("(", length(getDescendants(tree, 5085)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==5177)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20B"]])+
  geom_cladelab(node = 5177, label=paste0("(", length(getDescendants(tree, 5177)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==4481)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["21D (Eta)"]])+
  geom_cladelab(node = 4481, label=paste0("(", length(getDescendants(tree, 4481)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==4397)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20H (Beta, V2)"]])+
  geom_cladelab(node = 4397, label=paste0("(", length(getDescendants(tree, 4397)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==5353)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20J (Gamma, V3)"]])+
  geom_cladelab(node = 5353, label=paste0("(", length(getDescendants(tree, 5353)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==3612)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["21C (Epsilon)"]])+
  geom_cladelab(node = 3612, label=paste0("(", length(getDescendants(tree, 3612)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==5030)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20D"]])+
  geom_cladelab(node = 5030, label=paste0("(", length(getDescendants(tree, 5030)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==4827)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20G"]])+
  geom_cladelab(node = 4827, label=paste0("(", length(getDescendants(tree, 4827)), ")", sep=""), fontsize=text_label_size, offset = label_offset)+
  geom_point2(aes(subset=(node==5135)), shape=17, size=collapsed_clade_point_size, alpha = 1, color=clade_colors[["20B"]])+
  geom_cladelab(node = 5135, label=paste0("(", length(getDescendants(tree, 5135)), ")", sep=""), fontsize=text_label_size, offset = label_offset)
  
tree_figure_simplified
# clusters: 
# 5459 is gray (top)
# tree_figure_collapsed <- tree_figure %>%
#   ggtree::collapse(node = 3390) %>% # pink at bottom
#   ggtree::collapse(node = 4672) %>%
#   ggtree::collapse(node = 3975) %>% # middle grey
#   ggtree::collapse(node = 5112) %>% # yellow
#   ggtree::collapse(node = 5256) %>% # yellow
#   ggtree::collapse(node = 5085) %>% # yellow
#   ggtree::collapse(node = 5177) %>% # yellow
#   ggtree::collapse(node = 5055) %>% # yellow
#   ggtree::collapse(node = 3503) %>% # light blue
#   ggtree::collapse(node = 4184) %>%  # blue
#   ggtree::collapse(node = 3507) %>%  # blue
#   ggtree::collapse(node = 3875) %>%  # blue
#   ggtree::collapse(node = 3662) %>%  # blue
#   ggtree::collapse(node = 3874) %>%  # blue
#   ggtree::collapse(node = 4515) %>%  # pink
#   ggtree::collapse(node = 4467) %>%  # pink
#   ggtree::collapse(node = 4389) %>%  # pink
#   ggtree::collapse(node = 4353) %>%  # pink
#   ggtree::collapse(node = 4344) %>%  # pink
#   ggtree::collapse(node = 3588) %>%  # grey
#   ggtree::collapse(node = 3924) %>%  # grey
#   ggtree::collapse(node = 5067) %>%  # grey
#   ggtree::collapse(node = 4481) %>%  # grey
#   ggtree::collapse(node = 5353) %>%  # grey
#   ggtree::collapse(node = 4397) %>%  # grey
#   ggtree::collapse(node = 3847) %>%  # grey
#   ggtree::collapse(node = 3843) %>%  # grey
#   ggtree::collapse(node = 5030)+ # green
#   geom_point2(aes(subset=(node==3390)), shape=16, size=5, alpha = 0.5, color=region_colors[["20C"]])+
#   geom_cladelab(node = 3390, label=length(getDescendants(A2.5_subtree, 3390)), fontsize=text_label_size, offset = 0.000006)
# tree_figure_collapsed # use this line to get node labels

# ggsave("Figures/TMP_global_tree_fig.png", tree_figure_collapsed+geom_text(aes(label=node), size = 3), height = 22, width = 11, unit = "in")
# orange: 

# --------------- LEGEND ------------
# legend <- get_legend(tree_figure + 
#                        theme(legend.position = "bottom")+
#                        guides(fill = guide_legend(nrow = 5), stroke = guide_legend(nrow = 2, byrow=TRUE)))

legend = cowplot::get_plot_component(tree_figure, 'guide-box-bottom', return_all = TRUE)

# -------------- make plot together ---------------
plot_layout <- plot_grid(bargraph_figure + theme(legend.position = "none"), 
                  legend,
                  ncol = 1, 
                  rel_heights = c(3,1))

plot_layout <- plot_grid(plot_layout, 
                         tree_figure_simplified + theme(legend.position = "none"),
                         nrow = 1,
                         rel_widths = c(2,2),
                         labels = c("A", "B"))
plot_layout
ggsave("Figures/global_tree_layout_2.1.jpg", plot_layout, height = 8.5, width = 11, unit = "in")
