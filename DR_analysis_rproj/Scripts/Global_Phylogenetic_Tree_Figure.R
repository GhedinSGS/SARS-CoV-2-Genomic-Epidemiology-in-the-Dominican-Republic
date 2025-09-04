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

background_alpha = 0.8

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
  # geom_text(aes(label=node), size = 3) # use this line to get node labels
  # guides(fill = guide_legend(override.aes = list(alpha = .5))) # Sets legend symbols to opaque
tree_figure


########################################################
### collapsed nodes with big trianges in their place
########################################################
tree_figure_collapsed <- tree_figure %>% 
  ggtree::collapse(node = 7729, 'max', fill=clade_colors[["20I (Alpha, V1)"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 7554, 'max', fill=clade_colors[["20J (Gamma, V3)"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 6070, 'max', fill=clade_colors[["20H (Beta, V2)"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 7025, 'max', fill=clade_colors[["20F"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 5566, 'max', fill=clade_colors[["21I (Delta)"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 4474, 'max', fill=clade_colors[["19A"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 5719, 'max', fill=clade_colors[["20G"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 5901, 'max', fill=clade_colors[["21C (Epsilon)"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 7547, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 6815, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 6764, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 7014, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 7011, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 4630, 'max', fill=clade_colors[["19B"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 4528, 'max', fill=clade_colors[["19B"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 4715, 'max', fill=clade_colors[["21D (Eta)"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 4754, 'max', fill=clade_colors[["21J (Delta)"]], alpha=background_alpha)  %>% 
  ggtree::collapse(node = 6557, 'max', fill=clade_colors[["20D"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 7538, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 7405, 'max', fill=clade_colors[["20B"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 5849, 'max', fill=clade_colors[["20C"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 5848, 'max', fill=clade_colors[["20C"]], alpha=background_alpha) %>%
  ggtree::collapse(node = 5329, 'max', fill=clade_colors[["20A"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 5113, 'max', fill=clade_colors[["20E (EU1)"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 5100, 'max', fill=clade_colors[["20E (EU1)"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 4697, 'max', fill=clade_colors[["20A"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 4699, 'max', fill=clade_colors[["20A"]], alpha=background_alpha) %>% 
  ggtree::collapse(node = 4482, 'max', fill=clade_colors[["19B"]], alpha=background_alpha)
  


tree_figure_collapsed

# ggtree::collapse(node = NODE, 'max', fill=clade_colors[["COLOR"]], alpha=background_alpha)


# --------------- LEGEND ------------
# legend <- get_legend(tree_figure + 
#                        theme(legend.position = "bottom")+
#                        guides(fill = guide_legend(nrow = 5), stroke = guide_legend(nrow = 2, byrow=TRUE)))

legend = cowplot::get_plot_component(tree_figure_collapsed, 'guide-box-bottom', return_all = TRUE)

# -------------- make plot together ---------------
plot_layout <- plot_grid(bargraph_figure + theme(legend.position = "none"), 
                  legend,
                  ncol = 1, 
                  rel_heights = c(3,1))

plot_layout <- plot_grid(plot_layout, 
                         tree_figure_collapsed + theme(legend.position = "none"),
                         nrow = 1,
                         rel_widths = c(2,2),
                         labels = c("A", "B"))
plot_layout
ggsave("Figures/FIG2_global_tree_layout_4.1.svg", plot_layout, height = 8.5, width = 16, unit = "in")
