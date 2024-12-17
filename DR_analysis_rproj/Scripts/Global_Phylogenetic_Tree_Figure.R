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

# load data from previous script
source("Scripts/Global_Phylogenetic_Tree_Data_Formatting.R")

# ---------- TREE FIGURE SETUP -------------------------------


clade_colors <- c("#771155","#5c5c5c", "#AA4488", "#CC99BB", "#114477", 
                  "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
                  "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", 
                  "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122")
names(clade_colors) <- c("20F", "20A", "20I (Alpha, V1)", "20B", "20D", 
                         "20G", "20C", "20E (EU1)", "21J (Delta)", "20H (Beta, V2)",
                         "21D (Eta)", "21F (Iota)", "21C (Epsilon)", "20J (Gamma, V3)", "19B", 
                         "21I (Delta)", "21B (Kappa)", "19A", "21G (Lambda)", "21A (Delta)")

options(ignore.negative.edge=TRUE)

# tree branch colors
tree_branch_colors <- "#2b2b2b"

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
# --------- PLOT TREE --------------------- 
metadata <- metadata %>% 
  mutate(GL_yn_numeric = ifelse(GL_yn == "y", 1, 0)) %>% 
  mutate(sample_in_tree = ifelse(strain %in% tree$tip.label, "y", "n"))
metadata$GL_yn_numeric <- as.numeric(metadata$GL_yn_numeric)
 
tree_figure <- ggtree(tree, color = tree_branch_colors, size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(fill=Nextstrain_clade, alpha = GL_yn, stroke = GL_yn_numeric), shape=21, size=4)+
  scale_alpha_discrete(range = c(0.5, 0.95))+
  scale_fill_manual(values = clade_colors)+
  guides(fill = guide_legend(nrow = 5), stroke = guide_legend(nrow = 2, byrow=TRUE))+
  theme(legend.position = "bottom")

# --------------- LEGEND ------------
# legend <- get_legend(tree_figure + 
#                        theme(legend.position = "bottom")+
#                        guides(fill = guide_legend(nrow = 5), stroke = guide_legend(nrow = 2, byrow=TRUE)))

legend = cowplot::get_plot_component(tree_figure, 'guide-box-bottom', return_all = TRUE)

# -------------- make plot together ---------------
plot_layout <- plot_grid(bargraph_figure + theme(legend.position = "none"), 
                  legend,
                  ncol = 1, 
                  rel_heights = c(4,1))

plot_layout <- plot_grid(plot_layout, 
                         tree_figure + theme(legend.position = "none"),
                         nrow = 1,
                         rel_widths = c(2,3),
                         labels = c("A", "B"))

ggsave("Figures/global_tree_layout_1.1.svg", plot_layout, height = 8.5, width = 11, unit = "in")
