#libraries
library(cowplot)


#load map figure
source("Scripts/DR_MapFigure.R")

# load lineage / sample collection figure
source("Scripts/DR_Samples_Timeline_by_Lineage_Figure.R")

# load DR travel restrictions plot
source("Scripts/Restrictions_Figures.R")

# map_figure
# DR_Lineages_figure
# Cumulative_Cases_Plot

layout_1 <- cowplot::plot_grid(new_cases_plot, map_figure+theme(legend.position="none"), nrow=1, rel_heights=c(1,1), rel_widths=c(1.25, 1), labels = c("A", "B"))
layout_2 <- cowplot::plot_grid(layout_1, DR_Lineages_figure, ncol = 1, labels = c("", "C"), rel_heights = c(1, 1.4))
layout_2

ggsave("Figures/Figure_1_Layout.pdf", height = 11, width = 8, unit = "in")


 