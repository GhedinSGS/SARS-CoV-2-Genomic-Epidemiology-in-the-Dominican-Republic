#load libraries
library(tidyverse)
library(lubridate)

# load gisaid all DR samples early pandemic from GISAID
Caribbean_samples <- read_tsv("Data_Reanalysis/Dominican_Republic_GISAID_Early_Pandemic_Samples.tsv")
# load all of our lab DR samples
DR_samples <- read.csv("Data_Reanalysis/sample_data/DR_metadata_with_lineage.csv")
# nextclade classifications
nextclade_classifications <- read_tsv("Data_Reanalysis/nextclade_DR_GISAID_SGS_20230316.tsv")
  
# convert date to date format
Caribbean_samples$date <- as.Date(Caribbean_samples$date, format="%Y-%m-%d")
DR_samples$collection.date <- as.Date(DR_samples$collection.date, format="%m/%d/%y")

#combine Study Samples and GISAID data
AllData <- rbind(DR_samples %>% 
                   select(collection.date, pango_lineage, samplename) %>% 
                   rename(date = collection.date) %>% 
                   rename(pangolin_lineage = pango_lineage) %>% 
                   rename(strain = samplename) %>% 
                   mutate(samples = "SGS"),
                 Caribbean_samples %>% 
                   filter(grepl("*Ghedin*", authors)==FALSE) %>% 
                   select(date, pangolin_lineage, strain) %>% 
                   mutate(samples = "GISAID"))

AllData$date <- as.Date(AllData$date)

#join with nextclade classifications
AllData <- left_join(AllData, nextclade_classifications %>% rename(strain = seqName), by = "strain")

#color palette
# clade_colors <- c("#771155","#5c5c5c", "#AA4488", "#CC99BB", "#114477",
#                   "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
#                   "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", 
#                   "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122")

clade_colors <- c("#0050dc", "#00b31d",  "#eed41f", "#ff7a00",  "#ea229d", 
                  "#464746",  "#eb2f16", "#45b0cf",  "#8413b6", "#9aa65a",  
                  "#d48bd3", "#e1a17b",  "#895437", "white", "white", "white", "white", "white", "white", "white") 

names(clade_colors) <- c("20A", "20I (Alpha, V1)", "20B", 
                         "20G", "20C", "20E (EU1)", "21J (Delta)",
                         "21F (Iota)", "21C (Epsilon)", "20J (Gamma, V3)", "19B", 
                         "19A", "21A (Delta)", 
                         "20F", "21I (Delta)","20D", "21D (Eta)", "20H (Beta, V2)", "21B (Kappa)", "21G (Lambda)")
  
# names(clade_colors) <- c("20F", "20A", "20I (Alpha, V1)", "20B", "20D", 
#                          "20G", "20C", "20E (EU1)", "21J (Delta)", "20H (Beta, V2)",
#                          "21D (Eta)", "21F (Iota)", "21C (Epsilon)", "20J (Gamma, V3)", "19B", 
#                          "21I (Delta)", "21B (Kappa)", "19A", "21G (Lambda)", "21A (Delta)")

facet_labels = c('SGS' = "This Study",
                 'GISAID' = "GISAID")
DR_Lineages_figure <- ggplot(data = AllData %>% 
                               group_by(date, pangolin_lineage, samples, clade) %>% 
                               summarise(count = n()) %>% 
                               ungroup() %>% 
                               filter(date >= "2020-07-29" & date <= "2021-02-25"), 
       aes(x = date, y = pangolin_lineage, size = count, color = clade))+
  theme_bw()+
  geom_point(shape=1, stroke =1.5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0, 0))+
  xlab("Date")+
  ylab("Pangolin Lineage")+
  scale_color_manual(values = clade_colors)+
  facet_wrap(~samples, labeller = as_labeller(facet_labels))+
  guides(size=guide_legend(title="number of samples"))+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
DR_Lineages_figure
ggsave("Figures/Caribbean_lineage_over_time_comparison_2.1.pdf", DR_Lineages_figure, height = 6, width = 8, unit = "in")
