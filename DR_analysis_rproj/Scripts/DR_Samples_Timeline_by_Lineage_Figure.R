#load libraries
library(tidyverse)
library(lubridate)

# load gisaid all DR samples early pandemic from GISAID
Caribbean_samples <- read_tsv("Data/Dominican_Republic_GISAID_Early_Pandemic_Samples.tsv")
# load all of our lab DR samples
DR_samples <- read.csv("Data/DR_Database_With_Lineages.csv")
# nextclade classifications
nextclade_classifications <- read_tsv("Data/nextclade_DR_GISAID_SGS_20230316.tsv")
  
# convert date to date format
Caribbean_samples$date <- as.Date(Caribbean_samples$date, format="%Y-%m-%d")
DR_samples$collection.date <- as.Date(DR_samples$collection.date, format="%Y-%m-%d")

#combine Study Samples and GISAID data
AllData <- rbind(DR_samples %>% 
                   select(collection.date, Lineage, sample) %>% 
                   rename(date = collection.date) %>% 
                   rename(pangolin_lineage = Lineage) %>% 
                   rename(strain = sample) %>% 
                   mutate(samples = "SGS"),
                 Caribbean_samples %>% 
                   filter(grepl("*Ghedin*", authors)==FALSE) %>% 
                   select(date, pangolin_lineage, strain) %>% 
                   mutate(samples = "GISAID"))

AllData$date <- as.Date(AllData$date)

#join with nextclade classifications
AllData <- left_join(AllData, nextclade_classifications %>% rename(strain = seqName), by = "strain")

#color palette
clade_colors <- c("#771155","#5c5c5c", "#AA4488", "#CC99BB", "#114477", 
                  "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
                  "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", 
                  "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122")
names(clade_colors) <- c("20F", "20A", "20I (Alpha, V1)", "20B", "20D", 
                         "20G", "20C", "20E (EU1)", "21J (Delta)", "20H (Beta, V2)",
                         "21D (Eta)", "21F (Iota)", "21C (Epsilon)", "20J (Gamma, V3)", "19B", 
                         "21I (Delta)", "21B (Kappa)", "19A", "21G (Lambda)", "21A (Delta)")

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
  facet_wrap(~samples)

# remove additional datasets
rm(AllData, Caribbean_samples, DR_samples, nextclade_classifications)


