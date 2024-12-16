# load libraries
library(tidyverse)
library(sf)

# load DR database (with locations)
DR_database <- read.csv("Data_Reanalysis/sample_data/DR_metadata_with_lineage.csv")

# calculate number of samples per region
DR_samples_per_region <- DR_database %>% 
  group_by(region) %>% 
  summarise(number_of_samples = n()) %>% 
  ungroup()

DR_samples_per_region <- full_join(data.frame(region = c("Cibao Nordeste", "Cibao Norte", "Cibao Sur", "Cibao Noroeste", "El Valle", "Enriquillo", "Valdesia", "Ozama", "Higuamo", "Yuma")),
                                   DR_samples_per_region, 
                                   by = "region")

# load shapefile
DR_spdf <- sf::st_read("Data_Reanalysis/dom_admbnda_one_20210629_shp/dom_admbnda_adm1_one_20210629.shp")
  
# change the region names (remove special characters, etc) to match metadata
DR_spdf <- DR_spdf %>% 
  rename(region = ADM1_ES) %>% 
  mutate(region = case_when(
    region == "Región Cibao Nordeste" ~ "Cibao Nordeste",
    region == "Región Cibao Noroeste" ~ "Cibao Noroeste", 
    region == "Región Cibao Norte" ~ "Cibao Norte",
    region == "Región Cibao Sur" ~ "Cibao Sur",
    region == "Región El Valle" ~ "El Valle",
    region == "Región Enriquillo" ~ "Enriquillo",
    region == "Región Higuamo" ~ "Higuamo",
    region == "Región Ozama" ~ "Ozama",
    region == "Región Valdesia" ~ "Valdesia",
    region == "Región Yuma" ~ "Yuma",
    TRUE ~ as.character(region)
  ))

lat_long_each_region <- data.frame(latitude_center = c("19.404729", "19.483696", "19.057353","19.698838", "19.015716","18.256615", "18.588412", "18.571742","18.873611", "18.676135"), 
                                   longitude_center = c("-70.073727", "-70.779821", "-70.424707", "-71.428534", "-71.422058", "-71.467392", "-70.573662",  "-69.964889", "-69.588880", "-68.698770"), 
                                   region = c("Cibao Nordeste", "Cibao Norte", "Cibao Sur", "Cibao Noroeste", "El Valle", "Enriquillo", "Valdesia", "Ozama", "Higuamo", "Yuma"))

# Convert lat/long to a sf
lat_long_each_region_geometry <- lat_long_each_region %>%
  st_as_sf(coords = c("longitude_center", "latitude_center"))

#join samples data to shapefile
DR_spdf_metadata <- inner_join(DR_samples_per_region, DR_spdf, by="region") 

DR_spdf_metadata$number_of_samples <- as.factor(DR_spdf_metadata$number_of_samples)

#set colors for scale
sample_colors <- c("#dae7f5", "#b6d2f0", "#8fb8e3", "#8fb8e3", "#8fb8e3", "#06305c")
names(sample_colors) <- c("1", "2", "3", "4", "5", "83")

# set colors for the values (so scale is a little less extreme)
map_figure <- ggplot()+
  theme_void()+
  geom_sf(data = DR_spdf_metadata, aes(geometry = geometry, fill = number_of_samples))+
  scale_fill_manual(values=sample_colors, na.value = "white")+
  geom_sf(data = lat_long_each_region_geometry, size=0)+
  geom_sf_text(data = lat_long_each_region_geometry, aes(label = region), size=3)


