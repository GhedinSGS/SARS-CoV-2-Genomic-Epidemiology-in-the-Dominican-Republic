#load libraries
library(tidyverse)

# --------- SETUP ------------

#load data
metadata_in_analysis <- read.csv("Data/DR_Database_With_Lineages.csv")

num_samples_in_analysis <- nrow(metadata_in_analysis)

# --------- STATISTICAL CALCULATIONS ----------
table_statistics <- data.frame(variable = as.character(), 
                               symptoms = as.character(), 
                               count = as.numeric(), 
                               percent = as.numeric(), 
                               table_format = as.character())

#calculate all statistics for symptomatic and asymptomatic patients separately
for (i in 1:length(c("Asymptomatic", "Symptomatic"))){
  current_symp <- c("Asymptomatic", "Symptomatic")[i]
  
  #load data for this group
  data_temp <- metadata_in_analysis %>% 
    filter(symptoms == current_symp)
  
  # statistics by sex
  metadata_sex <- data_temp %>% 
    group_by(sex, symptoms) %>% 
    summarise(count = n()) %>% 
    mutate(percent = count / num_samples_in_analysis * 100) %>% 
    mutate(percent = format(round(percent, 2))) %>% 
    mutate(table_format = paste(count, " (", percent, ")", sep="")) %>% 
    rename(variable = sex) %>% 
    mutate(symptoms = current_symp)
  metadata_sex$count <- as.numeric(metadata_sex$count)
  metadata_sex$percent <- as.numeric(metadata_sex$percent)
  
  # statistics by age
  metdata_age <- data.frame(variable = "age", symptoms = current_symp, 
                            count = round(mean(data_temp$age_years), 2), 
                            percent = round(sd(data_temp$age_years),2), 
                            table_format = paste0(round(mean(data_temp$age_years), 2), 
                                                  "; ",
                                                  min(data_temp$age_years),
                                                  " - ",
                                                  max(data_temp$age_years),
                                                  " (", 
                                                  round(sd(data_temp$age_years), 2), 
                                                  ")" 
                                                  )
  )
  
  
  # statistics by region
  metadata_region <- data_temp %>% 
    group_by(region) %>% 
    summarise(count = n()) %>% 
    mutate(percent = count / num_samples_in_analysis * 100) %>% 
    mutate(percent = format(round(percent, 2))) %>% 
    mutate(table_format = paste(count, " (", percent, ")", sep="")) %>% 
    rename(variable = region) %>% 
    mutate(symptoms = current_symp)
  metadata_region$count <- as.numeric(metadata_region$count)
  metadata_region$percent <- as.numeric(metadata_region$percent)   
  
  # statistics by lineage
  metadata_lineage <- data_temp %>% 
    group_by(Lineage) %>% 
    summarise(count = n()) %>% 
    mutate(percent = count / num_samples_in_analysis * 100) %>% 
    mutate(percent = format(round(percent, 2))) %>% 
    mutate(table_format = paste(count, " (", percent, ")", sep="")) %>% 
    rename(variable = Lineage) %>% 
    mutate(symptoms = current_symp)
  metadata_lineage$count <- as.numeric(metadata_lineage$count)
  metadata_lineage$percent <- as.numeric(metadata_lineage$percent)
  
  # SYMPTOMATIC / ASYMPTOMATIC
  metadata_symptomatic <- data_temp %>% 
    group_by(symptoms) %>% 
    summarise(count = n()) %>% 
    mutate(percent = count / num_samples_in_analysis * 100) %>% 
    mutate(percent = format(round(percent, 2))) %>% 
    mutate(table_format = paste(count, " (", percent, ")", sep="")) %>% 
    rename(variable = symptoms) %>% 
    mutate(symptoms = current_symp)
  
  metadata_symptomatic$count <- as.numeric(metadata_symptomatic$count)
  metadata_symptomatic$percent <- as.numeric(metadata_symptomatic$percent)
  
  # join all data
  table_statistics <- rbind(table_statistics,
                            metadata_sex,
                            metdata_age,
                            metadata_region,
                            metadata_lineage,
                            metadata_symptomatic)
  table_statistics$count <- as.numeric(table_statistics$count)
  table_statistics$percent <- as.numeric(table_statistics$percent)
  
  rm(metdata_age)

}
table_statistics_2 <- table_statistics %>% 
  #select variables
  select(variable, symptoms, table_format) %>% 
  # pivot to summarise
  pivot_wider(names_from = symptoms,
              values_from = table_format) %>% 
  # add variable category
  mutate(category = (case_when(variable == "age" ~ "Age",
                            variable %in% c("M", "F") ~ "Sex",
                            variable %in% c("Asymptomatic", "Symptomatic") ~ "Symptoms",
                            variable %in% c(unique(metadata_in_analysis$region)) ~ "Region",
                            variable %in% c(unique(metadata_in_analysis$Lineage)) ~ "Lineage",
    TRUE ~ "X"
  ))) %>% 
  arrange(category) %>% 
  mutate(variable = case_when(variable == "M" ~ "Male",
                              variable == "F" ~ "Female",
                              TRUE ~ variable)) %>% 
  replace(is.na(.), "0")

