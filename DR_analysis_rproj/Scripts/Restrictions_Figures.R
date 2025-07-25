# Load libraries
library(tidyverse)
library(cowplot)
library(svglite)

#load datasets
#cumulative COVID-19 Cases in DR
cumulative_covid_cases <- read.csv("Data_Reanalysis/Travel_Restrictions/DR_COVID_Cases_Cumulative.csv")


#DR Precautions
precautions_dates <- read.csv("Data_Reanalysis/Travel_Restrictions/DominicanRepublic_Restrictions_Dateranges.csv")

#set dates to date format
cumulative_covid_cases$date <- as.Date(cumulative_covid_cases$date, format = "%Y-%m-%d")

precautions_dates$Start_Date <- as.Date(precautions_dates$Start_Date, format = "%m/%d/%y")
precautions_dates$End_Date <- as.Date(precautions_dates$End_Date, format = "%m/%d/%y")

### new

new_cases_plot <- cumulative_covid_cases %>% 
  mutate(month = format(date, "%m"),
         year = format(date, "%Y")) %>% 
  group_by(month, year) %>% 
  summarise(new_cases_month = sum(new_cases)) %>% 
  ungroup() %>% 
  arrange(year, month) %>% 
  mutate(month_year_date = as.Date(paste0("01-", month, "-", year), format = "%d-%m-%Y")) %>%
  ggplot(aes(x = month_year_date, y = new_cases_month))+
    theme_bw()+
    geom_col()+
    scale_x_date(date_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=30, hjust=1), legend.position = "bottom")+
    xlab("Date")+
    ylab("New Cases in the \nDominican Republic")+
    # gatherings under 100 people only
    geom_segment(aes(x = as.Date("2020-03-17"), y = 5*(30000/5), xend = as.Date("2021-03-01"), yend = 5*(30000/5), color = "No Large Gatherings"), size = 2)+
    
    #school closures
    geom_segment(aes(x = as.Date("2020-03-19"), y = 3*(30000/5), xend = as.Date("2021-03-01"), yend = 3*(30000/5), color = "School Closure"), size = 2)+

    #essential worker restrictions
    geom_segment(aes(x = as.Date("2020-03-19"), y = 4*(30000/5), xend = as.Date("2021-03-01"), yend = 4*(30000/5), color = "Work From Home"), size = 2)+

    #masking
    geom_segment(aes(x = as.Date("2020-05-01"), y =30000/5, xend = as.Date("2021-03-01"), yend = 30000/5, color = "Masking Required"), size = 2)+

    #border closures
    geom_segment(aes(x = as.Date("2020-03-16"), y = 0, xend = as.Date("2020-07-01"), yend = 0, color = "Border Closure"), size = 2)+
    geom_segment(aes(x = as.Date("2020-12-22"), y = 0, xend = as.Date("2021-03-01"), yend = 0, color = "Border Closure"), size = 2)+

    #Movement within countries restricted
    geom_segment(aes(x = as.Date("2020-03-20"), y = 2*(30000/5), xend = as.Date("2020-10-07"), yend = 2*(30000/5), color = "Movement within country restricted"), size = 2)


