################################################################################
# GOAL: this script is intended to check for any confounding variables that led
 # to differences in WGS success
################################################################################

#############
### setup ###
#############

# load libraries
library(tidyverse)

# load data
all_sample_metadata_raw <- read.csv("Data_Reanalysis/sample_data/dr.data.cleaned.csv")
successful_sample_metadata <- read.csv("Data_Reanalysis/sample_data/DR_metadata_with_lineage.csv")

# make list of samples that met all qc: 
high_qual_seqs <- successful_sample_metadata %>% pull(samplename)

# add whether or not sample passed qc to larger dataframe
# all_sample_metadata %<>%
all_sample_metadata <- all_sample_metadata_raw %>% 
  mutate(samplename = str_replace(tube.rename, "_", ""),
         pass_seq_qc = ifelse(samplename %in% high_qual_seqs, "y", "n"))

###################################################################
### analysis of sample success vs potential confounding factors ###
###################################################################
# table for statistical test results: 
statistical_tests <- data.frame(var = as.character(), test = as.character(), p = as.numeric())

# variables with chi-squared test
cat_vars <- c("patient.locality", "region", "sample.type", "symptoms", "sex")
for (i in 1:length(cat_vars)){
  
    # make a plot
    assign(paste0("plot_", cat_vars[i]),
      ggplot(all_sample_metadata, aes(x = .data[[cat_vars[i]]], fill = pass_seq_qc)) +
      geom_bar() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
    )
  
  qc_table <- table(all_sample_metadata[[ cat_vars[i] ]], all_sample_metadata$pass_seq_qc)
  test <- chisq.test(qc_table)
  statistical_tests <- rbind(statistical_tests, 
                             data.frame(var = cat_vars[i], test = "chi_squared", p = test[["p.value"]]))
  
}

# variables with t test
cont_vars <- c("age_years", "ct")
for (j in 1:length(cont_vars)){
  # make a plot
  assign(paste0("plot_", cont_vars[j]),
         ggplot(all_sample_metadata, aes(y = .data[[cont_vars[j]]], x = pass_seq_qc)) +
           geom_boxplot() +
           theme_minimal()
  )
  
  test <- t.test(
    all_sample_metadata %>% 
      filter(pass_seq_qc == "y") %>% 
      pull(cont_vars[j]),
    all_sample_metadata %>% 
      filter(pass_seq_qc == "n") %>% 
      pull(cont_vars[j]),
    alternative = "two.sided"
  )
  
  statistical_tests <- rbind(statistical_tests, 
                             data.frame(var = cont_vars[j], test = "t.test", p = test[["p.value"]]))
}

# collection.date
all_sample_metadata <- all_sample_metadata %>%
  mutate(
    collection.date = as.Date(collection.date),
    qc_fail = ifelse(pass_seq_qc == "n", 1, 0)  # or == "y" if you prefer
  )

model <- glm(qc_fail ~ collection.date, data = all_sample_metadata, family = binomial)
summary(model)

ggplot(all_sample_metadata, aes(x = collection.date, y = qc_fail)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) +
  labs(y = "Probability of QC fail") +
  theme_minimal()

statistical_tests <- rbind(statistical_tests, 
                           data.frame(var = "collection.date", test = "linear_regression", p = summary(model)$coefficients["collection.date", "Pr(>|z|)"]))

# save stuff
write.csv(statistical_tests, "Data_Reanalysis/sample_sequencing_confounders_analysis/statistical_test_results.csv", row.names = F)

# Find all plot objects that start with "plot_"
plots <- ls(pattern = "^plot_")

# Loop to save each
for (p in plots) {
  filename <- paste0("Data_Reanalysis/sample_sequencing_confounders_analysis/", p, ".png")
  png(filename, width = 1600, height = 1200)
  print(get(p))
  dev.off()
}

for (p in plots) {
  ggsave(filename = paste0("Data_Reanalysis/sample_sequencing_confounders_analysis/", p, ".jpg"), plot = get(p), width = 8, height = 6, unit = "in")
}

## TRASH
# # patient.locality
# all_sample_metadata %>% 
#   ggplot(aes(fill = pass_seq_qc, x = patient.locality))+
#   theme_minimal()+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
# 
# locality_qc_table <- table(all_sample_metadata$patient.locality, all_sample_metadata$pass_seq_qc)
# locality_test <- chisq.test(locality_qc_table)
# 
# statistical_tests <- rbind(statistical_tests, 
#                            data.frame(var = "patient.locality", test = "chi_squared", p = locality_test[["p.value"]]))
# 
# # region
# all_sample_metadata %>% 
#   ggplot(aes(fill = pass_seq_qc, x = region))+
#   theme_minimal()+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
# 
# region_qc_table <- table(all_sample_metadata$region, all_sample_metadata$pass_seq_qc)
# region_test <- chisq.test(region_qc_table)
# 
# statistical_tests <- rbind(statistical_tests, 
#                            data.frame(var = "region", test = "chi_squared", p = region_test[["p.value"]]))
# 
# # sample type
# all_sample_metadata %>% 
#   ggplot(aes(fill = pass_seq_qc, x = sample.type))+
#   theme_minimal()+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
# 
# sample.type_qc_table <- table(all_sample_metadata$sample.type, all_sample_metadata$pass_seq_qc)
# sample.type_test <- chisq.test(sample.type_qc_table)
# sample.type_test
# statistical_tests <- rbind(statistical_tests, 
#                            data.frame(var = "sample.type", test = "chi_squared", p = sample.type_test[["p.value"]]))
# 
# # symptom
# all_sample_metadata %>% 
#   ggplot(aes(fill = pass_seq_qc, x = symptoms))+
#   theme_minimal()+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
# 
# symptoms_qc_table <- table(all_sample_metadata$symptom, all_sample_metadata$pass_seq_qc)
# symptoms_test <- chisq.test(symptoms_qc_table)
# symptoms_test
# statistical_tests <- rbind(statistical_tests, 
#                            data.frame(var = "symptoms", test = "chi_squared", p = symptoms_test[["p.value"]]))

## CT
# all_sample_metadata %>% 
#   ggplot(aes(x = pass_seq_qc, y = ct))+
#   theme_minimal()+
#   geom_boxplot()
# 
# ct_test <- t.test(
#   all_sample_metadata %>% 
#     filter(pass_seq_qc == "y") %>% 
#     pull(ct),
#   all_sample_metadata %>% 
#     filter(pass_seq_qc == "n") %>% 
#     pull(ct),
#   alternative = "two.sided"
# )
# 
# statistical_tests <- rbind(statistical_tests, 
#                            data.frame(var = "CT", test = "t.test", p = ct_test[["p.value"]]))



