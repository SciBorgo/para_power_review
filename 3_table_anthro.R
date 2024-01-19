

# Para studies: Anthro table
# Borg DN
# October 2023

# Packages
library(readxl)
library(janitor)
library(tidyr)
library(gt)
library(psychometric)
library(forcats)
library(glue)
library(dplyr)

# Load data and set WD
here::here()

d <- read_xlsx('data_participants_2023-09-28.xlsx', sheet = 1) %>%
  clean_names() %>%
  mutate(author_year = paste0(author,year)) %>%
  dplyr::select(study_id, author_year, para)

dfactors <- read.csv('data_results_2023-09-28.csv') %>%
  clean_names() %>%
  left_join(., d,
            by = 'study_id') %>%
  mutate(grouping_for_table = paste0(toupper(substr(grouping_for_table, 1, 1)),
                                     substr(grouping_for_table, 2, nchar(grouping_for_table))),
         factor_for_table = paste0(toupper(substr(factor_for_table, 1, 1)),
                                     substr(factor_for_table, 2, nchar(factor_for_table))))

dfactors_plot <- read.csv('data_plot_results.csv') %>%
  clean_names()

# Loop to calculate 95%CIs
results <- vector("list", nrow(dfactors))

for (i in 1:nrow(dfactors)) {
  r <- dfactors$r_value[i]
  n <- dfactors$sample_size[i]
  
  results[[i]] <- CIr(r, n)
}

results_df <- do.call(rbind, results)

# Assign column names to the results dataframe
colnames(results_df) <- c("lower_bound", "upper_bound")

# Bind the results to the original dataframe
df <- cbind(dfactors, results_df)


# Summary for table
df %>%
  filter(var_cat == 'anthropometric',
         correlation == 'TRUE',
         para == 'FALSE') %>%
  mutate(r_value = round(r_value,2)) %>%
  group_by(grouping_for_table,factor_for_table) %>%
  summarise(n_cors=n(),
            min = round(min(r_value),2),
            max = round(max(r_value),2)) -> sumt1

# Unique studies
df %>%
  filter(var_cat == 'anthropometric',
         correlation == 'TRUE',
         para == 'FALSE') %>%
  dplyr::select(grouping_for_table, factor_for_table, study_id) %>%
  group_by(grouping_for_table, factor_for_table, study_id) %>%
  arrange(study_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(grouping_for_table, factor_for_table) %>%
  count() %>%
  arrange(grouping_for_table,-n, factor_for_table) -> n_studies

sumt <- left_join(sumt1, n_studies,
                  by = c('grouping_for_table','factor_for_table')) %>%
  arrange(grouping_for_table,-n, factor_for_table)

sumt

# Find min correlation with 95% CIs
df %>%
  filter(var_cat == 'anthropometric',
         correlation == 'TRUE',
         para == 'FALSE') %>%
  mutate(association_1 = ifelse(r_value > 0 & lower_bound > 0 & upper_bound > 0, "Positive", "Unclear"),
         association = ifelse(r_value < 0 & lower_bound < 0 & upper_bound < 0, "Negative", association_1),
         r_value = round(r_value,2),
         lower_bound = round(lower_bound,2),
         upper_bound = round(upper_bound,2),
         cor = glue('{r_value} ({lower_bound}, {upper_bound})')) %>%
  dplyr::select(factor_for_table, r_value, cor, association) %>%
  rename(min = r_value) -> sum_min

# Find max correlation with 95% CIs
df %>%
  filter(var_cat == 'anthropometric',
         correlation == 'TRUE',
         para == 'FALSE') %>%
  mutate(association_1 = ifelse(r_value > 0 & lower_bound > 0 & upper_bound > 0, "Positive", "Unclear"),
         association = ifelse(r_value < 0 & lower_bound < 0 & upper_bound < 0, "Negative", association_1),
         r_value = round(r_value,2),
         lower_bound = round(lower_bound,2),
         upper_bound = round(upper_bound,2),
         cor = glue('{r_value} ({lower_bound}, {upper_bound})')) %>%
  dplyr::select(factor_for_table, r_value, cor, association) %>%
  rename(max = r_value) -> sum_max

# Join
dsub <-
  left_join(sumt, sum_min,
            by = c('factor_for_table','min')) %>%
  rename(cor_min = cor) %>%
  mutate(min = NULL)

dsub %>% gt() # check

dsub2 <-
  left_join(sumt, sum_max,
            by = c('factor_for_table','max')) %>%
  rename(cor_max = cor) %>%
  mutate(max = NULL)

# Make table
join <- left_join(dsub, dsub2,
                  by = c('grouping_for_table', 'factor_for_table','n','n_cors')) %>%
  #dplyr::select(-min,-max) %>%
  relocate(factor_for_table,
           n,
           n_cors,
           cor_min,
           cor_max) %>%
  mutate(association = ifelse(association.x == association.y, association.x, 'Unclear')) %>%
  dplyr::select(-association.y, -association.x, -min,-max)

join

join %>%
  filter(!(factor_for_table == 'Thigh circumference' & cor_max == '0.51 (0.12, 0.76)')) %>%
  mutate(grouping_for_table = as.factor(grouping_for_table),
         grouping_for_table = factor(grouping_for_table, levels = c('Whole body','Upper limb','Trunk','Lower limb')),
         cor_range = ifelse(cor_max > cor_min, paste0(cor_min, ' to ', cor_max), paste0(cor_min))) %>%
  dplyr::select(-cor_min,
                -cor_max) %>%
  relocate(factor_for_table,
           grouping_for_table,
           n,
           n_cors,
           cor_range,
           association) -> sum

sum %>%
  arrange(-n,-n_cors,association,desc(cor_range)) %>%
  gt() %>%
  row_group_order(c('Whole body','Upper limb','Trunk','Lower limb')) %>%
  cols_label(factor_for_table = "Variable",
             n = "Studies, n",
             n_cors = "Cors, n",
             cor_range = "Correlation coefficient (95% CI)",
             association = "Association") %>%
  cols_align(align = "left") -> gtab; gtab


gtsave(gtab, "tab_anthro.docx")

# Ref's for table
df %>%
  filter(var_cat == 'anthropometric',
         correlation == 'TRUE',
         para == 'FALSE') %>%
  dplyr::select(grouping_for_table, factor_for_table, study_id, author_year) %>%
  group_by(grouping_for_table, factor_for_table, study_id, author_year) %>%
  arrange(study_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(grouping_for_table, factor_for_table,author_year) %>%
  count() %>%
  arrange(grouping_for_table,-n, factor_for_table) -> refs

refs %>% filter(grouping_for_table == 'Lower limb')
refs %>% filter(grouping_for_table == 'Trunk')

refs %>% filter(factor_for_table == 'Body mass')
refs %>% filter(factor_for_table == 'Height')
refs %>% filter(factor_for_table == 'Upper arm circumference flexed')
refs %>% filter(factor_for_table == 'Arm cross sectional area')
refs %>% filter(factor_for_table == 'Arm length')
refs %>% filter(factor_for_table == 'Triceps muscle thickness')
refs %>% filter(factor_for_table == 'Forearm circumference')
refs %>% filter(factor_for_table == 'Triceps fascicle length')
refs %>% filter(factor_for_table == 'Triceps pennation angle')

refs %>% filter(grouping_for_table == 'Upper limb',
                !(factor_for_table %in% c('Upper arm circumference flexed',
                                          'Arm cross sectional area',
                                          'Arm length',
                                          'Triceps muscle thickness',
                                          'Forearm circumference',
                                          'Triceps fascicle length',
                                          'Triceps pennation angle')))






#### For written summary for able-bodied
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'FALSE',
         var_cat == 'anthropometric') %>%
  distinct(study_id)

d_anthro <-
  df %>%
  filter(para == 'FALSE',
         var_cat == 'anthropometric')

# How many studies looked at anthro
length(unique(d_anthro$study_id))
length(unique(d_anthro$study_id))/24

# How many correlation studies
d_anthro %>%
  filter(correlation == 'TRUE') %>%
  distinct(study_id) %>%
  nrow(.)

# Select correlation studies
da_cor <-
  d_anthro %>%
  filter(correlation == 'TRUE')

da_cor

# All features
sum %>% print(n = nrow(sum))

# Features studied more than once
sum %>%
  filter(n>1) %>%
  print(n = nrow(sum))

# More than once positive
sum %>%
  filter(n>1,
         association == 'Positive') %>%
  print(n = nrow(sum))

sum %>%
  filter(n>1,
         association == 'Unclear') %>%
  print(n = nrow(sum))




#### End



  