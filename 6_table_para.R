

# Para studies
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
  mutate(var_cat = paste0(toupper(substr(var_cat, 1, 1)),
                                     substr(var_cat, 2, nchar(var_cat))),
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

# Assign column names
colnames(results_df) <- c("lower_bound", "upper_bound")

# Bind results
df <- cbind(dfactors, results_df)

# Summary for table
df %>%
  filter(para == 'TRUE',
         correlation == 'TRUE') %>%
  mutate(r_value = round(r_value,2)) %>%
  group_by(var_cat,factor_for_table) %>%
  summarise(n_cors=n(),
            min = round(min(r_value),2),
            max = round(max(r_value),2)) -> sumt1

# Unique studies
df %>%
  filter(para == 'TRUE',
         correlation == 'TRUE') %>%
  dplyr::select(var_cat, factor_for_table, study_id) %>%
  group_by(var_cat, factor_for_table, study_id) %>%
  arrange(study_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(var_cat, factor_for_table) %>%
  count() -> n_studies

sumt <- left_join(sumt1, n_studies,
                  by = c('var_cat','factor_for_table')) %>%
  arrange(var_cat,-n, factor_for_table)

sumt

# Find min correlations and their 95% CIs
df %>%
  filter(para == 'TRUE',
         correlation == 'TRUE') %>%
  mutate(association_1 = ifelse(r_value > 0 & lower_bound > 0 & upper_bound > 0, "Positive", "Unclear"),
         association = ifelse(r_value < 0 & lower_bound < 0 & upper_bound < 0, "Negative", association_1),
         r_value = round(r_value,2),
         lower_bound = round(lower_bound,2),
         upper_bound = round(upper_bound,2),
         cor = glue('{r_value} ({lower_bound}, {upper_bound})')) %>%
  dplyr::select(factor_for_table, r_value, cor, association) %>%
  rename(min = r_value) -> sum_min

# Find max correlations and their 95% CIs
df %>%
  filter(para == 'TRUE',
         correlation == 'TRUE') %>%
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

dsub %>% gt()

dsub2 <-
  left_join(sumt, sum_max,
            by = c('factor_for_table','max')) %>%
  rename(cor_max = cor) %>%
  mutate(max = NULL)

# Make table
join <- left_join(dsub, dsub2,
                  by = c('var_cat', 'factor_for_table','n','n_cors')) %>%
  relocate(factor_for_table,
           n,
           n_cors,
           cor_min,
           cor_max) %>%
  mutate(association = ifelse(association.x == association.y, association.x, 'Unclear')) %>%
  dplyr::select(-association.y, -association.x, -min,-max)

join

join %>%
  mutate(cor_range = ifelse(cor_max > cor_min, paste0(cor_min, ' to ', cor_max), paste0(cor_min))) %>%
  dplyr::select(-cor_min,
                -cor_max) %>%
  relocate(factor_for_table,
           var_cat,
           n,
           n_cors,
           cor_range,
           association) -> sum

sum %>%
  arrange(-n,-n_cors,association,desc(cor_range)) %>%
  gt() %>%
  row_group_order(c('Anthropometric','Body composition','Demographic', 'Technical')) %>%
  cols_label(factor_for_table = 'Variable',
             #var_cat = "Variable",
             n = "Studies, n",
             n_cors = "Cors, n",
             cor_range = "Correlation coefficient (95% CI)",
             association = "Association") %>%
  cols_align(align = "left") -> gtab; gtab


gtsave(gtab, "tab_para.docx")

# Investigation for reporting in table
df %>%
  filter(factor_for_table == 'Barbell mean propulsive velocity',
         para == 'TRUE') %>%
  dplyr::select(r_value,
                lower_bound,
                upper_bound)

# Ref's for tables
# Unique studies
df %>%
  filter(para == 'TRUE',
         correlation == 'TRUE') %>%
  dplyr::select(var_cat, factor_for_table, study_id, author_year) %>%
  group_by(var_cat, factor_for_table, study_id, author_year) %>%
  arrange(study_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(var_cat, factor_for_table,author_year) %>%
  count()






#### Summary of features for para-athlete
# Disability features: impairment type and severity
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'TRUE',
         var_cat == 'disability') %>%
  distinct(study_id)

d_disab <-
  df %>%
  filter(para == 'TRUE',
         var_cat == 'Disability')

# How many correlation studies
d_disab %>%
  filter(correlation == 'TRUE') %>%
  distinct(study_id)

# Select correlation studies
da_cor <-
  d_disab %>%
  filter(correlation == 'TRUE')

da_cor


# Anthropometric 
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'TRUE',
         var_cat == 'anthropometric') %>%
  distinct(study_id)

d_bodcomp <-
  df %>%
  filter(para == 'TRUE',
         var_cat == 'body composition')

# How many correlation studies
d_bodcomp %>%
  filter(correlation == 'TRUE') %>%
  distinct(study_id) %>%
  nrow(.)

# Select correlation studies
da_cor <-
  d_bodcomp %>%
  filter(correlation == 'TRUE')

da_cor




# Body composition
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'TRUE',
         var_cat == 'body composition') %>%
  distinct(study_id)

d_bodcomp <-
  df %>%
  filter(para == 'TRUE',
         var_cat == 'Body composition')

# How many correlation studies
d_bodcomp %>%
  filter(correlation == 'TRUE') %>%
  distinct(study_id) %>%
  nrow(.)

# Select correlation studies
da_cor <-
  d_bodcomp %>%
  filter(correlation == 'TRUE')

da_cor


# Demographic
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'TRUE',
         var_cat == 'demographic') %>%
  distinct(study_id, factor_for_table)



# Technical
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'TRUE',
         var_cat == 'technical') %>%
  distinct(study_id)

d_tech <-
  df %>%
  filter(para == 'TRUE',
         var_cat == 'Technical')

# How many correlation studies
d_tech %>%
  filter(correlation == 'TRUE') %>%
  distinct(study_id)

# Select all technical studies
d_tech %>%
  dplyr::select(author_year, correlation, regression, group_mean_difference)




# Neuromusclar
left_join(dfactors_plot,
          {d %>% dplyr::select(study_id, para)},
          by = 'study_id') %>%
  filter(para == 'TRUE',
         var_cat == 'neuromuscular') %>%
  distinct(study_id)



#### End
  