

# Analysis methods table
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
  clean_names()
length(unique(d$study_id))

dfactors <- read.csv('data_results_2023-09-28.csv') %>%
  clean_names() %>%
  dplyr::select(study_id, correlation, r_ci, p_value_correlation_reported, regression, reported_r_squared, scatter_plot, group_differences)
length(unique(dfactors$study_id))


# Main dataset
dmain <- d %>%
  dplyr::select(author,
         year,
         study_id,
         study_design,
         para,
         sample_size,
         correlation_type,
         between_group_analysis,
         group_difference_effect_size,
         group_difference_uncertanity)

# Wrangle correlation variables
dcorr <- 
  dfactors %>%
  dplyr::select(study_id, correlation, r_ci, p_value_correlation_reported, scatter_plot) %>%
  filter(correlation == 'TRUE') %>%
  group_by(study_id, r_ci, p_value_correlation_reported, scatter_plot, correlation) %>%
  summarise(n=n())

print(dcorr, n = nrow(dcorr))

d_cor <- dcorr %>%
  dplyr::select(study_id, correlation) %>%
  group_by(study_id, correlation) %>%
  count() %>%
  dplyr::select(-n) %>%
  left_join(., dmain %>% dplyr::select(study_id, correlation_type),
            by = 'study_id') %>%
  mutate(correlation = correlation_type) %>%
  dplyr::select(-correlation_type)

d_95 <- dcorr %>%
  dplyr::select(study_id, r_ci) %>%
  group_by(study_id,r_ci) %>%
  count() %>%
  dplyr::select(-n)

d_p <- dcorr %>%
  dplyr::select(study_id, r_ci) %>%
  group_by(study_id, p_value_correlation_reported) %>%
  count() %>%
  pivot_wider(names_from = p_value_correlation_reported,
              values_from = n,
              values_fill = 0) %>%
  rename(p_valueYes = `TRUE`,
         p_valueNo = `FALSE`) %>%
  mutate(p_valueYes = ifelse(p_valueYes == 0, 0, 1),
         p_valueNo = ifelse(p_valueNo == 0, 0, 1))

# Scatter plot
dcorreg <- 
  dfactors %>%
  dplyr::select(study_id, correlation, regression, scatter_plot) %>%
  filter(correlation == 'TRUE' | regression == 'TRUE') %>%
  group_by(study_id, scatter_plot) %>%
  summarise(n=n())

d_scatter <- dcorreg %>%
  dplyr::select(study_id, scatter_plot) %>%
  group_by(study_id, scatter_plot) %>%
  count() %>%
  pivot_wider(names_from = scatter_plot,
              values_from = n,
              values_fill = 0) %>%
  rename(scatter_plotYes = `TRUE`,
         scatter_plotNo = `FALSE`) %>%
  mutate(scatter_plotYes = ifelse(scatter_plotYes == 0, 0, 1),
         scatter_plotNo = ifelse(scatter_plotNo == 0, 0, 1))

# Regression
dreg <- 
  dfactors %>%
  dplyr::select(study_id, regression, reported_r_squared) %>%
  filter(regression == 'TRUE') %>%
  group_by(study_id, regression, reported_r_squared) %>%
  summarise(n=n())

dregression <- dreg %>%
  dplyr::select(study_id, regression) %>%
  group_by(study_id, regression) %>%
  count() %>%
  dplyr::select(-n)
  
# R-squared reporting
d_rsquared <- dreg %>%
  dplyr::select(study_id, reported_r_squared) %>%
  group_by(study_id, reported_r_squared) %>%
  count() %>%
  dplyr::select(-n)

# Group difference analysis
# dgroup <- 
#   dfactors %>%
#   dplyr::select(study_id, group_differences) %>%
#   filter(group_differences == 'TRUE') %>%
#   group_by(study_id, group_differences) %>%
#   summarise(n=n()) %>%
#   dplyr::select(-n)

# dgroup_es <-
#   dmain %>%
#   filter(between_group_analysis == 'TRUE') %>%
#   dplyr::select(study_id, group_difference_effect_size, group_difference_uncertanity)


# Joins
j0 <- merge(dmain, d_cor, by = 'study_id', all.x = T)

j1 <- merge(j0, d_95, by = 'study_id', all.x = T)

j2 <- merge(j1, d_p, by = 'study_id', all.x = T) %>%
  mutate(p_value = ifelse(p_valueYes == 1 & p_valueNo == 0, 'T',
                          ifelse(p_valueYes == 0 & p_valueNo == 1, 'F',
                                 ifelse(p_valueYes == 1 & p_valueNo == 1, 'Partial', .))))

j3 <- merge(j2, d_scatter, by = 'study_id', all.x = T) %>%
  mutate(scatter_plot_shown = ifelse(scatter_plotYes == 1 & scatter_plotNo == 0, 'T',
                                     ifelse(scatter_plotYes == 0 & scatter_plotNo == 1, 'F',
                                            ifelse(scatter_plotYes == 1 & scatter_plotNo == 1, 'Partial', .))))

j4 <- merge(j3, dregression, by = 'study_id', all.x = T) %>%
  mutate(regression = ifelse(is.na(regression), 'F', 'T'))
j4

j5 <- merge(j4, d_rsquared, by = 'study_id', all.x = T)
j5

# j6 <- merge(j5, dgroup, by = 'study_id', all.x = T)
# j6


# Make table
j5 %>%
  mutate(para = recode_factor(as.factor(para),
                              'FALSE' = 'Able-bodied athlete studies',
                              'TRUE' = 'Para athlete studies'),
         correlation = ifelse(is.na(correlation), 'F', correlation),
         r_ci = ifelse(r_ci == 'TRUE', 'T',
                       ifelse(r_ci == 'FALSE', 'F', .)),
         reported_r_squared = ifelse(is.na(reported_r_squared), 'F', 'T'),
         reported_r_squared = ifelse(regression == 'F', 'NA', reported_r_squared),
         between_group_analysis = ifelse(between_group_analysis == 'FALSE', 'F',
                                             ifelse(between_group_analysis == 'TRUE', 'T', between_group_analysis)),
         between_group_effect_size = ifelse(between_group_analysis == 'F', 'NA', group_difference_effect_size),
         between_group_effect_size = ifelse(between_group_effect_size == 'FALSE', 'F', between_group_effect_size),
         between_group_uncertantity = ifelse(between_group_analysis == 'F', 'NA', group_difference_uncertanity),
         between_group_uncertantity = ifelse(between_group_uncertantity == 'FALSE', 'F',
                                             ifelse(between_group_uncertantity == 'TRUE', 'T', between_group_uncertantity))) %>%
  dplyr::select(-p_valueYes,
                -p_valueNo,
                -scatter_plotYes,
                -scatter_plotNo,
                -study_id,
                -correlation_type,
                -group_difference_effect_size,
                -group_difference_uncertanity) %>%
  arrange(para, study_design, year) %>%
  relocate(-between_group_analysis,
           -between_group_effect_size,
           -between_group_uncertantity) -> dtab

dtab %>%
  gt(groupname_col = "para") %>%
  cols_label(author = "Author",
            year = "Year",
            study_design = "Study design",
            sample_size = "Sample size, n",
            correlation = "Correlation analysis",
            r_ci = "Correlation uncertainty",
            p_value = "Correlation p-value",
            scatter_plot_shown = "Scatter plot",
            regression = "Regression analysis",
            reported_r_squared = "R-squared",
            between_group_analysis = "Between group analysis",
            between_group_effect_size = "Group difference effect size",
            between_group_uncertantity = "Group difference uncertainty") %>%
  cols_align(align = "left") -> analysis_tab

analysis_tab

gtsave(analysis_tab, "analysis_tab.docx")


# Written summary: Able-bodied studies
dtab %>%
  filter(para == 'Able-bodied athlete studies') %>%
  group_by(para, correlation, regression, between_group_analysis) %>%
  summarise(n=n()) %>%
  mutate(pct = (n/24)*100) %>%
  arrange(-n)

sumfun <- function(var) {dtab %>%
  filter(para == 'Able-bodied athlete studies') %>%
  group_by(para, {{var}}) %>%
  summarise(n=n()) %>%
  mutate(pct = (n/24)*100) %>%
  arrange(-n)}
  
sumfun(correlation)
sumfun(r_ci); sumfun(r_ci)[2][[1]]
sumfun(p_value); sumfun(p_value)[2][[1]]
sumfun(scatter_plot_shown); sumfun(scatter_plot_shown)[2][[1]]

dtab %>%
  filter(para == 'Able-bodied athlete studies') %>%
  group_by(para, correlation, regression) %>%
  summarise(n=n()) %>%
  mutate(pct = (n/24)*100) %>%
  arrange(-n)

sumfun(scatter_plot_shown); sumfun(scatter_plot_shown)[2][[1]]


sumfun(between_group_analysis); sumfun(between_group_analysis)[2][[1]]
sumfun(between_group_effect_size); sumfun(between_group_effect_size)[2][[1]]
sumfun(between_group_uncertantity); sumfun(between_group_uncertantity)[2][[1]]


# Written summary para studies
dtab %>%
  filter(para == 'Para athlete studies') %>%
  group_by(para, correlation, regression, between_group_analysis) %>%
  summarise(n=n()) %>%
  mutate(pct = (n/8)*100) %>%
  arrange(-n)

sumfun <- function(var) {dtab %>%
    filter(para == 'Para athlete studies') %>%
    group_by(para, {{var}}) %>%
    summarise(n=n()) %>%
    mutate(pct = (n/8)*100) %>%
    arrange(-n)}

sumfun(correlation)
sumfun(r_ci); sumfun(r_ci)[2][[1]]
sumfun(p_value); sumfun(p_value)[2][[1]]
sumfun(scatter_plot_shown); sumfun(scatter_plot_shown)[2][[1]]

dtab %>%
  filter(para == 'Para athlete studies') %>%
  group_by(para, correlation, regression) %>%
  summarise(n=n()) %>%
  mutate(pct = (n/8)*100) %>%
  arrange(-n)

sumfun(between_group_analysis); sumfun(between_group_analysis)[2][[1]]
sumfun(between_group_effect_size); sumfun(between_group_effect_size)[2][[1]]
sumfun(between_group_uncertantity); sumfun(between_group_uncertantity)[2][[1]]


#### End
