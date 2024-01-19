

# Borg DN
# October 2023

# Packages
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggupset)
library(viridis)
library(binom)
library(cowplot)
library(grid)
library(gridExtra) 

# Load data and set WD
here::here()

d <- read_xlsx('data_participants_2023-09-28.xlsx', sheet = 1) %>%
  clean_names()

length(unique(d$study_id))

dfactors <- read.csv('data_plot_results.csv') %>%
  clean_names() %>%
  mutate(var_cat = paste0(toupper(substr(var_cat, 1, 1)),
                          substr(var_cat, 2, nchar(var_cat))))
  

# For categorising factors explored
length(unique(dfactors$study_id))

# Select para studies and wrangle data
d <- d %>%
  #filter(para == 'TRUE') %>%
  mutate(country_collected = as.factor(country_collected),
         population = as.factor(population),
         year = as.integer(year),
         study_design = as.factor(study_design),
         sample_size = as.numeric(sample_size),
         men_n = as.numeric(men_n),
         women_n = as.numeric(women_n),
         age_male = as.numeric(age_male),
         age_female = as.numeric(age_female),
         x1rm_absolute_male = as.numeric(x1rm_absolute_male),
         x1rm_absolute_female = as.numeric(x1rm_absolute_female),
         x1rm_rel_male = as.numeric(x1rm_rel_male),
         x1rm_rel_female = as.numeric(x1rm_rel_female),
         x1rm_prediction_study = as.factor(x1rm_prediction_study)) %>%
  dplyr::select(country_collected,
         population,
         year,
         study_design,
         sample_size,
         men_n,
         women_n,
         study_id,
         para,
         age_male,
         age_female,
         x1rm_absolute_male,
         x1rm_absolute_female,
         x1rm_rel_male,
         x1rm_rel_female,
         x1rm_prediction_study)

# Plot participants
dplot <- d %>%
  group_by(study_design,
           para) %>%
  summarise(sum = sum(sample_size, na.rm = T),
            men = sum(men_n, na.rm = T))

data <- data.frame(
  study_design = as.factor(c('Experimental','Prospective cohort')),
  para = as.logical(c('TRUE','TRUE')),
  sum = c(0,0),
  men = c(0,0)
) %>%
  as_tibble()

# union(dplot,data) %>%
#   ggplot(aes(x = study_design,
#              y = sum,
#              fill = para,
#              group = para))+
#   geom_bar(stat = 'identity', #$fill = '#6BAED6',
#            position = position_dodge2())+
#   theme_classic()+
#   labs(y = 'Total participants',
#        x = 'Study design')+
#   scale_fill_viridis_d(option = 'B', begin = 0.1, end = 0.75) -> p1; p1

union(dplot,data) %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para athlete studies',
                                    'FALSE' = 'Non-disabled athlete studies'),
         study_design = recode_factor(study_design,
                                      'Retrospective cohort' = 'Retrospective\ncohort',
                                      'Cross-sectional' = 'Cross-sectional',
                                      'Prospective cohort' = 'Prospective\ncohort',
                                      'Experimental' = 'Experimental')) %>%
  ggplot(aes(x = reorder(study_design, -sum),
             y = sum,
             fill = Population))+
  geom_bar(stat = 'identity')+
  theme_classic(base_size = 14)+
  labs(y = 'Total participants',
       x = 'Study design',
       fill = 'Legend')+
  theme(axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 14),
        legend.box.background = element_rect(colour = "black", size = 1))+
  scale_fill_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1)+
  scale_x_discrete(limits = c('Retrospective\ncohort',
                              'Cross-sectional',
                              'Prospective\ncohort',
                              'Experimental')) -> p1; p1


# Proportion of women
dprop <-
  d %>%
  group_by(study_design, para) %>%
  summarise(sum = sum(sample_size),
            women = sum(women_n, na.rm = T))

dprop

prop_confint <- function(x, N, ci, name, p){
  binom.confint(x = {{x}},
                n = {{N}},
                conf.level = {{ci}},
                methods = 'exact') %>%
    as_tibble() %>%
    mutate(study_design = {{name}},
           para = {{p}})
}

dplot_95 <- rbind(prop_confint(x = dprop[[4]][1], N = dprop[[3]][1], ci = 0.95, name = 'Cross-sectional', p = dprop[[2]][1]),
                  prop_confint(x = dprop[[4]][2], N = dprop[[3]][2], ci = 0.95, name = 'Cross-sectional', p = dprop[[2]][2]),
                  prop_confint(x = dprop[[4]][3], N = dprop[[3]][3], ci = 0.95, name = 'Experimental', p = dprop[[2]][3]),
                  prop_confint(x = dprop[[4]][4], N = dprop[[3]][4], ci = 0.95, name = 'Prospective\ncohort', p = dprop[[2]][4]),
                  prop_confint(x = dprop[[4]][5], N = dprop[[3]][5], ci = 0.95, name = 'Retrospective\ncohort', p = dprop[[2]][5]),
                  prop_confint(x = dprop[[4]][6], N = dprop[[3]][6], ci = 0.95, name = 'Retrospective\ncohort', p = dprop[[2]][6]))

dplot_95 %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para',
                                    'FALSE' = 'Able-bodied')) %>%
  ggplot(aes(x = study_design,
             y = mean,
             colour = Population,
             group = Population))+
  geom_pointrange(aes(ymin = lower,
                      ymax = upper),
                  position = position_dodge2(width = 0.4))+
  # geom_point(size = 2)+
  # geom_errorbar(aes(y = mean,
  #                   ymin = lower,
  #                   ymax = upper),
  #               width = 0.075)+
  theme_classic(base_size = 14)+
  scale_y_continuous(limits = c(0,0.43),
                     labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(limits = c('Retrospective\ncohort',
                              'Cross-sectional',
                              'Prospective\ncohort',
                              'Experimental'))+
  labs(y = 'Proportion of females',
       x = 'Study design')+
  theme(axis.text.x = element_text(size = 9),
        legend.position = c(0.8,0.8),
        legend.box.background = element_rect(colour = "black"))+
  scale_colour_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p2; p2 # + scale_x_discrete(labels = c('Cross-\nsectional','Retrospective\ncohort')) 





# Studies by country
# d %>%
#   mutate(para = as.factor(para),
#          para = relevel(para, ref = 'TRUE')) %>%
#   group_by(country_collected, para) %>%
#   summarise(n=n()) %>%
#   ggplot(aes(x = reorder(country_collected, -n),
#              y = n,
#              fill = para))+
#   geom_bar(stat = 'identity')+
#   theme_classic()+
#   labs(y = 'Number of studies',
#        x = 'Country [...]')+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         legend.position = c(0.8, 0.8))+
#   scale_fill_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p3; p3



# Summarise age
d %>%
  dplyr::select(para, age_male, age_female) %>%
  rename(men = age_male,
         women = age_female) %>%
  pivot_longer(cols = c(men, women),
               names_to = 'sex',
               values_to = 'age') %>%
  drop_na(age) %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para',
                                    'FALSE' = 'Able-bodied'),
         sex = as.factor(sex),
         sex = relevel(sex, ref = 'men'),
         sex = recode_factor(sex,
                             'men' = 'Men',
                             'women' = 'Women')) %>%
  ggplot(aes(x = sex,
             y = age))+
  geom_boxplot(outlier.size = -1,
               colour = 'gray70')+
  geom_jitter(aes(colour = Population,
                  group = Population),
              position = position_dodge2(0.35),
              size = 2.5)+
  theme_classic(base_size = 14)+
  labs(y = 'Sample age\n(years)',
       x = 'Sex')+
  scale_y_continuous(n.breaks = 6)+
  scale_colour_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p4; p4


# Summarise 1RM
d %>%
  dplyr::select(para,x1rm_absolute_male,x1rm_absolute_female) %>%
  rename(men = x1rm_absolute_male,
         women = x1rm_absolute_female) %>%
  pivot_longer(cols = c(men, women),
               names_to = 'sex',
               values_to = 'one_rm') %>%
  drop_na(one_rm) %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para',
                                    'FALSE' = 'Able-bodied'),
         sex = as.factor(sex),
         sex = relevel(sex, ref = 'men'),
         sex = recode_factor(sex,
                             'men' = 'Men',
                             'women' = 'Women')) %>%
  ggplot(aes(x = sex,
             y = one_rm))+
  geom_boxplot(outlier.size = -1,
               colour = 'gray70')+
  geom_jitter(aes(colour = Population,
                  group = Population),
              position = position_dodge2(0.35),
              size = 2.5)+
  theme_classic(base_size = 14)+
  labs(y = 'Sample bench press 1RM\n(kg)',
       x = 'Sex')+
  scale_colour_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p5; p5


# Summarise 1RM relative
d %>%
  dplyr::select(para, x1rm_rel_male, x1rm_rel_female) %>%
  rename(men = x1rm_rel_male,
         women = x1rm_rel_female) %>%
  pivot_longer(cols = c(men, women),
               names_to = 'sex',
               values_to = 'one_rm_rel') %>%
  drop_na(one_rm_rel) %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para',
                                    'FALSE' = 'Able-bodied'),
         sex = as.factor(sex),
         sex = relevel(sex, ref = 'men'),
         sex = recode_factor(sex,
                             'men' = 'Men',
                             'women' = 'Women')) %>%
  ggplot(aes(x = sex,
             y = one_rm_rel))+
  geom_boxplot(outlier.size = -1,
               colour = 'gray70')+
  geom_jitter(aes(colour = Population,
                  group = Population),
              position = position_dodge2(0.35),
              size = 2.5)+
  theme_classic(base_size = 14)+
  labs(y = 'Sample bench press 1RM to\nbody mass ratio',
       x = 'Sex')+
  scale_colour_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p6; p6

library(ggpubr)
key <- get_legend(p1)
ggpubr::as_ggplot(key) -> p7


# Panel
plot_grid(p1 + theme(legend.position = 'none'),
          p2 + theme(legend.position = 'none'),
          p4 + theme(legend.position = 'none'),
          p5 + theme(legend.position = 'none'),
          p6 + theme(legend.position = 'none'),
          p7,
          align = 'v',
          axis = "lr",
          scale = 0.95,
          labels = c('A','B','C','D','E'),
          rel_heights = c(0.95, 0.9, 0.9),
          ncol = 2)

# Save
ggsave(file = "figure_people.png",
       width = 9.5,
       height = 11,
       dpi = 600)








# Types of factors
# Use this for plotting different combinations of factors studied
dsub <- 
  dfactors %>%
  dplyr::select(study_id, var_cat) %>%
  left_join(., d %>% dplyr::select(study_id, para),
            by = 'study_id') %>%
  group_by(study_id, var_cat, para) %>%
  summarise(n=n()) %>%
  drop_na(var_cat) %>%
  dplyr::select(-n)

dsub %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para',
                                    'FALSE' = 'Non-disabled')) %>%
  group_by(var_cat, Population) %>%
  summarise(n=n()) %>%
  drop_na(Population) %>%
  ggplot(aes(x = reorder(var_cat, -n),
             y = n,
             fill = Population))+
  geom_bar(stat = 'identity')+
  theme_classic()+
  labs(y = 'Number of studies',
       x = 'Feature')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = c(0.8, 0.85))+
  scale_y_continuous(n.breaks = 10)+
  scale_fill_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p4; p4

# Plot unique combinations of factors
dsub %>%
  mutate(para = as.factor(para),
         para = relevel(para, ref = 'TRUE'),
         Population = recode_factor(para,
                                    'TRUE' = 'Para',
                                    'FALSE' = 'Non-disabled')) %>%
  group_by(study_id, Population) %>%
  drop_na(Population) %>%
  summarize(factors = list(var_cat)) %>%
  ggplot(aes(x = factors,
             fill = Population)) +
  geom_bar(aes(fill = Population)) +
  scale_x_upset(n_intersections = 50)+
  labs(y = 'Number of studies',
       x = 'Combination of features')+
  theme(panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        legend.position = c(0.8, 0.825))+
  scale_y_continuous(n.breaks = 8)+
  scale_fill_viridis_d(option = 'B', begin = 0.1, end = 0.75, direction = -1) -> p5; p5


# Panel
plot_grid(p4,
          p5,
          #align = 'v',
          #axis = "lr",
          scale = 0.95,
          labels = c('A','B'),
          ncol = 2)

ggsave(file = "figure_factors.png",
       width = 9,
       height = 5,
       dpi = 600)


# Plot unique combinations of factors, coloured (filled) by whether the study is a prediction equation study 
deq <- 
  dfactors %>%
  select(study_id, var_cat) %>%
  left_join(., d %>% select(study_id, para, x1rm_prediction_study),
            by = 'study_id') %>%
  group_by(study_id, var_cat, para, x1rm_prediction_study) %>%
  summarise(n=n()) %>%
  drop_na(var_cat) %>%
  select(-n)


deq %>%
  mutate(x1rm_prediction_study = relevel(x1rm_prediction_study, ref = 'TRUE')) %>%
  group_by(study_id, x1rm_prediction_study) %>%
  drop_na(para) %>%
  summarize(factors = list(var_cat)) %>%
  ggplot(aes(x = factors,
             fill = x1rm_prediction_study)) +
  geom_bar(aes(fill = x1rm_prediction_study)) +
  scale_x_upset(n_intersections = 50)+
  labs(y = 'Number of studies',
       x = 'Combinations of factors')+
  theme(panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        legend.position = c(0.8, 0.8))+
  scale_fill_viridis_d(option = 'C', begin = 0.1, end = 0.5, direction = -1) -> p5; p5

ggsave(file = "figure_factors_pred_equation.png",
       width = 5,
       height = 5,
       dpi = 600)


#### End





