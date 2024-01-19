
# Borg DN
# October 2023

# Packages
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)

# Load data and set WD
here::here()

d <- read_xlsx('data_participants_2023-09-28.xlsx', sheet = 1) %>%
  clean_names() %>%
  mutate(country_collected = as.factor(country_collected),
                       population = as.factor(population),
                       year = as.integer(year),
                       study_design = as.factor(study_design),
                       sample_size = as.numeric(sample_size),
                       men_n = as.numeric(men_n),
                       women_n = as.numeric(women_n),
                       age_male = as.numeric(age_male),
                       age_female = as.numeric(age_female),
                       mass_male = as.numeric(mass_male),
                       mass_female = as.numeric(mass_female),
                       height_male = as.numeric(height_male),
                       height_female = as.numeric(height_female),
                       x1rm_absolute_male = as.numeric(x1rm_absolute_male),
                       x1rm_absolute_female = as.numeric(x1rm_absolute_female),
                       x1rm_rel_male = as.numeric(x1rm_rel_male),
                       x1rm_rel_female = as.numeric(x1rm_rel_female),
                       x1rm_prediction_study = as.factor(x1rm_prediction_study))

length(unique(d$study_id))

# Look at variables
names(d)

# Number of participants
d %>%
  group_by(para) %>%
  summarise(sum = sum(sample_size),
            med = median(sample_size),
            q1 = quantile(sample_size, prob = 0.25),
            q3 = quantile(sample_size, prob = 0.75)) -> s1; s1

d %>%
  group_by(para) %>%
  summarise(sum = sum(women_n, na.rm = T)) %>%
  mutate(pct_ab = sum/s1$sum[1],
         pct_para = sum/s1$sum[2])

# Population able-bodied
d %>%
  filter(para == 'FALSE') %>%
  group_by(population) %>%
  summarise(n=n()) %>%
  mutate(pct = (n/nrow(.))*100) %>%
  print(n = nrow(.))


# Involve males; involve females
d %>%
  group_by(para, men) %>%
  summarise(n=n())

d %>%
  group_by(para, women) %>%
  summarise(n=n())

# AB male and female summary
d %>%
  filter(para == 'FALSE') %>%
  dplyr::select(age_male,
                mass_male,
                height_male,
                x1rm_absolute_male,
                x1rm_rel_male) %>%
  summary()

d %>%
  filter(para == 'FALSE') %>%
  dplyr::select(age_female,
                mass_female,
                height_female,
                x1rm_absolute_female,
                x1rm_rel_female) %>%
  summary()

# Para male and female summary
d %>%
  filter(para == 'TRUE') %>%
  dplyr::select(age_male,
                mass_male,
                height_male,
                x1rm_absolute_male,
                x1rm_rel_male) %>%
  summary()

d %>%
  filter(para == 'TRUE') %>%
  dplyr::select(age_female,
                mass_female,
                height_female,
                x1rm_absolute_female,
                x1rm_rel_female) %>%
  summary()


# Study design summary
d %>%
  group_by(para, study_design) %>%
  summarise(n=n())

d %>%
  group_by(para, competition_results, author) %>%
  summarise(n=n()) %>%
  print(n = nrow(.))



#### End




