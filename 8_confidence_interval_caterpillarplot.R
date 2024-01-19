

# Catepillar plot
# Borg DN
# October 2023

# Packages
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(psychometric)
library(ggplot2)

# Load data and set WD
here::here()

d <- read.csv('data_results_2023-09-28.csv') %>%
  clean_names() %>%
  filter(correlation == 'TRUE')

# Loop to calculate 95% CIs
results <- vector("list", nrow(d))

for (i in 1:nrow(d)) {
  r <- d$r_value[i]
  n <- d$sample_size[i]
  
  results[[i]] <- CIr(r, n)
}

results_df <- do.call(rbind, results)

# Assign column names
colnames(results_df) <- c("lower_bound", "upper_bound")

# Bind results
df <- cbind(d, results_df)
df

# Function for CI excluding zero
interval_ex_zero <- function(a,b){sign(a)*sign(b) >= 0}

d_ci <-
  df %>%
  dplyr::select(sample_size, r_value, lower_bound, upper_bound, var_cat) %>%
  drop_na(upper_bound) %>% # remove 1 observation where a CI couldn't be calculated
  arrange(-lower_bound) %>%
  mutate(row_id = row_number(.),
         colour = ifelse(r_value>0.3 | r_value<(-0.3), '1','0'),
         does_ci_exclude_zero = interval_ex_zero(lower_bound, upper_bound))

# How many exclude null
d_ci %>%
  group_by(does_ci_exclude_zero) %>%
  summarise(n=n()) %>%
  mutate(pct = n/sum(n))

# How many greater than small effect sizes exclude null
d_ci %>%
  #filter(r_value > 0.1 | r_value < (-0.1)) %>%
  filter((r_value >=0.1 & r_value <0.3) | (r_value <=(-0.1) & r_value >(-0.3))) %>%
  group_by(does_ci_exclude_zero) %>%
  summarise(n=n()) %>%
  mutate(pct = n/sum(n))

# How many greater than moderate effect sizes exclude null
d_ci %>%
  #filter(r_value > 0.3 | r_value < (-0.3)) %>%
  filter((r_value >=0.3 & r_value <0.5)) %>%
  group_by(does_ci_exclude_zero) %>%
  summarise(n=n()) %>%
  mutate(pct = n/sum(n))


# Plot
d_ci$does_ci_exclude_zero_fct <- factor(d_ci$does_ci_exclude_zero, levels = c('FALSE', 'TRUE'), labels = c('True', 'False'))

d_ci %>%
  ggplot(aes(x = reorder(row_id, r_value),
             y = r_value,
             ymin = lower_bound,
             ymax = upper_bound))+
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.1, ymax = 0.1, fill = "gray", alpha = 1)+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.1, ymax = 0.3, fill = "red", alpha = 0.25)+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.1, ymax = -0.3, fill = "red", alpha = 0.25)+
  geom_pointrange(fatten = 1,
                  aes(colour = does_ci_exclude_zero_fct))+
  theme_classic()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'bottom')+
  scale_colour_grey()+
  #facet_wrap(~var_cat, scales = 'free_x')+
  geom_hline(yintercept = 0, colour = 'red')+
  labs(y = 'Correlation coefficient',
       colour = '95% CI includes the null value of 0')

# Save
ggsave(file = "caterpillar_plot.png",
       width = 6,
       height = 4,
       dpi = 600)


#### End



