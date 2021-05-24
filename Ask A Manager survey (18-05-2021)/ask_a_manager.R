library(tidyverse)
library(ggridges)
library(patchwork)
options(scipen = 999)

df_import <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

top_10_industries <- df_import %>% 
  count(industry) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  pull(industry)

df_filt <- df_import %>% 
  filter(industry %in% top_10_industries 
         & gender %in% c("Woman", "Man") 
         & currency %in% c("USD", "CAD", "GBP") 
         & how_old_are_you != "under 18") %>% 
  mutate(years_of_experience_in_field = ifelse(years_of_experience_in_field == "5-7 years", 
                                               "5 - 7 years", years_of_experience_in_field)) %>% 
  mutate(how_old_are_you = fct_rev(how_old_are_you),
         years_of_experience_in_field = fct_rev(fct_relevel(years_of_experience_in_field,
                                                    "1 year or less",
                                                    "2 - 4 years",
                                                    "5 - 7 years",
                                                    "8 - 10 years",
                                                    "11 - 20 years",
                                                    "21 - 30 years",
                                                    "31 - 40 years",
                                                    "41 years or more")))

pay_gap_by_industry <- df_filt %>% group_by(industry, gender) %>% 
  summarise(median_salary = median(annual_salary)) %>% 
  pivot_wider(names_from = gender, values_from = median_salary) %>% 
  rowwise() %>% 
  mutate(pay_gap = Man - Woman) %>% 
  mutate(industry = fct_reorder(industry, pay_gap))

density_plot <- df_filt %>% filter(industry == "Computing or Tech") %>%
  group_by(industry, gender) %>% 
  ggplot(aes(x = annual_salary, fill = gender, colour = gender)) +
  geom_density(position = position_nudge(x = 0, y = -5), alpha = 0.8) +
  xlim(c(0,500000))

dot_plot <- df_filt %>% filter(industry == "Computing or Tech") %>%
  group_by(industry, gender) %>% 
  summarise(median_salary = median(annual_salary)) %>% 
ggplot(aes(x = median_salary, y = industry, fill = gender, colour = gender)) +
  geom_point(size = 3) +

      xlim(c(0,500000))

density_plot / dot_plot  

df_filt %>% group_by(industry, gender) %>% 
  ggplot(aes(x = annual_salary, y = industry, colour = gender)) + 
  geom_density_ridges(position = position_nudge(x = 0, y = 0.02), scale = 0.8, alpha = 0.3,
                      size = 1) +
stat_summary(aes(y = industry),
             geom = "point", fun = "median",
             size = 3,
             position = position_nudge(x = 0, y = -0.1)) +
  scale_color_manual(values = c("red", "blue")) +
  xlim(c(0,250000))


industry <- df_filt %>% group_by(industry, gender) %>% 
  ggplot(aes(x = annual_salary, y = industry, colour = gender)) + 
  geom_density_ridges(position = position_nudge(x = 0, y = 0.02), scale = 0.8, alpha = 0.3,
                      size = 1) +
  stat_summary(aes(y = industry),
               geom = "point", fun = "median",
               size = 3,
               position = position_nudge(x = 0, y = -0.1)) +
  scale_color_manual(values = c("red", "blue")) +
  xlim(c(0,250000))

education <- df_filt %>% group_by(highest_level_of_education_completed, gender) %>% 
  ggplot(aes(x = annual_salary, y = highest_level_of_education_completed, colour = gender)) + 
  geom_density_ridges(position = position_nudge(x = 0, y = 0.02), scale = 0.8, alpha = 0.3,
                      size = 1) +
  stat_summary(aes(y = highest_level_of_education_completed),
               geom = "point", fun = "median",
               size = 3,
               position = position_nudge(x = 0, y = -0.1)) +
  scale_color_manual(values = c("red", "blue")) +
  xlim(c(0,250000))

experience <- df_filt %>% group_by(years_of_experience_in_field, gender) %>% 
  ggplot(aes(x = annual_salary, y = years_of_experience_in_field, colour = gender)) + 
  geom_density_ridges(position = position_nudge(x = 0, y = 0.02), scale = 0.8, alpha = 0.3,
                      size = 1) +
  stat_summary(aes(y = years_of_experience_in_field),
               geom = "point", fun = "median",
               size = 3,
               position = position_nudge(x = 0, y = -0.1)) +
  scale_color_manual(values = c("red", "blue")) +
  xlim(c(0,250000))

age <- df_filt %>% group_by(how_old_are_you, gender) %>% 
  ggplot(aes(x = annual_salary, y = how_old_are_you, colour = gender)) + 
  geom_density_ridges(position = position_nudge(x = 0, y = 0.02), scale = 0.8, alpha = 0.3,
                      size = 1) +
  stat_summary(aes(y = how_old_are_you),
               geom = "point", fun = "median",
               size = 3,
               position = position_nudge(x = 0, y = -0.1)) +
  scale_color_manual(values = c("red", "blue")) +
  xlim(c(0,250000))


(education | experience) / (industry | age)
