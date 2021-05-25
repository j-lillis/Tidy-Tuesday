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


#### create plot function

### TO DO: fix the sorting - it isn't working for some reason

density_dumbbell_plot <- function(.data, y_var, sort = F) {
  
  y_var = enquo(y_var)
  
  medians <- .data %>% group_by(!!y_var, gender) %>% 
    summarise(median_salary = median(annual_salary)) %>% 
    pivot_wider(names_from = gender, values_from = median_salary) %>% 
    rowwise() %>% 
    mutate(pay_gap = Man - Woman,
           pay_gap_pct = (Man - Woman)/Woman)  %>% 
    pivot_longer(c(Man, Woman), 
                 names_to = "gender",
                 values_to = "annual_salary") 
  
  plot_init <- if (sort == T) {
    medians %>% ggplot(aes(x = annual_salary, y = reorder(factor(!!y_var), pay_gap_pct), 
                           fill = gender, colour = gender)) 
  } else  { medians %>% ggplot(aes(x = annual_salary, y = !!y_var, 
                                   fill = gender, colour = gender)) }
  
  dumbbell_position <- position_nudge(x = 0, y = -0.1)
  
  plot_init +
  
    geom_line(aes(x = annual_salary, y = !!y_var, group = !!y_var),
              position = dumbbell_position, color = "grey", size = 3) +
    
    stat_summary(
      geom = "point", fun = "median",
      size = 5,
      position = dumbbell_position) +
    
    geom_text( 
              aes(label = round(pay_gap_pct*100,1), 
                  x = ( max(annual_salary) - min(annual_salary)))) +
    
    
    geom_density_ridges(data = .data, 
                        aes(x = annual_salary, y = !!y_var, fill = gender, colour = gender),
                        
                        position = position_nudge(x = 0, y = 0.02), scale = 0.7, alpha = 0.3,
                        size = 1) +
    
    xlim(c(0,250000))
}

df_filt %>% density_dumbbell_plot(how_old_are_you, sort = F)


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
