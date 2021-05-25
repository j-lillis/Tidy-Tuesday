library(tidyverse)
library(ggridges)
library(patchwork)
library(here)
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

density_dumbbell_plot <- function(.data, y_var, sort = F) {
  
  y_var = enquo(y_var)
  
  medians <- .data %>% group_by(!!y_var, gender) %>% 
    summarise(median_salary = median(annual_salary)) %>% 
    pivot_wider(names_from = gender, values_from = median_salary) %>% 
    rowwise() %>% 
    mutate(pay_gap = Man - Woman,
           pay_gap_pct = (Man - Woman)/Woman,
           y_var = factor(!!y_var))  %>% 
    ungroup() %>% group_by(!!y_var) %>% 
    mutate(text_location = median(c(Man, Woman)))  %>% 
    pivot_longer(c(Man, Woman), 
                 names_to = "gender",
                 values_to = "annual_salary") %>% ungroup()
  
  plot_init <- if (sort == T) {
    medians %>% ggplot(aes(x = annual_salary, y = reorder(y_var, pay_gap_pct), 
                           fill = gender, colour = gender)) 
  } else  { medians %>% ggplot(aes(x = annual_salary, y = !!y_var, 
                                   fill = gender, colour = gender)) }
  
  dumbbell_position <- position_nudge(x = 0, y = -0.1)
  
  plot_init +
    geom_line(aes(x = annual_salary, group = y_var),
              position = dumbbell_position, color = "grey", size = 3) +
    
    stat_summary(
      geom = "point", fun = "median",
      size = 5,
      position = dumbbell_position) +
    
    geom_text( 
      aes(label = round(pay_gap_pct*100,1), 
          x = text_location)) +
    
    
    geom_density_ridges(data = .data, 
                        aes(x = mean(annual_salary), y = !!y_var, fill = gender, colour = gender),
                        
                        position = position_nudge(x = 0, y = 0.02), scale = 0.7, alpha = 0.3,
                        size = 1) +
    
    xlim(c(0,250000))
}

density_dumbbell_plot(df_filt, industry, sort = T)




(education | experience) / (industry | age)

ggsave(filename =  paste0("temp-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), 
       device = NULL, path = here("Temp plots"),
       dpi = 320, width = 20, height = 12)
