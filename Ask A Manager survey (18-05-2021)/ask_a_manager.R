library(tidyverse)
library(ggridges)
library(patchwork)
library(wesanderson)
library(ghibli)
library(here)
options(scipen = 999)


#### to do: add fonts, add titles + subtitles, add scales formatting to x axis, add caption, 
# use geom_text to add in base size for each density ???

df_import <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

top_6_industries <- df_import %>% 
  count(industry) %>% 
  arrange(desc(n)) %>% 
  head(6) %>% 
  pull(industry)

df_filt <- df_import %>% 
  filter(gender %in% c("Woman", "Man") 
         & currency %in% c("USD", "CAD", "GBP") 
         & how_old_are_you != "under 18") %>% 
  drop_na(highest_level_of_education_completed) %>% 
  mutate(years_of_experience_in_field = ifelse(years_of_experience_in_field == "5-7 years", 
                                               "5 - 7 years", years_of_experience_in_field),
         years_of_experience_in_field = fct_collapse(years_of_experience_in_field, 
                                                     "21 years or more" = c("21 - 30 years",
                                                                            "31 - 40 years",
                                                                            "41 years or more"))) %>% 
  mutate(age = fct_rev(how_old_are_you),
         experience = fct_rev(fct_relevel(years_of_experience_in_field,
                                          "1 year or less",
                                          "2 - 4 years",
                                          "5 - 7 years",
                                          "8 - 10 years",
                                          "11 - 20 years",
                                          "21 years or more")),
         education = fct_rev(fct_relevel(highest_level_of_education_completed,
                                         "High School",
                                         "Some college",
                                         "College degree",
                                         "Professional degree (MD, JD, etc.)",
                                         "Master's degree",
                                         "PhD")))


# colours
background <- "black"
text_colour <- ghibli_palettes$PonyoLight[2]
yellow <- ghibli_palettes$PonyoMedium[6]
blue <- ghibli_palettes$PonyoMedium[3]
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
  text_pct_position <- position_nudge(x = 0, y = -0.3)
  
  plot_init +
    
    geom_line(aes(x = annual_salary, group = y_var),
              position = dumbbell_position, 
              color = text_colour, size = 2, alpha = 0.5) +
    
    stat_summary(
      geom = "point", fun = "median",
      size = 5,
      position = dumbbell_position) +
    
    stat_summary(
      geom = "point", fun = "median",
      size = 2.5, colour = "black",
      position = dumbbell_position) +
    
    geom_density_ridges(data = .data, 
                        aes(x = annual_salary, y = !!y_var, colour = gender),
                        
                        position = position_nudge(x = 0, y = 0.02), scale = 0.7, alpha = 0.3,
                        size = 1) +
    
    geom_text( 
      aes(label = paste0("+", round(pay_gap_pct*100,0), "%"), 
          x = text_location),
      position = text_pct_position,
      colour = alpha(yellow, 0.3)) +
    xlim(c(0,250000)) +
    
    scale_fill_manual(values = c(yellow, blue),
                      aesthetics = c("colour", "fill")) +
    
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
    
    
    theme(plot.background  = element_rect(fill = background, colour = background),
          panel.background = element_rect(fill = background, colour = background),
          axis.text.y = element_text(size = 12, 
                                     colour = alpha(text_colour, 0.6),
                                     margin = margin(r = -20),
                                     hjust = 0.5),
          axis.text.x = element_text(size = 12, 
                         colour = alpha(text_colour, 0.5),
                         hjust = 0.5),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = alpha(ghibli_palettes$PonyoMedium[2], 0.1), size = 1.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          #plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.position = "none") +
    
    coord_cartesian(clip = "off")
}

density_dumbbell_plot(df_filt, experience, sort = F)

density_dumbbell_plot(df_filt, age) + density_dumbbell_plot(df_filt, experience) +
  density_dumbbell_plot(df_filt %>% filter(industry %in% top_6_industries), industry, sort = T) + density_dumbbell_plot(df_filt, education) +
  plot_layout(ncol = 2)


ggsave(filename =  paste0("temp-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), 
       device = NULL, path = here("Temp plots"),
       dpi = 320, width = 18, height = 12)
