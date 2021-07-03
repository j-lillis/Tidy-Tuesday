library(ggridges)
library(patchwork)
library(tidyverse)
library(ghibli)
library(ggtext)
library(extrafont)
library(here)

# import and clean data

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
  mutate(age = fct_rev(str_replace_all(how_old_are_you, "-", " - ")),
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
                                         "PhD"))) %>% 
  
  # currency exchange rates taken from xe.com for 09/05/2021
  mutate(annual_salary = case_when(currency == "CAD" ~ annual_salary * 0.8238345533,
                                   currency == "GBP" ~ annual_salary * 1.3989316868,
                                   TRUE ~ annual_salary))


# plot setup

background <- ghibli_palettes$PonyoDark[1]
text_colour <- ghibli_palettes$PonyoLight[2]
yellow <- ghibli_palettes$PonyoMedium[6]
blue <- ghibli_palettes$PonyoMedium[3]

font_bold <- "Roboto-Bold"
font_regular <- "Roboto-Regular"
font_thin <- "Roboto-Light"


# plot function

density_dumbbell_plot <- function(.data, y_var, sort = F, pct_change_text = T) {
  
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
    
    { if(pct_change_text == TRUE) 
      
      geom_text( 
        aes(label = paste0("+", round(pay_gap_pct*100,0), "%"), 
            x = text_location),
        position = text_pct_position,
        colour = alpha(yellow, 0.5),
        family = font_regular) } +
    
    scale_fill_manual(values = c(yellow, blue),
                      aesthetics = c("colour", "fill")) +
    
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
    
    scale_x_continuous(limits = c(0, 250000), labels = scales::label_dollar()) +
    
    theme(plot.background  = element_rect(fill = background, colour = background),
          panel.background = element_rect(fill = background, colour = background),
          axis.text.y = element_text(size = 12, 
                                     colour = alpha(text_colour, 0.7),
                                     margin = margin(r = -15),
                                     hjust = 0.5,
                                     family = font_regular),
          axis.text.x = element_text(size = 12, 
                                     colour = alpha(text_colour, 0.2),
                                     hjust = 0.5,
                                     family = font_regular),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = alpha(ghibli_palettes$PonyoMedium[2], 0.1), size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          
          plot.subtitle = element_markdown(colour = text_colour, size = 20, family = font_regular,
                                           margin = margin(0,0,20,0)),
          
          plot.title.position = "plot",
          plot.margin = grid::unit(c(5,5,10,10), "mm"),
          legend.position = "none") +
    
    coord_cartesian(clip = "off")
}


# create individual plots

age <- density_dumbbell_plot(df_filt, age) + 
  labs(subtitle = "Age:")

experience <- density_dumbbell_plot(df_filt, experience) + 
  labs(subtitle = "Experience in field:")

industry <- density_dumbbell_plot(df_filt %>% filter(industry %in% top_6_industries), industry, sort = T) + 
  labs(subtitle = "Industry:")

education <- density_dumbbell_plot(df_filt, education) + 
  labs(subtitle = "Highest level of education:")


# create legend
reps <- 1000000

dummy_dists <- tibble(annual_salary = c(rnorm(reps,3),
                                        rnorm(reps,2.2)), 
                      gender = factor(c(rep("Man", reps), rep("Woman", reps))),
                      var_z = rep(reps*2))

legend <- dummy_dists %>%  
  density_dumbbell_plot(y_var = var_z, pct_change_text = F) + 
  xlim(c(-1,6))

y_position_neg = nrow(dummy_dists) - 0.1
y_position_pos = nrow(dummy_dists) + 0.25

female_x <- 0.1
male_x <- 5.1
x_difference <- 1.6
y_drop <- 0.4
y_raise <- 0.4

legend_annotated <- dummy_dists %>%  
  density_dumbbell_plot(y_var = var_z, pct_change_text = F) + 
  xlim(c(-1,6)) + 
  
  geom_text( 
    aes(label = paste0("\'+'*chi*\'%'")),
    x = 2.6,
    position = position_nudge(x = 0, y = -0.25),
    colour = alpha(yellow, 0.5),
    family = font_regular, parse = T) +
  
  # female distribution
  geom_curve(aes(x = female_x, xend = female_x + x_difference - 0.7, y = y_position_pos + y_raise, yend = y_position_pos),
             curvature = 0.2,
             arrow = arrow(length = unit(0.03, "npc")),
             color = text_colour) +
  annotate("text", x = female_x, y = y_position_pos + 0.7, label = "Female\nsalary\ndistribution", color = blue) +
  
  # male distribution
  geom_curve(aes(x = male_x, xend = male_x - x_difference + 0.7, y = y_position_pos + y_raise, yend = y_position_pos),
             curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc")),
             color = text_colour) +
  annotate("text", x = male_x, y = y_position_pos + 0.7, label = "Male\nsalary\ndistribution", color = yellow) +
  
  # female median
  geom_curve(aes(x = female_x, xend = female_x + x_difference, y = y_position_neg - y_drop, yend = y_position_neg),
             curvature = -0.4,
             arrow = arrow(length = unit(0.03, "npc")),
             color = text_colour) +
  annotate("text", x = female_x, y = y_position_neg - 0.7, label = "Median\nfemale\nsalary", color = blue) +
  
  # male median
  geom_curve(aes(x = male_x , xend = male_x - x_difference, y = y_position_neg - y_drop, yend = y_position_neg),
             curvature = 0.4, 
             arrow = arrow(length = unit(0.03, "npc")),
             color = text_colour) +
  annotate("text", x = male_x, y = y_position_neg - 0.7, label = "Median\nmale\nsalary", color = yellow) +
  
  # pct change
  geom_curve(aes(x = 2.7, xend = 2.7, y = y_position_neg -1, yend = y_position_neg - 0.3),
             curvature = 0.2,
             arrow = arrow(length = unit(0.03, "npc")),
             color = text_colour) +
  annotate("text", x = 2.7, y = y_position_neg - 1.3, 
           label = "Percentage difference\n between male and\nfemale median", color = text_colour) +
  
  annotate("text", x = 2.7, y = nrow(dummy_dists) + 1.7, 
           label = "How to read\nthe plots:", color = text_colour, size = 7, family = font_regular,
           lineheight = 1) +
  
  theme(axis.text.x  = element_blank(),
        #plot.margin = grid::unit(c(-10,5,-10,3), "mm"),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = alpha(text_colour,0.05), color = NA),
        panel.background = element_rect(fill = alpha(text_colour,0.05), color = NA))


# create intro text

intro_text <- ggplot() +
  geom_textbox(aes(x = -3, y = 1, 
                   label = "The **Ask A Manager Survey** collected information on salaries in April - May 2021.<br><br>
                Data presented here only includes respondents who provided their salary in USD, CAD, or GBP — these were then standardised to USD. Respondents were filtered to only include <span style = 'color:#278B9AFF;'>**women**</span> and <span style = 'color:#D8AF39FF;'>**men**</span> over the age of 18.<br><br>
                X axis is limited to $250,000 for clarity although maximum salaries exceed this. *n*=23,714 except for the **Industry** plot, which only shows responses for the six most popular industries (*n*=12,876)."), 
               family = "Roboto", size = 4.5, 
               colour = NA, fill = NA, text.colour = text_colour,
               width = grid::unit(0.95, "npc"),
               halign = 0.5, lineheight = 1.35) +
  theme_void() +
  theme(plot.margin = grid::unit(c(0,5,0,3), "mm"),
        panel.spacing = grid::unit(1, "mm"),
        plot.background = element_rect(fill = alpha(text_colour,0.05), color = NA),
        panel.background = element_rect(fill = alpha(text_colour,0.05), color = NA))


# assemble entire plot

layout <- "
FFBBBBCCCC
FFBBBBCCCC
AADDDDEEEE
AADDDDEEEE
"

plot <- legend_annotated + age + experience + industry + education + plot_layout(design = layout) +
  intro_text +
  plot_annotation(title = "**Ask A Manager Survey 2021:** exploring the gender pay gap",
                  caption = "Plot created by Johnny Lillis | github.com/j-lillis | twitter.com/johnny_c_lillis | Data from Ask A Manager | #TidyTuesday") &  
  theme(plot.background = element_rect(fill = background, colour = NA),
        plot.title = element_markdown(colour = text_colour, family = "Roboto", size = 45, hjust = 0, margin = margin(15,0,15,-10)),
        plot.caption = element_text(colour = alpha(text_colour, 0.7), family = font_regular, hjust = 0, size = 12),
        plot.margin = margin(5,5,5,5,"mm"))


# save to temp plots file
# ggsave(filename =  paste0("temp-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), 
#        plot = plot,
#        device = NULL, path = here("Temp plots"),
#        dpi = 320, width = 17, height = 14)

# save final plot
ggsave(filename =  "ask_a_manager_survey_plot.png", 
       plot = plot,
       device = NULL, path = here("Ask A Manager survey (18-05-2021)"),
       dpi = 320, width = 17, height = 14)

