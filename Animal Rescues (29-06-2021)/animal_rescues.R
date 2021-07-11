library(tidyverse)
library(lubridate)
library(extrafont)
library(cowplot)

animal_rescues_import <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

red <- "#d52b1f"
grey <- "#636b6e"
font <- "Lato"


plot <- animal_rescues_import %>% 
  mutate(animal_group_parent = fct_lump(factor(animal_group_parent), 5)) %>% 
  
  filter(cal_year < 2021, animal_group_parent != "Other")  %>% 
  
  group_by(cal_year, animal_group_parent) %>% 
  summarise(total = n(), 
            cost = sum(as.numeric(incident_notional_cost), na.rm = T)) %>% 
  mutate(animal_group_parent = paste0(animal_group_parent, if_else(animal_group_parent == "Fox", "es", "s"))) %>% 
  ggplot(aes(x = cal_year, y = total)) + geom_col(width = 0.3, fill = red) +
  facet_wrap(~ factor(animal_group_parent, levels = c("Cats", "Birds", "Dogs", "Foxes", "Horses")), 
             nrow = 1,
             strip.position = "bottom") +
  labs(caption = "Source: London Datastore | Plot: Johnny Lillis (github.com/j-lillis)") +
  theme_void() +
  theme(panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(2,20,4,10),"mm"),
        axis.text.y = element_text(color = grey, family = font, face = "bold"),
        axis.ticks.length.y = unit(9, "mm"),
        strip.text = element_text(size = 14, margin = margin(8,0,8,0), 
                                  color = grey, family = font, face = "bold"),
        plot.background   = element_rect(colour = "white", size = 3, fill = "#F5F5F5"),
        plot.caption = element_text(colour = alpha(grey, 0.5), size = 8, hjust = 1, family = font),
        plot.caption.position = "plot") +
  coord_cartesian(clip = "off")


label_x <- 0.34
label_y <- 0.9
ggdraw(plot) + draw_label("London Fire Brigade animal rescues", x = label_x, y = label_y, 
                          colour = red, size = 28.2,
                          fontfamily  = font, fontface = "bold", hjust = 0) +
  draw_label(str_wrap("The London Fire Brigade performs a range of services not related to fires, including rescuing animals who have become trapped or distressed.
                      Each plot shows the total number of rescues in each year from 2009 to 2020 for the five most rescued animals.", 86),
            x = label_x + 0.0025, y = label_y - 0.1, fontfamily  = font, lineheight = 1.01, hjust = 0, size = 12) +
  
  draw_label("2009", x = 0.104, y = 0.15, size = 10, fontfamily = font, fontface = "bold", colour = alpha(grey, 0.6)) +
  draw_label("2020", x = 0.224, y = 0.15, size = 10, fontfamily = font, fontface = "bold", colour = alpha(grey, 0.6))
  
#save to temp plots file
#ggsave(filename =  paste0("temp-", format(Sys.time(), "%Y%m%d"), ".png"), 
#       device = NULL, path = here::here("Temp plots"),
#       dpi = 320, width = 10, height = 5)

ggsave(filename = "animal_resuces_plot.png",
       path = here::here("Animal Rescues (29-06-2021)"),
       dpi = 320, width = 10, height = 5)

