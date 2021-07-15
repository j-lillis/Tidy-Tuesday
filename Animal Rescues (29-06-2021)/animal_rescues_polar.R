

polar_plot <- animal_rescues_import %>% 
  mutate(month = month(date_time_of_call)) %>% 
  mutate(animal_group_parent = fct_lump(factor(animal_group_parent), 5)) %>% 
  filter(cal_year < 2021, animal_group_parent != "Other")  %>% 
  group_by(animal_group_parent, month) %>% 
  summarise(total = n()) %>% 
  mutate(percentage = total / sum(total)) %>% 
  ggplot(aes(x = month, y = percentage)) + 
  geom_hline(yintercept = seq(0, 0.16, by = 0.03), colour = alpha(grey, 0.2), size = 0.5) + 
  geom_line(colour = red, size = 1) + 
  facet_wrap(~ fct_relevel(animal_group_parent, "Cats", "Birds", "Dogs", "Foxes", "Horses"), nrow = 1) +
  coord_polar(clip = "off") + 
  ylim(c(0,0.17)) +
  scale_x_continuous(breaks = c(3.75,6.5,9.25,12), labels = c("Spring", "Summer", "Autumn", "Winter")) +
  scale_fill_discrete() +
  theme_void() +
  theme(plot.margin = margin(85,15,85,15)) +
  theme(axis.text.x = element_text(family = font, colour = alpha(grey, 0.5), angle = c(-90,0,90,0)),
        #panel.spacing = unit(1, "lines"),
        axis.ticks.length.y = unit(9, "mm"),
        strip.text = element_text(size = 14, margin = margin(8,0,8,0), 
                                  color = grey, family = font, face = "bold"),
        plot.background   = element_rect(colour = red, size = 3, fill = background),
        plot.caption = element_text(colour = alpha(grey, 0.5), size = 8, hjust = 1, family = font),
        plot.caption.position = "plot")

label_x <- 0.34
label_y <- 0.9

polar_plot <- ggdraw(polar_plot) + draw_label("London Fire Brigade animal rescues", x = label_x, y = label_y, 
                                              colour = red, size = 28.2,
                                              fontfamily  = font, fontface = "bold", hjust = 0) +
  draw_label(str_wrap("The London Fire Brigade performs a range of services not related to fires, including rescuing animals who have become trapped or distressed.
                      Each bar plot shows the total number of rescues in each year from 2009 to 2020 for the five most rescued animals.", 86),
             x = label_x + 0.0025, y = label_y - 0.1, fontfamily  = font, lineheight = 1.01, hjust = 0, size = 12, colour = "black") +
  
  #draw_label(str_wrap("The plot to the right shows total number of rescues for the period by month.", 50),
  #           x = label_x + 0.2, y = label_y - 0.2, fontfamily  = font, lineheight = 1.01, hjust = 0, size = 12, colour = "black") +
  
  draw_label("2009", x = 0.104, y = 0.15, size = 10, fontfamily = font, fontface = "bold", colour = alpha(grey, 0.6)) +
  draw_label("2020", x = 0.224, y = 0.15, size = 10, fontfamily = font, fontface = "bold", colour = alpha(grey, 0.6)) +
  NULL



ggsave(filename = "animal_resuces_plot_polar.png", plot = polar_plot,
       path = here::here("Animal Rescues (29-06-2021)"),
       dpi = 320, width = 10, height = 5)