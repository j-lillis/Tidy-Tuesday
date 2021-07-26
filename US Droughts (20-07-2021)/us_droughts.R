library(tidyverse)
library(geofacet)
library(lubridate)
library(patchwork)

drought_import <- readr::read_csv(here::here('US Droughts (20-07-2021)', 'droughts_data.csv')) %>% 
  janitor::clean_names() %>% 
  #pivot_longer(cols = c(none:d4), names_to = 'drought_lvl', values_to = 'area_pct') %>% 
  rename(state_abb = state_abbreviation)


View(drought_import)

fill_scale <- #viridis::inferno(15)[c(6, 8, 10, 12, 14)]
  #heat.colors(5)
  RColorBrewer::brewer.pal(6, 'YlOrRd')[c(2:6)]





# think this works now, but need to work out exactly what we're plotting...

geo_facets <- drought_import %>% mutate(year = year(valid_start)) %>% 
  pivot_longer(cols = c(none:d4), names_to = 'drought_lvl', values_to = 'area_pct') %>%
  
  mutate(drought_yes = if_else(area_pct > 10, 1, 0)) %>% 
  mutate(drought_lvl_num = case_when(drought_lvl == "none" ~ 0,
                                     drought_lvl == "d0" ~ 1,
                                     drought_lvl == "d1" ~ 2,
                                     drought_lvl == "d2" ~ 3,
                                     drought_lvl == "d3" ~ 4,
                                     drought_lvl == "d4" ~ 5)) %>% 
  group_by(state_abb, map_date) %>% 
  filter(drought_lvl_num != 0) %>% 
  arrange(state_abb, map_date, desc(drought_lvl_num)) %>% 
  mutate(cumulative_area_pct = cumsum(area_pct)) %>% 
  mutate(cumulative_drought_yes = if_else(cumulative_area_pct > 10, 1, 0)) %>% 
  filter(cumulative_drought_yes == 1) %>% 
  mutate(final_area = if_else(drought_lvl_num == max(drought_lvl_num) & cumulative_drought_yes == 1, cumulative_area_pct, area_pct)) %>%
  
  filter(drought_yes == 1, drought_lvl_num != 0) %>% 

group_by(state_abb, valid_start) %>% 
  slice_max(drought_lvl_num) %>% ungroup() %>% group_by(state_abb, year, drought_lvl_num) %>% summarise(total = n()) %>%

  filter(year > 2010) %>% 
  ggplot(aes(x = year, y = total, fill = fct_rev(factor(drought_lvl_num)))) +
  geom_col(width = 1, size = 0.2, color = "white") +
  facet_geo(~ state_abb, strip.position = "bottom") +
  scale_fill_manual(values = rev(fill_scale)) + 
  scale_y_continuous(limits = c(0,52), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1.5, "lines"))


national_droughts <- read_csv(here::here('US Droughts (20-07-2021)', 'national_droughts_data.csv')) %>% janitor::clean_names() %>% 
  pivot_longer(cols = c(none:d4), names_to = 'drought_lvl', values_to = 'area_pct')

area_plot <- national_droughts %>% filter(drought_lvl != "none") %>% 
  ggplot(aes(x = valid_start, y = area_pct, fill = fct_rev(factor(drought_lvl)))) +
  geom_area() +
  scale_fill_manual(values = rev(fill_scale))  +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(legend.position = "none")

layout <- "
A
A
A
A
A
#
B"

geo_facets / area_plot + plot_layout(design = layout)


national_droughts %>% 
  mutate(year = year(valid_start)) %>% 
  filter(drought_lvl != "none", year == 2019) %>% 
  ggplot(aes(x = valid_start, y = area_pct, fill = fct_rev(factor(drought_lvl)))) +
  geom_area() +
  scale_fill_manual(values = rev(fill_scale)) + coord_polar()

