

# TO DO:

# - fix area plot dates and x axis text,
# - move area plot title down slightly
# - annotations



library(tidyverse)
library(geofacet)
library(lubridate)
library(patchwork)
library(extrafont)
library(ggtext)

state_lookup <- tibble::tribble(~state_name, ~state_abb,
                                "ALABAMA",  "AL",
                                "ALASKA", "AK",
                                "AMERICAN SAMOA", "AS",
                                "ARIZONA", "AZ",
                                "ARKANSAS", "AR",
                                "CALIFORNIA", "CA",
                                "COLORADO", "CO",
                                "CONNECTICUT", "CT",
                                "DELAWARE", "DE",
                                "DISTRICT OF COLUMBIA", "DC",
                                "FLORIDA", "FL",
                                "GEORGIA", "GA",
                                "GUAM", "GU",
                                "HAWAII", "HI",
                                "IDAHO", "ID",
                                "ILLINOIS", "IL",
                                "INDIANA", "IN",
                                "IOWA", "IA",
                                "KANSAS", "KS",
                                "KENTUCKY", "KY",
                                "LOUISIANA", "LA",
                                "MAINE", "ME",
                                "MARYLAND", "MD",
                                "MASSACHUSETTS", "MA",
                                "MICHIGAN", "MI",
                                "MINNESOTA", "MN",
                                "MISSISSIPPI", "MS",
                                "MISSOURI", "MO",
                                "MONTANA", "MT",
                                "NEBRASKA", "NE",
                                "NEVADA", "NV",
                                "NEW HAMPSHIRE", "NH",
                                "NEW JERSEY", "NJ",
                                "NEW MEXICO", "NM",
                                "NEW YORK", "NY",
                                "NORTH CAROLINA", "NC",
                                "NORTH DAKOTA", "ND",
                                "NORTHERN MARIANA IS", "MP",
                                "OHIO", "OH",
                                "OKLAHOMA", "OK",
                                "OREGON", "OR",
                                "PENNSYLVANIA", "PA",
                                "PUERTO RICO", "PR",
                                "RHODE ISLAND", "RI",
                                "SOUTH CAROLINA", "SC",
                                "SOUTH DAKOTA", "SD",
                                "TENNESSEE", "TN",
                                "TEXAS", "TX",
                                "UTAH", "UT",
                                "VERMONT", "VT",
                                "VIRGINIA", "VA",
                                "VIRGIN ISLANDS", "VI",
                                "WASHINGTON", "WA",
                                "WEST VIRGINIA", "WV",
                                "WISCONSIN", "WI",
                                "WYOMING", "WY"
) %>% 
  mutate(state_name = str_to_title(state_name)) %>% 
  mutate(state_name = case_when(state_name == "District Of Columbia" ~ "D.C.",
                                state_name == "New Hampshire" ~ "N. Hampshire",
                                TRUE ~ state_name))

us_grid <- us_state_grid2 %>% mutate(name = case_when(code == "DC" ~ "D.C.",
                                                      code == "NH" ~ "N. Hampshire",
                                                      TRUE ~ name))

drought_import <- readr::read_csv(here::here('US Droughts (20-07-2021)', 'droughts_data.csv')) %>% 
  janitor::clean_names() %>% 
  #pivot_longer(cols = c(none:d4), names_to = 'drought_lvl', values_to = 'area_pct') %>% 
  rename(state_abb = state_abbreviation) %>% 
  mutate(year = year(valid_start)) %>% 
  left_join(., state_lookup) 



View(drought_import)

# plot setup
font_reg <- "Roboto Slab"
font_light <- "Roboto Slab Light"
font_bold <- "Roboto Slab Medium"

grey <- "#717169"
light_grey <- "#91918a"

fill_scale <- #viridis::inferno(15)[c(6, 8, 10, 12, 14)]
  #heat.colors(5)
  RColorBrewer::brewer.pal(6, 'YlOrRd')[c(2:6)]


drought_labels <- tribble(
  ~drought_lvl, ~drought_desc,
  "d0", "Abnormally dry",
  "d1", "Moderate drought",
  "d2", "Severe drought",
  "d3", "Extreme drought",
  "d4", "Exceptional drought"
)


geo_facets <- drought_import %>% 
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
  
  filter(year > 2009) %>%   left_join(., state_lookup) %>% 
  ggplot(aes(x = year, y = total, fill = fct_rev(factor(drought_lvl_num)))) +
  geom_col(width = 1, size = 0.2, color = "white") +
  
  facet_geo(~ state_name, strip.position = "bottom", grid = us_grid) +
  
  scale_fill_manual(values = rev(fill_scale), labels = rev(drought_labels$drought_desc),
                    name = NULL) + 
  scale_y_continuous(limits = c(0,52), expand = c(0,0), breaks = c(0,52)) +
  scale_x_continuous(expand = c(0,0)) +
  
  labs(subtitle = "Bars show number of weeks where each drought\ncategory covered at least 10% of the state's\nland area for every year from 2011 to 2020:") +
  theme_void() +
  theme(legend.position = "top",
        #legend.key = element_rect(colour = light_grey, size = 1),
        legend.key.width = unit(50, "mm"),
        legend.key.height = unit(4, "mm"),
        legend.margin = margin(c(-100,90,30,0)),
        legend.text = element_markdown(family = font_reg, size = 16, colour = light_grey),
        legend.title = element_blank(),
        legend.spacing.x = unit(3, "lines"),
        strip.text = element_text(family = font_reg, size = 14, colour = grey, margin = margin(2,0,2,0)),
        panel.spacing = unit(1.2, "lines"),
        plot.subtitle = element_text(colour = grey, family = font_light, #face = "bold",
                                     size = 22, hjust = 0.5, vjust = -16,
                                     #margin = margin(-15,0,20,-20))
                                     )) + 
  guides(fill = guide_legend(label.position = "top", nrow = 1,
                             reverse = T
  )) +
  coord_cartesian(clip = "off")


national_droughts <- read_csv(here::here('US Droughts (20-07-2021)', 'national_droughts_data_1.csv')) %>% janitor::clean_names() %>% 
  pivot_longer(cols = c(none:d4), names_to = 'drought_lvl', values_to = 'area_pct') %>% 
  mutate(year = year(valid_start)) %>% 
  
  group_by(year) %>% arrange(valid_start) %>% 
  mutate(year_start = if_else(row_number() == 1, 1, NA_real_),
         year_start_break = if_else(year_start == 1, paste0(year_start, "_", year), NA_character_)) %>% 
  ungroup() 




# filter so that starts at 2011
area_plot <- 
  
  national_droughts %>% 
  filter(drought_lvl != "none", valid_start >= as_date("2010-01-01"), valid_start <= as_date("2020-12-31")) %>% 
  mutate(valid_start = case_when(valid_start == "2010-01-05" ~ as_date("2010-01-01"),
                                 valid_start == "2020-12-29" ~ as_date("2020-12-31"),
                                 TRUE ~ valid_start)) %>% 
  ggplot(aes(x = valid_start, y = area_pct/100, fill = fct_rev(factor(drought_lvl)))) +
  geom_area() +
  scale_fill_manual(values = rev(fill_scale)) + 
  scale_x_date(
    breaks = seq(as_date("2010-01-01"), as_date("2020-12-31"), by = "1 year"),
               #limits = as.Date(c("2010-01-01", "2020-12-31")),
               date_labels = "%Y",
    expand = c(0.001,0.001)
               ) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent) +
  #theme_void() +
  labs(subtitle = "Percentage of entire USA land area affected:", x = NULL, y = NULL) +
  theme(legend.position = "none", 
        axis.line.x  = element_line(colour = light_grey, size = 0.75),
        axis.line.y = element_blank(),
        panel.background = element_rect(fill = NA),
        axis.text.x = element_text(colour = light_grey, hjust = -1.2, vjust = 2, family = font_bold, face = "bold", size = 16),
        #axis.title.y = element_text(colour = light_grey, family = font_bold, face = "bold", size = 16),
        axis.text.y = element_text(colour = light_grey, family = font_reg, face = "bold", size = 16),
        axis.ticks.x = element_line(color = c(alpha(light_grey, 0), rep(light_grey, 10)), size = 0.75),
        axis.ticks.y  = element_blank(),
        axis.ticks.length = unit(0.25, "cm"),
        panel.grid.major.y = element_line(colour = alpha(light_grey, 0.4)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = grey, family = font_light, #face = "bold", 
                                     size = 22, hjust = 0.5, vjust = 0,
                                     margin = margin(-15,0,20,0))
        #panel.ontop = TRUE
  ) +
  coord_cartesian(clip = "off", xlim = c(as_date("2010-01-05"), as_date("2020-12-29")))


layout <- "
A
A
A
A
A
A
A
B
B"

plot <- geo_facets / area_plot + plot_layout(design = layout) +
  plot_annotation(title = "Drought in the USA — 2011 to 2020",
                  caption = "Plot created by Johnny Lillis | github.com/j-lillis | twitter.com/johnny_c_lillis | Data from US Drought Monitor | #TidyTuesday") & 
  theme(plot.title = element_text(colour = grey, family = font_reg, face = "bold", size = 48, hjust = 0.5,
                                  margin = margin(0,0,-5,0)),
        
        plot.caption = element_text(colour = grey, family = font_light, size = 14, hjust = 0.5),
        plot.margin = unit(c(5,8,5,2), "mm"))



ggsave(filename = "us_droughts_export.png",
       plot = plot, path = here::here("US Droughts (20-07-2021)"),
       width = 20, height = 16)

# save to temp plots file
ggsave(filename =  paste0("temp-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), 
       plot = plot,
       device = NULL, path = here::here("Temp plots"),
       dpi = 320, width = 20, height = 16)


