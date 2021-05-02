
#### rework with maps as facet - create period col in master data then facet on that - should
#### solve issues with alignment

library(tidyverse)
library(urbnmapr)
library(sf)
library(patchwork)
library(ggtext)

post_offices_import <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv') %>% 
  filter(established > 200) %>% 
  drop_na(latitude)

post_offices <- post_offices_import  %>% 
  mutate(period = 
           case_when(established <= 1800 ~ "1800",
                     established <= 1825 ~ "1825",
                     established <= 1850 ~ "1850",
                     established <= 1875 ~ "1875",
                     established <= 1900 ~ "1900",
                     established <= 1925 ~ "1925")) %>% 
  filter(established <= 1925) %>% 
  filter(latitude > 23 & latitude < 50)
  
discontinued_counts <- post_offices %>%
  count(discontinued)

post_office_year_counts <- post_offices %>% count(established) %>% 
  inner_join(., discontinued, by = c("established" = "discontinued" )) %>% 
  rename("established" = "n.x",
         "discontinued" = "n.y",
         "year" = "established") %>% 
  mutate(decade = year - year %% 10) %>% 
  mutate(period = 
           case_when(year <= 1800 ~ "1800",
                     year <= 1825 ~ "1825",
                     year <= 1850 ~ "1850",
                     year <= 1875 ~ "1875",
                     year <= 1900 ~ "1900",
                     year <= 1925 ~ "1925")) %>% 
  mutate(discontinued = discontinued * -1) %>% 
  pivot_longer(cols = c(established, discontinued))
  

bar_1800 <- post_office_year_counts %>% filter(period == "1800") %>% 
  ggplot(aes(x = period, y = value, fill = name)) + geom_col() +
  theme(panel.background  = element_rect(fill = background),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = usps_red, face = "bold", size = 20),
        panel.grid.major.x = element_line(color = "grey"),
        panel.grid.major.y = element_blank(),
        #plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position = "none") +
  scale_fill_manual(values = c(usps_red, usps_blue)) + coord_flip() + ylim(c(-3000,3000))
  
  
View(post_offices)

us_map <- get_urbn_map(sf = F) %>% filter(!state_abbv %in% c("AK", "HI"))

us_map <- map_data("state")
post_offices %>% 
  filter(latitude > 23 & latitude < 50) %>% map_plot

usps_blue <- "#004B87"
usps_red <- "#DA291C"
grey_accent <- "#808080"
background <- "#f0efe4"

theme_update(plot.background = element_rect(fill = background),
             plot.margin = margin(rep(5,4)))

####### INDIVIDUAL MAPS 
map_plot <- function(.data, data_title){
  .data %>% ggplot() + 
    geom_polygon( data = us_map, aes(x= long, y = lat, group = group), fill = "#D3D3D3", 
                  colour = "#E8E8E8",
                  size = 0.2) + 
    theme(panel.background  = element_rect(fill = background),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color = usps_red, face = "bold", size = 20),
          panel.grid = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    geom_point(aes(x = longitude, y = latitude), size = 0.5, alpha = 0.4, color = usps_blue) +
    annotate(geom = "text", x = centre_lon, y = centre_lat, label = paste0(data_title),
             size = 14, fontface = "bold", colour = grey_accent, alpha = 0.85)
}


centre_lon = -100
centre_lat = 40

post_offices %>% map_plot("") + facet_wrap(~period)

(est_to_1800 <- post_offices %>% 
    filter(latitude > 23 & latitude < 50) %>%
    filter(established < 1800 & discontinued > 1800) %>% 
    map_plot("1800"))

est_to_1800 / bar_1800 + plot_layout(heights = c(15,1))

(est_to_1825 <- post_offices %>% 
    filter(latitude > 23 & latitude < 50) %>%
    filter(established <= 1825 & discontinued > 1825) %>% 
    map_plot("1825"))


(est_to_1850 <- post_offices %>% 
    filter(latitude > 23 & latitude < 50) %>%
    filter(established <= 1850 & discontinued > 1850) %>% 
    map_plot("1850"))

(est_to_1875 <- post_offices %>% 
    filter(latitude > 23 & latitude < 50) %>%
    filter(established <= 1875 & discontinued > 1875) %>% 
    map_plot("1875"))

(est_to_1900 <- post_offices %>% 
    filter(latitude > 23 & latitude < 50) %>%
    filter(established <= 1900 & discontinued > 1900) %>% 
    map_plot("1900"))

(est_to_1925 <- post_offices %>% 
    filter(latitude > 23 & latitude < 50) %>%
    filter(established <= 1925 & discontinued > 1925) %>% 
    map_plot("1925"))



######### TOP GRAPH

discontinued <- post_offices %>%
  count(discontinued)

(bar_plot <- post_office_year_counts %>% 
    filter(year >= 1800 & year <= 1925) %>% 
    ggplot(aes(x = period, y = value)) + 
    geom_col(aes(fill = name)) +
    theme(panel.background  = element_rect(fill = background),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color = usps_red, face = "bold", size = 20),
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.major.x = element_blank(),
          #plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.position = "none") +
    scale_fill_manual(values = c(usps_red, usps_blue)))
#### TEXT BOX
text_box <- ggplot(tibble(x = 1,
                          y = 1,
                          label = "here is some test text")) +
  geom_textbox(
    aes(x, y, label = label),
    width = unit(35.5, "lines"),
    lineheight = 1.3,
    hjust = 0,
    box.colour = NA
  ) +
  theme( panel.background  = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
         plot.title = element_text(color = usps_red, face = "bold", size = 20),
         
         plot.margin=grid::unit(c(0,0,0,0), "mm"))

###### PLOT LAYOUT
layout <- "
ABC
DEF
GGH"

(est_to_1800 | est_to_1825 | est_to_1850) /
  (est_to_1875 | est_to_1900 | est_to_1925) /
  (text_box | bar_plot) + plot_layout(widths = c(1,1,1))


(est_to_1800 + est_to_1825 + est_to_1850 + 
  est_to_1875 + est_to_1900 + est_to_1925 + 
  text_box + bar_plot + plot_layout(design = layout, widths = c(1,1,1)))
