library(tidyverse)
library(urbnmapr)
library(sf)
library(patchwork)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv') %>% 
  filter(established > 200) %>% 
  drop_na(latitude)

View(post_offices)

us_map <- get_urbn_map(sf = F) %>% filter(!state_abbv %in% c("AK", "HI"))

us_map <- map_data("state")
post_offices %>% 
filter(latitude > 23 & latitude < 50) %>% map_plot

 map_plot <- function(.data){
   .data %>% ggplot() + geom_polygon( data = us_map, aes(x= long, y = lat, group = group), fill = "#D3D3D3", 
                              colour = "#E8E8E8",
                         size = 0.2) + 
  theme(panel.background  = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
     geom_point(aes(x = longitude, y = latitude), size = 0.1) + 
     coord_fixed(1.25)
 }

 
 
(est_to_1800 <- post_offices %>% 
   filter(latitude > 23 & latitude < 50) %>%
  filter(established < 1800) %>% 
  map_plot)
 
 (est_to_1825 <- post_offices %>% 
     filter(latitude > 23 & latitude < 50) %>%
     filter(established < 1825) %>% 
     map_plot)
 
 (est_to_1850 <- post_offices %>% 
     filter(latitude > 23 & latitude < 50) %>%
     filter(established < 1850) %>% 
     map_plot)

 (est_to_1875 <- post_offices %>% 
     filter(latitude > 23 & latitude < 50) %>%
     filter(established < 1875) %>% 
     map_plot)
 
 (est_to_1900 <- post_offices %>% 
     filter(latitude > 23 & latitude < 50) %>%
     filter(established < 1900) %>% 
     map_plot)
 
(est_to_1800 | est_to_1825 | est_to_1850) / (est_to_1875 | est_to_1900 | est_to_1925)
# rewrite above with sf

us_map <- get_urbn_map(sf = T) %>% filter(!state_abbv %in% c("AK", "HI")) %>% st_union() 

post_offices_sf <- post_offices %>% st_as_sf(coords = c("longitude", "latitude")) 

st_crs(post_offices_sf) <- 2163

us_map %>% ggplot() + geom_sf(fill = "#D3D3D3", 
                                          colour = "#E8E8E8",
                                          size = 0.2) + 
geom_sf()

post_offices_sf %>% ggplot() + geom_sf()


  post_offices_sf %>% ggplot() + geom_sf()
theme(panel.background  = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_point(aes(x = longitude, y = latitude), size = 0.1) + coord_fixed(1.25)




post_offices %>% ggplot(aes(x = longitude, y = latitude)) + geom_point()

post_offices %>% count(established) %>% ggplot(aes(x = established, y = n)) + geom_col()
