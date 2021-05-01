library(tidyverse)
library(urbnmapr)
library(sf)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv') %>% 
  filter(established > 200) %>% 
  filter(latitude < 50 & longitude > -150 & longitude  < 0)

View(post_offices)


us_map <- map_data("usa")

us_map <- get_urbn_map(sf = F) %>% filter(!state_abbv %in% c("AK", "HI"))

post_offices %>% ggplot() + geom_polygon( data = us_map, aes(x= long, y = lat), fill = "#D3D3D3", 
                              colour = "#E8E8E8",
                         size = 0.2) + 
  theme(panel.background  = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_point(aes(x = longitude, y = latitude))

post_offices %>% ggplot(aes(x = longitude, y = latitude)) + geom_point()

post_offices %>% count(established) %>% ggplot(aes(x = established, y = n)) + geom_col()
