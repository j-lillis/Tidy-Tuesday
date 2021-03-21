library(tidyverse)
library(ggridges)
library(lubridate)
library(extrafont)

# font for title, game names, and x axis labels from https://www.fontspace.com/press-start-2p-font-f11591

# colours from Tomorrow Night Eighties theme https://github.com/chriskempson/tomorrow-theme
background = "#2d2d2d" 
current_line = "#393939"
selection = "#515151" 
foreground = "#cccccc" 
comment = "#999999" 
red = "#f2777a" 
orange = "#f99157" 
yellow = "#ffcc66" 
green = "#99cc99" 
aqua = "#66cccc" 
blue = "#6699cc" 
purple = "#cc99cc" 

# import
games_import <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv') %>%
  mutate(year = as.numeric(year),
         month = factor(month, levels = month.name),
         date = make_date(year, month)) %>% 
  mutate(month_short = month.abb[month])

# clean
games <- games_import %>% group_by(gamename, year) %>% 
  mutate(year_total_avg = sum(avg)) %>% 
  mutate(month_as_pct_of_year = avg/year_total_avg) %>% 
  arrange(desc(year_total_avg)) %>% filter(year == 2020) %>% ungroup()

# identify top 10 games by total across year
top_games <- games %>% group_by(gamename) %>%  summarise(year_total_avg) %>% arrange(desc(year_total_avg)) %>% slice(n=1) %>% 
  arrange(desc(year_total_avg)) %>% ungroup() %>% slice_head(n=10) %>% pull(gamename)

# plot
(plot <- games %>% 
    filter(gamename %in% top_games) %>% 
    
    # create ranking variable
    group_by(gamename) %>% 
    mutate(rank = dense_rank(desc(avg))) %>% 
    mutate(max_month = month[rank == 1]) %>% 
    mutate(max_month_num = match(max_month, month.name)) %>%
    ungroup() %>% 
    mutate(gamename = fct_reorder(gamename, desc(max_month_num), max)) %>% 
    
    # ggplot
    ggplot(aes(x = date, y = gamename, 
               height = month_as_pct_of_year,
               group = gamename, 
               fill = gamename
    )) +
    
    # this was not used in the end - highlights Mar-May to show impact of Covid-19 lockdowns
    #geom_rect(xmin = as.numeric(games$date[10]), xmax = as.numeric(games$date[8]), 
    #          ymin = 0, ymax = 11, size = 1.5, 
    #          fill = comment, 
    #          alpha = 0.1,
    #          linetype = 2) +
    
    geom_ridgeline(stat = "identity", scale = 8.5,
                   #fill = "#66cccc",
                   color = purple, size = 1,
                   alpha = 0.55) +
    
    geom_text(aes(label=ifelse(month=="June",paste0(gamename),"")), 
              position = position_nudge( x = 13, y = 0.3), 
              #nudge_x = -15, nudge_y =  0.3,
              color = yellow, family = "Press Start 2P", size = 3.5, hjust = 0.5) +
    
    labs(title = "DISTRIBUTION OF AVERAGE USERS PER \nMONTH FOR TOP 10 MOST POPULAR GAMES\nON STEAM IN 2020",
         subtitle = "For each game I calculated the percentage of total 2020 users that occurred\nin each month, to show the change in popularity across the year. Games are\nranked based on peak month - those which peaked earlier in 2020 are higher\nand those which peaked later are lower.",
         x = NULL, y = NULL,
         caption = "\nCreated by @johnny_c_lillis\n") +
    scale_x_date(date_breaks  = "1 months",
                 date_labels  = "%b") +
    
    scale_fill_cyclical(values = c(green, aqua)) +
    
    scale_y_discrete(expand = c(0,0.2)) +
    
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background  = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = background),
          axis.text.x = element_text(color = purple, family = "Press Start 2P", size = 10),
          plot.title = element_text(color = red, family = "Press Start 2P", lineheight = 1.2, size = 16, hjust = 0.5),
          plot.title.position = "panel",
          plot.subtitle  = element_text(color = orange, family = "OCR A Std", size = 10.5, lineheight = 1.1, margin = margin(7,0,12,0), hjust = 0.5),
          plot.caption = element_text(family = "OCR A Std", size = 10.5, colour = orange)) +    
    NULL
)

png("plot_v1.png", res = 300*2,  width =2500*2, height = 2500*2)
print(plot)
dev.off()

