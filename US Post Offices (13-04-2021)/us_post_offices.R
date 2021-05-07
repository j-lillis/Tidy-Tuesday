library(tidyverse)
library(sf)
library(patchwork)
library(ggtext)
library(extrafont)
library(here)

#### DATA SETUP

post_offices_import <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv') %>% 
  filter(established > 200) 

post_offices <- post_offices_import  %>% 
  mutate(period = 
           case_when(established <= 1800 ~ "1800",
                     established <= 1825 ~ "1825",
                     established <= 1850 ~ "1850",
                     established <= 1875 ~ "1875",
                     established <= 1900 ~ "1900",
                     established <= 1925 ~ "1925")) 

discontinued_counts <- post_offices %>%
  count(discontinued)

# the following code and approach based on https://github.com/henrywrover/tidytuesday/blob/master/R/post%20offices.R

post_office_year_counts <- post_offices %>% count(established) %>% 
  inner_join(., discontinued_counts, by = c("established" = "discontinued" )) %>% 
  rename("established" = "n.x",
         "discontinued" = "n.y",
         "year" = "established") %>% 
  mutate(period = 
           case_when(year <= 1800 ~ "1800",
                     year <= 1825 ~ "1825",
                     year <= 1850 ~ "1850",
                     year <= 1875 ~ "1875",
                     year <= 1900 ~ "1900",
                     year <= 1925 ~ "1925")) %>% 
  mutate(discontinued = discontinued * -1) %>% 
  pivot_longer(cols = c(established, discontinued))

post_offices_list <- list(post_offices, post_office_year_counts)


##### PLOT SETUP  

us_map <- map_data("state")

usps_blue <- "#004B87"
usps_red <- "#DA291C"
grey_accent <- "#808080"
grey_light <- "#D3D3D3"
grey_med <- "#C0C0C0"
grey_med_dark <- "#989898"
background <- "#f0efe4"

font <- "Raleway-Medium"
font_bold <- "Raleway-SemiBold"

theme_update(plot.background = element_rect(fill = background, color = NA),
             plot.margin = margin(rep(5,4)))


####### FUNCTION TO GENERATE INDIVIDUAL MAPS AND BAR PLOTS 

plot_period <- function(.data, period_to_plot){
  
  centre_lon = -115
  centre_lat = 26.5
  
  min_period <- .data[[1]] %>% filter(period == period_to_plot) %>% 
    summarise(min(established)) %>% pull()
  
  map_plot <- .data[[1]] %>% 
    drop_na(latitude) %>% 
    filter(established <= 1925) %>% 
    filter(latitude > 23 & latitude < 50) %>% 
    filter(period == period_to_plot) %>% 
    ggplot() + 
    geom_polygon( data = us_map, aes(x= long, y = lat, group = group), fill = grey_light, 
                  colour = "#E8E8E8",
                  size = 0.2) + 
    theme(panel.background  = element_rect(fill = background),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
    ) +
    geom_point(aes(x = longitude, y = latitude), size = 0.5, alpha = 0.4, color = usps_blue) +
    coord_fixed(1.25, clip = "off") +
    annotate(geom = "text", x = centre_lon, y = centre_lat, label = period_to_plot, family = font_bold,
             size = 12, fontface = "bold", colour = grey_med, alpha = 0.8) +
    annotate(geom = "text", x = centre_lon-8, y = centre_lat+4.5, label = paste0(min_period, "-"), family = font_bold,
             size = 8, fontface = "bold", colour = grey_med, alpha = 0.7)
  
  bar_plot <- .data[[2]] %>% filter(period == period_to_plot) %>% 
    ggplot(aes(x = 0, y = value, fill = name)) + geom_col(width = 1) +
    theme(panel.background  = element_rect(fill = background),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          #plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.position = "none") +
    geom_segment(aes(x = -0.5, xend = 0.5, y = 0, yend = 0), color = grey_med_dark) +
    scale_fill_manual(values = c(usps_red, usps_blue)) + 
    
    coord_flip(clip = "off") + ylim(c(-55000,71000))
  
  return(map_plot / bar_plot + plot_layout(heights = c(15,1)))
}

periods <- c("1800", "1825", "1850", "1875", "1900", "1925")

period_plots <- periods %>% map(plot_period, .data = post_offices_list)

###### GENERATE CHART OF FIRST POST OFFICE BY EACH STATE

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
  mutate(state_name = str_to_lower(state_name))

state_established <- post_offices %>% 
  mutate(state = factor(state)) %>% 
  group_by(state) %>% slice_min(established) %>%
  slice(n=1) %>% arrange(established) %>% select(state, established) %>% 
  inner_join(., state_lookup, by = c("state" = "state_abb"))

state_timeline <- state_established %>% 
    group_by(established) %>% 
    mutate(rank = dense_rank(state_name)) %>% 
    ungroup() %>% 
    mutate(colour_code = ifelse(row_number() %% 2 == 0,1,0)) %>% 
    ggplot(aes(y = established, x = reorder(state, established), label = state)) +
    geom_text(aes(colour = factor(colour_code)), size = 4, family = "Raleway-ExtraBold") +
    theme(axis.title = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = background),
          panel.background = element_rect(fill = background),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(color = grey_light, size = 0.5),
          panel.grid.major.y = element_line(color = grey_light, size = 0.5),
          axis.text.y = element_text(family = font, color = grey_med_dark, size = 14),
          plot.subtitle  = element_text(family = font, color = grey_accent, size = 25,
                                    margin = margin(20,0,0,0)),
          plot.title.position = "plot",
          legend.position = "none"
    ) +
    
    scale_color_manual(values = c(usps_blue, usps_red)) +
    scale_y_reverse(breaks = seq(1650, 1900, 50), minor_breaks = seq(1650, 1900, 25),
                    sec.axis = sec_axis(~., breaks = seq(1650, 1900, 50))) +
    labs(subtitle   = "Year the first post office was established in each state") 


####### FINAL ASSEMBLY OF PLOTS

period_plots_merged <- (period_plots[[1]] | period_plots[[2]] | period_plots[[3]]) / 
  (period_plots[[4]] | period_plots[[5]] | period_plots[[6]])

(period_plots_merged / state_timeline) + 
  plot_annotation(title = "A history of post offices in the United States",
                  subtitle = "Dots show every post office established by the end of each period<br>Bars show the number of post offices <span style = 'color:#004B87;'>established</span> and <span style = 'color:#DA291C;'>discontinued</span> within each period",
                  caption = "Plot created by Johnny Lillis | github.com/j-lillis | twitter.com/johnny_c_lillis | Data from Cameron Blevins & Richard W. Helbock") & 
  theme(plot.title  = element_text(family = font_bold, size = 48, color = grey_accent, margin = margin(10,40,7,20)),
        plot.subtitle  = element_markdown(family = font_bold, size = 22, color = grey_med_dark, lineheight = 1.3),
        plot.caption = element_text(family = font, size = 10, color = grey_med_dark),
        panel.background  = element_rect(fill = background, color = NA))

ggsave(filename =  paste0("temp-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), 
       device = NULL, path = here("Temp plots"),
       dpi = 320, width = 15, height = 10)

        