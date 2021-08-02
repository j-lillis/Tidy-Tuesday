Tidy Tuesday
================
These are plots I have created as part of the [#TidyTuesday](https://github.com/rfordatascience/tidytuesday) project. I have used each plot as an opportunity to explore different visualisation techniques, design principles, and ggplot2 functionality. Unless otherwise stated, plots are created entirely in R with no use of image editing software. Click a plot to view a full resolution version, and view underlying R code in the respective folders at the top of this page.

## US Droughts (20-07-2021)

For this plot I used the `{geofacet}` package - it's useful for showing change over time at the state level while still illustrating the geographical relationship between states. I exported the plot from R as an SVG file and did final touches in Inkscape, including adding axis lines and annotation. While there are benefits to keeping the visualisation process entirely in R, sometimes it is more efficient to manually add in objects and move things around. 

![US Droughts](https://raw.githubusercontent.com/j-lillis/Tidy-Tuesday/main/US%20Droughts%20(20-07-2021)/us_droughts_plot.png)


## London Fire Brigade animal rescues (29-06-2021)

My previous two plots had been quite "busy", so I wanted to create a clean and simple visualisation which made the most of the striking LFB red.

![London Fire Brigade - animal rescues](https://raw.githubusercontent.com/j-lillis/Tidy-Tuesday/main/Animal%20Rescues%20(29-06-2021)/animal_rescues_plot.png)


## Ask A Manager Survey (18-05-2021)

I like using dumbbell plots for clearly comparing averages between two groups, but also wanted to show salary distribution. Inspired by raincloud plots, I combined the two. The legend took a lot of work to get right, but I wanted to make sure the plots could be easily interpreted by anyone.

![Ask A Manager Survey](https://raw.githubusercontent.com/j-lillis/Tidy-Tuesday/main/Ask%20A%20Manager%20survey%20(18-05-2021)/ask_a_manager_survey_plot.png)


## US Post Offices (13-04-2021)

This was my first time using the `{patchwork}` package to assemble a more complex plot. Rather than using facets for the six map plots, I created a custom function to assemble the map, red/blue bar, and annotations for start and end year. I then used `purrr` to iterate the function over the time periods I had defined. I'm really happy with how the bottom plot turned out - I originally alternated colours of the states to improve legibility, but I think it also adds aesthetic variation.

![US Post Offices](https://raw.githubusercontent.com/j-lillis/Tidy-Tuesday/main/US%20Post%20Offices%20(13-04-2021)/post_offices_plot.png)


## Steam (16-03-2021)

My first TidyTuesday plot. I wanted to evoke a retro gaming aesthetic through my choice of fonts and colours, and it was a good excuse to try out the `{ggridges}` package.

![Steam plot](https://raw.githubusercontent.com/j-lillis/Tidy-Tuesday/main/Steam%20(16-03-2021)/steam_plot.png)


