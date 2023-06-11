#######################################
# Practice Mapping using ggplot2
#
# Katherine Schaughency
# 6 Jun 2023
# ----------------------------------- #
# Reference: 
# https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html
#######################################

# --------------------------------- #
# load R package

library(tidyverse)
library(ggplot2)

# --------------------------------- #
# create grid info for the world map

# Read in the csv file. You can download the CSV file from the aforementioned reference line.
world.map <- read.csv("/Users/katherineschaughency/Desktop/worldtilegrid.csv") %>% 
  
  # keep the variables that I need for the map
  select("name","alpha.3","region","x","y") %>% 

  # add Taiwan to the map
  rbind(c("Taiwan","TWN","Asia","27","8")) %>% 
  
  # change the lat/lon from character to integer
  mutate(x = as.integer(x)) %>% 
  mutate(y = as.integer(y)) %>% 

  # mark the places that I have been
  mutate(been=case_when(name %in% c("Austria",
                                    "Bangladesh",
                                    "Belgium",
                                    "Canada",
                                    "China",
                                    "Ethiopia",
                                    "France",
                                    "Germany",
                                    "Iceland",
                                    "India",
                                    "Italy",
                                    "Japan",
                                    "Luxembourg",
                                    "South Korea",
                                    "Nepal",
                                    "Netherlands",
                                    "New Zealand",
                                    "Singapore",
                                    "Switzerland",
                                    "Thailand",
                                    "Great Britain and Northern Ireland",
                                    "United States of America",
                                    "Taiwan") ~ "YES",
                        TRUE ~ "NO")) %>% 
  
  # combine continent and county labels as one variable (for coloring purpose)
  mutate(country.been = paste(region, been))
  
 
# check data
View(world.map)
names(world.map)
dim(world.map)
str(world.map)
levels(as.factor(world.map$country.been))

# --------------------------------- #
# plot

# read data and set basic parameters
ggplot(world.map, 
       aes(xmin = x, 
           ymin = y, 
           xmax = x + 1, 
           ymax = y + 1, 
           fill = country.been)) +

  # draw white rectangular
  geom_rect(color = "#ffffff") +

  # have a minimalist theme
  theme_minimal() + 

  # specify the theme
  #    don't draw grid lines
  #    have no axis text and title;
  #    legend is on the right
  #    have a light grey dotted line, 0.5 size, around the legend box
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "right",
        legend.background = element_rect(linetype = 2, size = 0.5, colour = "light grey")
        ) +

  # specify the text in the grid map
  geom_text(aes(x = x, y = y, label = alpha.3), 
            color = "#000000", alpha = 0.7, nudge_x = 0.5, nudge_y = -0.5, size = 3) + 

  # reserve y. if y is not reversed, southern hemisphere will be on top.
  scale_y_reverse() + 

  # specify the legend name, color, and label
  scale_fill_manual(breaks = c("Africa NO",
                                 "Americas NO",
                                 "Asia NO",
                                 "Europe NO",
                                 "Oceania NO",
                                 "Antarctica NO",
                                 "Africa YES",
                                 "Americas YES",
                                 "Asia YES",
                                 "Europe YES",
                                 "Oceania YES"),
                      labels = c("Not Yet Visited Countries in Africa",
                                 "Not Yet Visited Countries in Americas",
                                 "Not Yet Visited Countries in Asia",
                                 "Not Yet Visited Countries in Europe",
                                 "Not Yet Visited Countries in Oceania",
                                 "Antarctica: Not Yet Visited",
                                 "Visited Countries in Africa",
                                 "Visited Countries in Americas",
                                 "Visited Countries in Asia",
                                 "Visited Countries in Europe",
                                 "Visited Countries in Oceania"),
                      values = c("#E6E6FA",
                                 "#AFE1AF",
                                 "#FFFF8F",
                                 "#F8C8DC",
                                 "#ADD8E6",
                                 "#A9A6A7",
                                 "#7F3C8D",
                                 "#11A579",
                                 "#F2B701",
                                 "#E73F74",
                                 "#3969AC")) +

  # specify each grid's aspect ratio (since I am using the three letter country name, I thought 0.75 would fit the text the best)
  coord_equal(ratio = 0.75) +

  # specify the legend title
  # align categories in two columns
  guides(fill=guide_legend(title="Not Yet Visited and Visited Countries",
                           ncol=2))



