# TidyTuesday on a Wednesday in the Sparks Lab 

# Setup ####

# Get the Data
  # Read in with tidytuesdayR package 
  # This loads the readme and all the data sets for the week of interest
  # Either ISO-8601 date or year/week works!
  # install.packages("tidytuesdayR")
# tuesdata <- tidytuesdayR::tt_load('2023-06-20') # this works as well
tuesdata <- tidytuesdayR::tt_load(2023, week = 25)

# Pulling out the data from the tt_data into more manageable data frames
ufo_sightings <- tuesdata$`ufo_sightings` # Actual sightings data
places <- tuesdata$`places` # Metadata for locations in the sightings data frame
day_parts_map <- tuesdata$`day_parts_map` # Times for specific astrological day parts at specific places

# Load required packages
library(tidyverse)
library(usmap)
library(stringr)
library(showtext)
library(maps)
library(gganimate)
library(patchwork)

# Figure 1 - exploring the colors of unidentified lights in the sky ####

# Adding special font for figure 1 (X files esque font)
font_add_google("Special Elite", family = "special")
showtext_auto()

# Pulling out only USA data, then grouping and summarizing by state
state_sightings <- ufo_sightings %>% filter(country_code=="US") %>% 
  group_by(state) %>% summarise(n=n())

# List of colors to check for in the data
color_list <- c("red","orange","yellow","green","blue","purple","white","black","pink","maroon")

# Pulling out data that have some description of "lights" in the sky in the summary column, and adding a
  # color column with a placeholder (ph) value 
lights <- ufo_sightings[grep("light",ufo_sightings$summary),] %>% 
  mutate(color="ph")

# For loop that changes the placeholder value to the color of the light described. This is done iteratively 
  # over the colors in "color_list" and where there are multiple colors, they are separated by a comma. ph is then removed
for (i in 1:length(color_list)){
    lights$color[grep(color_list[i],lights$summary)] <- str_c(lights$color[grep(color_list[i],lights$summary)], 
                                                              color_list[i], sep=", ") %>% str_remove("ph, ")
  }
  
# Removing rows that have no color, and then separating those that had multiple colors into distinct rows.
lights <- lights %>% filter(color!="ph") %>% 
  separate_rows(color,sep=", ")

# Filtering data for a few select colors with >1000 observations
lights_short <- lights %>% filter(color %in% c((lights %>% 
                                                group_by(color) %>% 
                                                summarise(n=n()) %>% 
                                                filter(n>1000))$color), 
                                  country_code=="US")

# Summarizing number of sightings over the USA by color, and setting factor levels
lights_summary <- lights_short %>% 
  group_by(color) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(color=factor(color,levels=c("red","orange","yellow","green","blue","white")))

# Summarizing number of color observations by state and adding column with fraction of total within state
lights_summary_state <- lights_short %>% 
  group_by(state,color) %>% 
  summarise(n=n()) %>% 
  mutate(frac=n/sum(n),color=factor(color,levels=c("red","orange","yellow","green","blue","white"))) %>% 
  ungroup()

# Summarizing number of sightings over the USA by color, for each year in the data set
lights_summary_year <- lights_short %>% 
  mutate(year = year(reported_date_time)) %>% 
  group_by(year,color) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(color=factor(color,levels=c("red","orange","yellow","green","blue","white")))

# Presetting a main title for figure 1
color_title <- expression(paste(italic('"I was suddenly aware of most of the sky being filled with a white light"')))

# Setting up a small dataframe with parameters for labeling colors in figure 1. This was trial and error to get the y's right
color_labs <- tribble(
  ~"color",~"y",~"x",
  "red", 0.85, "WY",
  "orange",0.625, "WY",
  "yellow",0.55, "WY",
  "green",0.475, "WY",
  "blue",0.35, "WY",
  "white",0.2, "WY"
) %>% mutate(color=factor(color,levels=c("red","orange","yellow","green","blue","white")))

# Creating a vector of colors to be used for the plots
plot_cols <- c("#DB163F","#DB710B","#DBCD00","#00DC23","#16B1DB","#EBE9E6")

# Stacked bar chart to be positioned in the top panel of the full figure
top <- ggplot(lights_summary_state)+
  geom_bar(aes(x=state,y=frac,fill=color ), 
           position="stack", 
           stat="identity")+
  geom_text(data = color_labs,
            aes(y=y,x=x,color=color,label=color),
            nudge_x = 0.5,
            hjust=0,
            family ="special")+
  scale_fill_manual(values = plot_cols)+
  scale_color_manual(values = plot_cols)+
  scale_x_discrete(expand=expansion(add = c(1, 3.3)))+
  scale_y_continuous(expand=c(0,0))+
  labs(title = color_title,
       subtitle="The relative prevalence of the most common colored lights from unidentified sources seen in the sky in the USA, by State")+
  theme_void()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,
                                   color="white", 
                                   size = 10),
        panel.background = element_rect(fill="black"),
      panel.grid=element_blank(),
      plot.background = element_rect(fill="black",
                                     color=NULL),
      text = element_text(color = "white", 
                          family ="special", 
                          size=7),
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size=10),
      plot.caption = element_text(size = 10))

# Pie chart to be place in the bottom left panel of the full figure
botL <- ggplot(lights_summary,aes(x="",y=n,fill=color))+ 
  geom_bar(stat="identity", 
           width=1) +
  coord_polar("y", start=0)+
  scale_fill_manual(values = plot_cols)+
  labs(subtitle=str_wrap("The relative prevalence of the most common colored lights seen in the sky above the USA",45))+
  theme_void()+
  theme(legend.position = "none",
        panel.background = element_rect(fill="black"),
        panel.grid=element_blank(),
        plot.background = element_rect(fill="black"),
        text = element_text(color = "white", 
                            family ="special", 
                            size=7),
        axis.line = element_line(color="black"),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size=10,
                                     hjust=0.5),
        plot.caption = element_text(size = 10))

# Stacked area chart to be the bottom panel of the full figure
botR <- ggplot(lights_summary_year)+
  geom_text(aes(x = 2014, y=2250,label="2014"),
            color = "#EBE9E6")+
  geom_area(aes(x=year,y=n,fill=color ))+
  scale_fill_manual(values = plot_cols)+
  scale_y_continuous(expand = expansion(add = c(0, 100)), 
                     position="right")+
  scale_x_continuous(expand=c(0,0))+
  labs(x=NULL,
       y=NULL,
       subtitle=str_wrap("The number of sightings of the most common colored lights seen in the sky above the USA by year",60),
       caption = "NOTE: The two lower panels colors follow the order listed to the right of the top panel, with the pie chart going counter-clockwise from the top, and the area chart from top to bottom")+
  theme_void()+
  theme(legend.position = "none",
        axis.text.x = element_text(color="white", 
                                   size = 10),
        axis.text.y = element_text(color="white", 
                                   size = 10),
        panel.background = element_rect(fill="black",
                                        color=NULL),
        panel.grid=element_blank(),
        plot.background = element_rect(fill="black",
                                       color=NULL),
        text = element_text(color = "white", 
                            family ="special", 
                            size=7),
        axis.line = element_line(color="white"),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size=10,
                                     hjust=1),
        plot.caption = element_text(size = 8,
                                    hjust=0.5))

# Creating the bottom panel combining botR and botL by setting botR as an inset figure to fill up empty space
  # This takes advantage of the patchwork package and the inset_element function
com <- botR+inset_element(botL,
                          left=0,
                          bottom =0.15,
                          right=0.5,
                          top=1,
                          align_to = "full")

# Combining top and bottom panels using patchwork syntax
top/com


# Figure 2 - a quick gif illustrating UFO sightings in a given city, at different parts of the day ####

# Vector of "day parts" in chronological order
day_parts <- c("astronomical dawn","nautical dawn","civil dawn","morning","afternoon",
  "civil dusk","nautical dusk","astronomical dusk","night")

# Creating a color palette for the gif
pal_synth9 <- colorRampPalette( c("#F2059F","#8B2A8C","#EBE9E6","#024873","#0FBF9F"))(9)

# Changing the factor levels of day_part column to the order of the vector above
ufo_sightings <- ufo_sightings %>% 
  mutate(day_part=factor(day_part,
                         levels=day_parts))

# Summary of number of observations per city in a given "day part"
city_dp_summary <- ufo_sightings %>% 
  filter(country_code=="US") %>% 
  group_by(city,state,day_part) %>% 
  summarise(n=n()) %>% 
  mutate(city=str_to_sentence(city)) %>%  #Converting all city names to sentence case (e.g. Ithaca, rather than ITHACA or ithaca)
  drop_na(day_part) %>% 
  ungroup()

# Joining with the places data frame to get lat/long for each city
city_dp_summary <- places %>% 
  filter(country_code=="US") %>% 
  inner_join(city_dp_summary,
             by=c("city","state"),
             multiple="all")

# Converting lat/long into the correct projection for usmaps package plotting
cdp2 <- usmap_transform(
  city_dp_summary,
  input_names = c("longitude", "latitude"),
  output_names = c("x", "y")
)

# Setting a main title for the gif
color_title2 <- '"<i>This was in the city, I caught a movement over my shoulder and saw a very low black object going north. It appeared to be floating."</i>'

# Plotting a gif that shows total occurrences in a given city during a given day part
a <- plot_usmap(regions="state", color="white",fill="black")+
  geom_point(data=cdp2,
             aes(x=x,
                 y=y,
                 size=log10(n),
                 color=day_part))+
  scale_color_manual(values=pal_synth9)+
  guides(color="none",
         size=guide_legend(override.aes=list(color="white")))+
  labs(title = str_wrap('"This was in the city, I caught a movement over my shoulder and saw a very low black object going north. It appeared to be floating."'),
    caption="{closest_state}",
    subtitle= str_wrap("Total number of UFO sightings in US cities during different parts of the day. Point size scales to the log10 of number of sightings."),
    size=str_wrap("Sightings, log10 scale",11))+
  theme_void()+
  theme(panel.background = element_rect(fill="black"),
        plot.background = element_rect(fill="black"),
        text = element_text(color = "white"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
       plot.caption = element_text(size = 14)
        )+
  transition_states(states=day_part, 
                    transition_length = 3, 
                    state_length = 3, 
                    wrap = TRUE)+
  enter_grow()+
  exit_shrink()

# Setting plot size constant
S <- 5 

# Animating the gif
ufo_gif <- animate(a, 
                   height = S, 
                   width = S*1.8, 
                   units = "in", 
                   res = 200)

# Outputting the gif
ufo_gif 
