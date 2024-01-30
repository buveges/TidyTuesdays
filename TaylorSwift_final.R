#Load in the data
#tuesdata <- tidytuesdayR::tt_load(2023, week = 42)

#taylor_album_songs <- tuesdata$taylor_album_songs
#taylor_all_songs <- tuesdata$taylor_all_songs
#taylor_albums <- tuesdata$taylor_albums

# Load packages
library(tidyverse)
library(taylor)
library(tayloRswift)
library(showtext)

# Data import and set up ####

# Adding a fancy font
font_add_google("IM Fell DW Pica", family = "special")
showtext_auto()

# Creating a quick operator "not in"
`%ni%`<- negate(`%in%`)

# Extract data
df_as <- taylor::taylor_album_songs
df_alb <- taylor::taylor_albums%>% 
  mutate(user10=user_score*10)
df_all <- taylor::taylor_all_songs 

# Pulling out album metacritic scores
ab_meta <- df_alb %>%
  mutate(year=year(album_release)) %>%  # creating a column of year from release_date
  arrange(year) %>% ungroup() %>%  # Arranging the data chronologically
  filter(album_name %ni% c("Beautiful Eyes", 	
                           "The Taylor Swift Holiday Collection")) # removing two albums with no scores

# Vector of album names in chronological order; used to set factor order later
ab_ord2 <- as.vector(ab_meta$album_name) 

# Vecotr of palett names in taylor package, set in chronological order
pal_ord2 <- c("taylor_swift","fearless","speak_now","red","1989",
              "reputation", "lover", "evermore","folklore", "fearless_tv", "red_tv",
              "midnights", "1989_tv", "speak_now_tv")

# Converting album names in ab_meta to a factor with levels defined by ab_ord2
ab_meta <- ab_meta %>% 
  mutate(album_name=factor(album_name,
                           levels=ab_ord2))

# Subsetting df_all into new dataframe of only relevant columns
sng_meta <- df_all %>% 
  select(album_name,album_release,track_number,
         track_name) %>% 
  mutate(album_name=factor(album_name,
                           levels=ab_ord2)) %>% 
  filter(album_name %ni% c("Beautiful Eyes", 	
                           "The Taylor Swift Holiday Collection"))

# Joining sng_meta and ab_meta on album name to get metacritic score associated with songs
sng_meta <- left_join(sng_meta,ab_meta) %>% 
  arrange(year)

# Separate vector of album names from sng_meta dataframe
aN <- unique(as.character(sng_meta$album_name))

# Creating a summary dataframe with number of songs in each album
n_songs <- sng_meta %>%  group_by(album_name) %>%  summarise(n_songs = n()) %>% 
  mutate(cs=cumsum(n_songs)) %>% drop_na()

# Palette and circles generation ####

# Creating a giant palette composed of all the album specific palettes. number of colors scaled to number of songs in album
big_pal <- c()

## palette proceeds from 1 to n in set order
for(i in 1:length(n_songs$album_name)){
  
  temp <- colorRampPalette( colors = unlist(album_palettes[pal_ord2[i]],use.names = FALSE))(n_songs$n_songs[i])
  
  big_pal <- c(big_pal,temp)
}

## palette takes random assemblage of the colors in each of the palettes
big_pal2 <- c()

for(i in 1:length(n_songs$album_name)){
  
  temp <-  sample(x=colorRampPalette( colors = unlist(album_palettes[pal_ord2[i]],use.names = FALSE))(n_songs$n_songs[i]),
                  size=n_songs$n_songs[i],
                  replace=FALSE)
  
  big_pal2 <- c(big_pal2,temp)
}


# Creating a function to generate circles. Number of circles generated corresponds to number of songs in the album
  # Radii of circles is scaled to number of songs, with outermost and inner most circles at fixed radii.
TayCircles <- function(df,val,class,site,f_levs=NULL,radii=NULL,ctrs=NULL,n=100){
  if (is.null(radii)){
    #radii <- seq(1:length(unique(df$class)))
    radii <- seq(from=5,to=0.5,length.out = length(df[[class]]-2))
  }
  if (is.null(ctrs)){
    #ctrs <- radii-max(radii)
    ctrs <- rep(mean(df[[val]],na.rm=TRUE),length(df[[class]]-2))
  }
  te <- seq(0, 2*pi, length.out=n)
  df2 <- data.frame(site=df[[site]],
                    class=df[[class]],
                    val=df[[val]])
  if (is.null(f_levs)){
    f_levs <- seq(1:length(unique(df2$class)))
  }
  
  sumV <- df2 %>% group_by(site,class) %>% 
    summarise(val=mean(val)) %>% 
    ungroup() %>% 
    mutate(class=factor(class,levels=f_levs),
           i = as.numeric(factor(class, levels = f_levs)))
  
  cir <- data.frame(x=c(),y=c(),val=c(),class=c(),site=c())
  
  for(j in 1:length(sumV$site)){
    tmp <- data.frame(x = 0+radii[sumV$i[j]]*cos(te), 
                      y = ctrs[sumV$i[j]]+radii[sumV$i[j]]*sin(te), 
                      val = sumV$val[j],
                      class=sumV$class[j],
                      site=sumV$site[j])
    cir <- rbind(cir,tmp)
  }
  cir <- cir %>% mutate(class=factor(class,levels=rev(f_levs)))
  return(cir)
}

# Creating dummy data frame.
Circ3 <- data.frame(x=NA,y=NA,val=NA,class=NA,site=NA)

# Running a for loop where TayCircles is applied to each album
for(i in 1:length(aN)){
  temp <- TayCircles(df=sng_meta %>% filter(album_name==aN[[i]]),
                     val="metacritic_score", 
                     class="track_number", 
                     site="album_name")
  
  Circ3 <- rbind(Circ3,temp)
}

# Cleaning up Circ3
Circ3 <- Circ3 %>% drop_na() %>% 
  mutate(class=as.numeric(class),
         site=factor(site,levels=ab_ord2)) %>% 
  rename(album_name=site,track_number=class) %>% 
  arrange(album_name)

# Arranging sng_meta by album names, in chronological order
sng_meta <- sng_meta %>% 
  arrange(album_name)%>% 
  drop_na(album_name) 

# Adding a column to sng_meta that is a running tally of songs. Basically creating a unique identifier for each song,
  # when arranged in defined album order
sng_meta <- sng_meta %>% 
  mutate(sng_n=seq(1:length(sng_meta$track_number)))

# Small df from sng_meta
SM <- sng_meta %>% 
  select(album_name,track_number,sng_n)

# Creating a new df by joining Circ3 and SM
Circ4 <- left_join(Circ3, SM)

# Setting factor levels of each song
Circ4 <- Circ4 %>% 
  mutate(sng_n=factor(sng_n, levels = seq(1:283)))

# Creating a "label" frame to be used in geom_text call while plotting
meta_text <- Circ4 %>% group_by(album_name) %>% summarise(x=mean(x), y = mean(y)) %>% 
  #mutate(Nudge = c(4,3,3.5,2,2,3.5,2.5,3,3,-7,-6,3,-7,-6))
  mutate(Nudge = c(9,8,9,7,7,8,7,7,8,-13,-12,8,-13,-12))


# Plotting everything up ####
ggplot(Circ4)+
  geom_polygon(aes(x=x,y=y,fill= sng_n, group=track_number),
               show.legend = FALSE,
               color="white",
               linewidth=0.05 )+
  geom_text(data = meta_text, aes(x=x,
                                  y=y+Nudge,
                                  label=album_name),
            angle=90,family="special")+
  scale_fill_manual(values=big_pal2)+
  facet_grid(~album_name, labeller = label_wrap_gen(multi_line = TRUE))+
  scale_y_continuous(labels = function(x){paste(x, "-")},    # simulate tick marks for left axis
                     sec.axis = dup_axis(breaks = 0)) +      # add right axis
  annotate(geom = 'segment', 
           y = Inf, 
           yend = Inf, 
           color = 'black', 
           x = -Inf, 
           xend = Inf, 
           size = 1)+
  labs(y= "Album Metacritic Score",
       title = "Taylor Swift's 'Meta-oric' Rise",
       caption = "General upward trend in Metacritic scores of Taylor Swifts albums (center ring). Albums arranged by release date, and number of rings corresponds to number of songs on the album. Color schemes correspond to album color palettes in taylor package.")+
  theme_classic()+
  theme(
    text = element_text(family="special"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    strip.text.x.top = element_blank(),
    strip.background = element_blank(),
    axis.title.y.right = element_blank(),                # hide right axis title
    axis.text.y.right = element_blank(),                 # hide right axis labels
    axis.ticks.y = element_blank(),                      # hide left/right axis ticks
    axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
    panel.spacing = unit(0, "mm"),                       # remove spacing between facets
    plot.background = element_rect(fill="grey85")
  )