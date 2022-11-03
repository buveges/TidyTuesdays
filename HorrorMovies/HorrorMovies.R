# TidyTuesday Horror Movies - Skeleton Plot
library(tidyverse)
library(lubridate)
library(showtext)

##REad in data
horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')
## Add numeric columns for Month and Year of release
horror_movies <-  horror_movies |> mutate(Month = month(release_date),Year=year(release_date))
## Calculations
test <- horror_movies |> 
  group_by(Year, Month) |> 
  summarise(n=n(), 
            tot.rev = sum(revenue), # sum of revenue each month by year
            max.pop = max(popularity)) |>  # maximum popularity score each month) 
  ungroup() |> 
  group_by(Year) |> 
  mutate(sum = sum(n),  # Total number of movies each year
         prop = n/sum) |>  # proportion of movies released in each month for a given year
  filter(Year >2010& Year<2022) |> ungroup() # Filtering for only years 2011-2021
## Data for the "eyes" of the plot
eyes <- test |> group_by(Year) |> 
  summarise(min.rev = min(tot.rev),
            min.rev.month = Month[tot.rev == min(tot.rev)],
            max.rev = max(tot.rev),
            max.rev.month = Month[tot.rev == max(tot.rev)])
## Binding relevant data from test to the eyes df
test2 <- test |> group_by(Year) |> 
  filter(tot.rev==max(tot.rev))
eyes <- left_join(eyes,test2,by = "Year")
## Filtering for the top revenue movies for each year
tops <- horror_movies |> group_by(Year) |> 
  filter(Year >2010& Year<2022) |> 
  filter(revenue == max(revenue)) |> 
  select(Year,title) |> 
  ungroup() |> 
  arrange(Year) |> 
  mutate(u.d = c(1,-1,1,-1,1,-1,1,-1,1,-1,1)) # Column used to place title above or below the skeleton for a given year
tops <- left_join(tops,eyes,by="Year")
## Data frames for the inset legend skeleton and labels
insetT <- test |> filter(Year == 2012) |> mutate(sum = 2200)
insetE <- data.frame(Year = 2012, sum = 2200, min.rev.month =3, max.rev.month = 10)
insetText <- data.frame(x = c(2011,2013.5, 2011.25, 2013, 2015),
                        y = c(2500,2500,1750, 1800, 2200),
                        curvature = c(0.5,1,0.5,0.5,0.5),
                        xend = c(2011.6,2012.4,2011.6,2012.3,2012.6),
                        yend = c(2250,2255,2075,2045,2200),
                        labels = c("Min. Revenue Month", "Max. Revenue Month", "Monthly Prop. of Yr. Total","October", "Total Number of Movies Each Year"))
eyesize <- 2.5
textsize <- 3
font_add_google("Jolly Lodger", family = "special")
showtext_auto()

## Constructing the plot                    
H <- ggplot()+
  geom_segment(data = insetText, aes(x = x,xend=xend, y = y,yend=yend), color = "white", arrow = arrow(length = unit(0.03, "npc")), alpha = 0.6)+
  geom_label(data = insetText, aes(x = x, y = y, label = labels), color = "white", fill = "black", family ="special", size = textsize-0.5)+
  geom_crossbar(data=insetT,aes(x=Year, fill = factor(Month),y = sum, ymax = sum, ymin = sum-1000*prop ), 
                position = "dodge", stat = "identity",alpha = 0.7)+
  geom_point(data=insetE,aes(x=Year+((max.rev.month-6.5)*(1/13)), y = sum+50),
             color='white', shape = 16, size = eyesize,alpha = 0.7)+
  geom_point(data=insetE,aes(x=Year+((min.rev.month-6.5)*(1/13)),y = sum+50),
             color='white', shape = 1, size = eyesize,alpha = 0.7)+
  geom_crossbar(data=test,aes(x=Year, fill = factor(Month),y = sum, ymax = sum, ymin = sum-1000*prop ), 
                position = "dodge", stat = "identity")+
  geom_point(data=eyes,aes(x=Year+((max.rev.month-6.5)*(1/13)), y = sum+50),
             color='white', shape = 16, size = eyesize)+
  geom_point(data=eyes,aes(x=Year+((min.rev.month-6.5)*(1/13)), y = sum+50),
             color='white', shape = 1, size = eyesize)+
  geom_text(data = tops,aes(x=Year, y = sum+(300*-u.d), label = title), 
            color = "white", hjust = "center", family ="special", size = textsize)+
  scale_fill_manual(values = c("white","white","white","white","white","white","white","white","white","orange","white","white" ),
                    guide = FALSE)+
  scale_x_continuous(breaks = unique(test$Year))+
  labs(title = "Horror Movie 'Skeleton' Plot",
       y = "Yearly Number of Movies Made",
       x = "Year",
       caption = "@benuveges",
       subtitle = "Listed movie had the highest revenue for associated year.")+
  theme(panel.background = element_rect(fill="black"),
        panel.grid=element_blank(),
        plot.background = element_rect(fill="black"),
        text = element_text(color = "white", family ="special", size=7),
        axis.title = element_text(size=13),
        axis.text = element_text(color="white", size = 7),
        axis.line = element_line(color="white"),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size = 10))
  
png( "TheHorror.png", res = 720, height = 4, width = 6, units = "in")
H
dev.off()
