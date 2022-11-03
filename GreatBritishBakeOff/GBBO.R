# TidyTuesday Great British Bakeoff
library(tidyverse)
library(bakeoff)
library(fontawesome)
library(emojifont)


# Flowers ####
## Pulling out instances where bakers used floral extracts in challenges
test <- challenges[unique(sort(c(grep("lavender|rose|lilac|violet",challenges$signature , ignore.case = TRUE),
  grep("lavender|rose|lilac|violet",challenges$showstopper , ignore.case = TRUE)))),]

## Removing Rosemary from test
test2 <- test[-sort(c(grep("rosemary",test$signature , ignore.case = TRUE),
grep("rosemary",test$showstopper , ignore.case = TRUE))),]
## Bit of data wrangling
tt <- length(test2$result)
tc <- as.numeric(challenges %>% drop_na(result) %>%  summarise(length(result)))

a <- test2 %>% drop_na(result) %>% group_by(result) %>% summarise(n = n(), per = (n/tt)*100) %>% 
  mutate(Floral = "Flower Extract(s) Used", label = 'fa-star' ) %>% 
  mutate(per = if_else(result == "OUT", per*-1,per))
b <- challenges %>% filter(result %in% c("IN","OUT","STAR BAKER")) %>% drop_na(result) %>% group_by(result) %>% 
  summarise(n = n(), per = (n/tc)*100) %>% 
  mutate(Floral = "No Flower Extract(s)", label = "fa-arrow-circle-o-left" )%>% 
  mutate(per = if_else(result == "OUT", per*-1,per))

Flowers <- rbind(a,b) %>% filter(result!="IN")

pal <- bakeoff_palette("showstopper")

## The plot
FlowerPlot <- ggplot(Flowers)+
  labs(x = element_blank(),
       y = "Percent of Bakers (%)",
       title = "Using the other types of 'Flowers' in the GBBO",
       subtitle = str_wrap("Baking with flower extracts is a tricky enterprise on the Great British Bake Off. Bakers who used rose, violet, lilac, or lavender extracts in their Signature or Showstopper bakes (n=32) were almost twice as likely to win Star Baker that week (Star), but were also almost three times as likely to go home (arrow), boom or bust! Flowers were used in both challenges only once, in series three, by James who won Star Baker that week.", 80),
       fill = "Result",
       caption = "@benuveges")+
  geom_hline(yintercept = 0, color = pal[7])+
  geom_bar(aes(x=Floral, fill = result, y=per ),
           stat="identity")+
geom_fontawesome(
  alias = c("fa-star","fa-arrow-circle-o-left",
            "fa-star","fa-arrow-circle-o-left"),
  color = c(pal[8],pal[3] ,pal[8] ,pal[3]),
  size = 8,
  x = c(1,1,
        2,2),
  y = c(max(Flowers$per)-3.5,min(Flowers$per)+4,
        7.5,-8.5))+
  scale_x_discrete(position = "top")+
  scale_fill_bakeoff()+
    theme_minimal()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = pal[10], color = NA),
        axis.line.y.left = element_line(color=pal[7]),
        text = element_text(color = "black"),
        plot.background = element_rect(fill = pal[10]),
        panel.grid = element_blank(),
        axis.line.x.bottom  = element_blank(),
        panel.border=element_blank(),
        plot.title= element_text(size = 10),
        plot.subtitle= element_text(size = 7))

png( "FlowerBaking.png", res = 1080, height = 4, width = 4.1, units = "in")
FlowerPlot
dev.off()

# BreadWeek ####
## Data Frame specifying breadweeks
BreadWeek <- data.frame(series =  c(1:10), episode = c(3,3,2,2,3,3,3,3,3,3))
## Data wrangling
BreadWeek <- left_join(BreadWeek, 
                       challenges %>% filter(result == "STAR BAKER"), 
                        by = c("series","episode")) %>% 
  select(series,episode,baker,result,technical)

BreadWeek <- left_join(BreadWeek, 
                       bakers,
                       by = c("series","baker")) %>% 
  select(series,baker,series_winner,percent_episodes_appeared, percent_technical_top3 ,occupation)

## The plot
Bread <- ggplot(BreadWeek %>% drop_na(baker) %>% arrange(series))+
  labs(y=element_blank(),
       x = "Percentage Of Episodes Appeared In (%)",
       title = "The Importance of 'Bread Week' On The GBBO",
       subtitle = str_wrap("Bread week is regarded as one of the toughest days in the tent. The bakers need to pull out all of the stops to impress the 'King of Bread'. And it's a big deal if they do. The baker that earns Star Baker during bread week typically appears in above the median percentage of episodes (- - -), has gone on to the final (100%) half of the time, and has gone on to win it all twice (Crown). (Note, no 'Star Baker' title in S1)", 75),
       caption = "@benuveges")+
  geom_bar(aes(y = baker,
               x = percent_episodes_appeared, 
               fill = baker), 
           stat = "identity")+
  geom_emoji(alias = "crown",
             color = pal[8],
             y = c("John","Rahul"), x = 95,
             size=7)+
  geom_vline(xintercept = median(bakers$percent_episodes_appeared), linetype = 'dashed')+
  scale_fill_bakeoff("signature", guide="none")+
  scale_x_continuous (limits = c(0,100), expand = expand_scale(mult = c(0, 0), 
                                                               add = c(0, 2)))+
  theme_minimal()+
    theme(plot.background = element_rect(fill = "#efc689"),
          panel.grid = element_blank(),
          text = element_text(color = "black"),
          axis.text =  element_text(color = "black"),
          axis.line.y.left = element_line(color="black"),
          plot.title= element_text(size = 10),
          plot.subtitle= element_text(size = 7))
Bread

png( "BreadWeek.png", res = 1080, height = 4, width = 4.1, units = "in")
Bread
dev.off()
