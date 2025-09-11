# Pokémon TidyTuesday on a Wednesday 

## Load packages ####
library(tidyverse)
library(ggimage)
library(showtext)

## Load in data ####
tuesdata <- tidytuesdayR::tt_load(2025, week = 13)
pokemon_df <- tuesdata$pokemon_df

# Add https to url_icon links
pokemon_df <- pokemon_df %>% mutate(icon_new = paste0("https:",url_icon ))

# Add font
font_add_google("Yusei Magic", family = "special")
showtext_auto()

# Add color palette based on types
type_colors <-  tribble(
~"type",~"color",
"normal", '#A8A77A',
"fire", '#EE8130',
"water", '#6390F0',
"electric", '#F7D02C',
"grass", '#7AC74C',
"ice", '#96D9D6',
"fighting", '#C22E28',
"poison", '#A33EA1',
"ground", '#E2BF65',
"flying", '#A98FF3',
"psychic", '#F95587',
"bug", '#A6B91A',
"rock", '#B6A136',
"ghost", '#735797',
"dragon", '#6F35FC',
"dark", '#705746',
"steel", '#B7B7CE',
"fairy", '#D685AD'
) %>% mutate(t=1)

# Arranging by type
type_colors <- type_colors %>% 
  arrange(type)

## Data wrangling ####

# Summarizing median value of each stat grouped by primary type (type_1)
stats <- pokemon_df %>% 
  group_by(type_1) %>% 
  summarise(hp=median(hp),
            atk=median(attack),
            def=median(defense),
            spatk=median(special_attack),
            spdef=median(special_defense),
            spd=median(speed)
  )

# Converting stats dataframe to long format
stats_long <- stats %>% 
  pivot_longer(-type_1,
               names_to = "stat",
               values_to = "value")

# Calculating which stat has the highest and lowest median values for each type
max_min_stats <- stats_long %>% 
  group_by(type_1) %>% 
  summarise(max = max(value),
            min= min(value),
            stat = stat[which(value==max)]) %>% 
  mutate(max_col = "max")

# Joining with stats_long dataframe
stats_long <- left_join(stats_long,
                        max_min_stats,
                        by=c("type_1","stat"))

# Dataframe of only type_1 and stat
stat_select <- max_min_stats %>%
  select(type_1, stat)

# Converting main pokemon data frame into long format compatible with stat_long
p_long <- pokemon_df %>%
  drop_na(url_icon) %>%
  select(pokemon,
         type_1,
         hp,
         attack,
         defense,
         special_attack,
         special_defense,
         speed,
         icon_new) %>%
  rename(
    hp = hp,
    atk = attack,
    def = defense,
    spatk = special_attack,
    spdef = special_defense,
    spd = speed
  ) %>%
  pivot_longer(-c(pokemon, type_1, icon_new), 
               names_to = "stat")

# Pulling out the pokemon that has the highest value in the stat with the highest median value of each type
# i.e., the highest median stat for the rock type is defense, so this pulls out the pokemon that has the highest
# base defense stat value (Regirock at 200).
p_max_stat <- left_join(stat_select, p_long) %>%
  group_by(type_1) %>%
  summarise(max = max(value),
            pokemon = pokemon[which(value == max(value))],
            stat = unique(stat)) %>%
  slice_head() %>%
  ungroup()

# Adding icon column to p_max stat
p_max_stat <- left_join(p_max_stat, pokemon_df %>% 
                          select(pokemon, icon_new), by = "pokemon")

# Adding an y placement column to allow for adjusting geom_text placement in final figure
p_max_stat <- p_max_stat %>%
  mutate(y = c(45, 60, 70, 60, 60, 60,
               60, 80, 55, 50, 60, 52,
               55, 50, 60, 70, 80, 50))

## Creating the plot ####
poke_plot <- ggplot(stats_long) +
  geom_col(aes(
    x = stat,
    y = value,
    fill = type_1,
    color = max_col
  ),
  linewidth = 1) +
  geom_image(data = p_max_stat,
             aes(x = stat, y = 0, image = icon_new),
             size = 0.4) +
  geom_text(data = p_max_stat,
            aes(x = stat, y = y, label = max)) +
  #geom_text(data=p_max_stat,  # this block will add the name of the center pokemon for each type
  #         aes(x="spatk",y=100,label=pokemon),nudge_x = -0.5,
  #         )+
  scale_fill_manual(values = type_colors$color) +
  scale_color_manual(values = c("black", rep(NA, 5)),
                     na.value = "transparent") +
  coord_polar() +
  labs(
    y = "Median Stat Value",
    title = "Pokémon median stat distributions by primary type!",
    subtitle = "Each pie slice scales to the median value of the listed stat for all Pokémon possessing a given primary type.
Pie slice outlined in black denotes the stat that has the highest median value across all Pokémon of each primary type.
Icon shows the Pokémon that has the highest base value of that particular stat, which is shown by the number within the slice.",
caption = "@benuveges.bsky.social"
  ) +
  facet_wrap(~ type_1, ncol = 6) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "transparent"),
    axis.title.x = element_blank(),
    text = element_text(family = "special"),
    axis.text.x = element_text(vjust = -1)
  )

poke_plot
