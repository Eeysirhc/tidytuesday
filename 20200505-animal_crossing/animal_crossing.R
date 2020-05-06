
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2020-05-06

# TIDYTUESDAY: ANIMAL CROSSING (WEEK 2020-05-05)
# SOURCE: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-05/readme.md

# LOAD LIBRARIES
library(tidyverse)
library(ggfortify)

# READ DATA
villagers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')


# PLOT
p <- villagers %>% 
  select(gender, species, personality) %>% 
  mutate(species = str_to_title(species)) %>%
  group_by(gender, species, personality) %>% 
  summarize(n = n()) %>% 
  mutate(pct_total = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(personality, pct_total, fill = gender, color = gender, group = gender)) +
  geom_polygon(alpha = 0.5) +
  geom_point() +
  coord_polar() +
  facet_wrap(~species) +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL,
       title = "Animal Crossing: villager personality traits by species & gender",
       caption = "by: @eeysirhc\nsource:VillagerDB") +
  theme_bw(base_size = 15) +
  theme(legend.position = 'top',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# SAVE PLOT
ggsave("animal_crossing.png", p, width = 30, height = 20, units = "in")

