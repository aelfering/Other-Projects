# #30DayChartChallenge Day 3
# historical

library(tidyverse)
library(zoo)

windowsFonts(sans="IBM Plex Sans")
loadfonts(device="win")
loadfonts(device="postscript")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

cfb_data <- read.csv('C:/Users/alexe/Desktop/FBS Full Schedule.csv') %>% select(-X)
Conferences <- read.csv('C:/Users/alexe/Desktop/Conferences.csv') %>% mutate(Conf = trim(Conf), Div = trim(Div)) %>% select(-X)

rolling_conf_wins <- cfb_data %>%
  #filter(Conf == 'Big Ten') %>%
  mutate(Wins = ifelse(Pts > Opp, 1, 0)) %>%
  group_by(School, 
           Season) %>%
  summarise(Wins = sum(Wins)) %>%
  ungroup() %>%
  group_by(School) %>%
  mutate(RollingWins = rollapplyr(Wins, 10, sum, partial = TRUE)) %>%
  ungroup() %>%
  filter(Season >= 1971) %>%
  inner_join(Conferences,
             by = c('Season' = 'Season', 
                    'School' = 'Var.2')) %>%
  filter(Conf == 'ACC')

ribbon_wins <- rolling_conf_wins %>%
  group_by(Season) %>%
  summarise(min_wins = min(RollingWins, na.rm = TRUE),
            max_wins = max(RollingWins, na.rm = TRUE)) %>%
  ungroup()

winningest_teams <- ggplot(ribbon_wins,
                           aes(Season)) + 
  geom_ribbon(mapping = aes(ymin = min_wins,
                            ymax = max_wins),
              alpha = 0.4,
              #color = 'gray70',
              fill = 'grey90') +
  geom_line(rolling_conf_wins,
            mapping = aes(x = Season,
                          y = RollingWins,
                          group = School),
            color = 'gray',
            alpha = 0.3,
            size = 1) +
  geom_line(rolling_conf_wins %>% filter(School %in% c('Florida State', 'Maryland', 'Virginia Tech')),
            mapping = aes(x = Season,
                          y = RollingWins,
                          color = School),
            #color = 'darkgray',
            alpha = 0.7,
            size = 1.5) +
  geom_line(rolling_conf_wins %>% filter(School == 'Clemson'),
            mapping = aes(x = Season,
                          y = RollingWins,
                          color = School),
            #color = '#54278f',
            size = 2) +
  geom_point(rolling_conf_wins %>% filter(School == 'Clemson', Season == 2008 | Season == 1978),
             mapping = aes(x = Season,
                           y = RollingWins,
                           color = School),
             #color = '#54278f',
             size = 4) +
  geom_point(rolling_conf_wins %>% filter(School == 'Clemson', Season == 2008 | Season == 1978),
             mapping = aes(x = Season,
                           y = RollingWins,
                           color = School),
             color = 'black',
             shape = 1,
             size = 4) +
  scale_color_manual(values = c('Clemson' = 'dark orange',
                                'Maryland' = 'red',
                                'Florida State' = 'dark red',
                                'Virginia Tech' = 'maroon')) +
  labs(title = 'Clemson Remains the Winningest Team in ACC',
       subtitle = 'Total wins by ACC team every 10 seasons',
       caption = 'Alex Elfering | 30 Day Chart Challenge Day #3: Historical\nSource: College Football Reference | Inspiration: The Economist',
       color = '',
       x = '',
       y = '') +
  #scale_y_continuous(position = 'right') +
  theme(plot.title = element_text(face = 'bold', size = 18),
        plot.subtitle = element_text(size = 16),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y.left = element_text(size = 12, color = 'black'),
        axis.text.x.bottom = element_text(size = 12, color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

ggsave(winningest_teams,
       file = 'Pac-12 Winnigest Teams.png', 
       width = 6, 
       height = 6, 
       units = 'in')
