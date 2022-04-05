# #30DayChartChallenge Day 3
# Slope

library(tidyverse)
#library(zoo)
library(ggplot2)
library(lubridate)
library(ggrepel)

aa_oma <- read.csv('~/aa oma flights.csv')

windowsFonts(sans="IBM Plex Sans")
loadfonts(device="win")
loadfonts(device="postscript")

# quick data cleaning
dsm_flights_names <- c('carrier_code', 'date', 'flight_number', 'tail_number', 'destination_airport', 'sch_dep_time', 'act_dep_time', 'sch_elap_time', 'act_elap_time', 'dep_del', 'wheels_off_time', 'taxi_out_time', 'delay_carrier', 'delay_weather', 'delay_nas', 'delay_security', 'delay_late_air_arrival')
colnames(aa_oma) <- dsm_flights_names

aa_oma_stats <- aa_oma %>%
  mutate(delayed = ifelse(dep_del > 15, 1, 0),
         flights = 1,
         date = mdy(date)) %>%
  filter(year(date) == 2019) %>%
  group_by(destination_airport) %>%
  summarise(delayed = sum(delayed),
            flights = sum(flights)) %>%
  ungroup() %>%
  filter(flights > 50) %>%
  mutate(pct_flight = flights/sum(flights),
         pct_delayed = delayed/sum(delayed),
         rank_flights = dense_rank(desc(flights)),
         rank_delayed = dense_rank(desc(delayed)),
         delayed_50 = (50*delayed)/flights,
         rank_per_50 = dense_rank(desc(delayed_50))) %>%
  arrange(desc(flights))

aa_raw_numbers <- aa_oma_stats %>%
  select(dest = destination_airport,
         flights,
         delays = delayed_50) %>%
  pivot_longer(cols = c('flights', 
                        'delays'),
               names_to = 'stat',
               values_to = 'val')

aa_oma_select <- aa_oma_stats %>%
  select(dest = destination_airport,
         flights = rank_flights,
         delays = rank_per_50) %>%
  pivot_longer(cols = c('flights', 
                        'delays'),
               names_to = 'stat',
               values_to = 'rank') %>%
  inner_join(aa_raw_numbers) %>%
  mutate(stat = factor(stat, levels = c('flights', 'delays')))
  

aa_chart <- aa_oma_select %>%
  ggplot() + 
  geom_line(mapping = aes(x = factor(stat),
                          y = rank,
                          group = dest,
                          color = dest),
            size = 3) +
  geom_line(mapping = aes(x = stat,
                          y = rank,
                          group = dest,
                          color = dest),
            size = 5) +
  geom_point(mapping = aes(x = stat,
                          y = rank,
                          group = dest,
                          color = dest),
            size = 6) +
  geom_point(mapping = aes(x = stat,
                           y = rank,
                           group = dest,
                           color = dest),
             color = 'white',
             size = 5) +
  geom_text_repel(data = aa_oma_select %>% filter(stat == 'flights'),
                  mapping = aes(x = stat,
                                y = rank,
                                label = scales::comma(val)),
                  hjust = 2) + 
  #geom_text_repel(data = aa_oma_select %>% filter(stat == 'delays'),
  #                mapping = aes(x = stat,
  #                              y = rank,
  #                              label = dest,
  #                              color = dest),
  #                #face = 'bold',
  #                hjust = -5) + 
  geom_text_repel(data = aa_oma_select %>% filter(stat == 'delays'),
                   mapping = aes(x = stat,
                                 y = rank,
                                 label = scales::comma(val)),
                   #label.size = NA,
                   hjust = -2) + 
  scale_y_reverse() +
  scale_x_discrete(position = 'top',
                   labels = c('Total Flights',
                              'Delays per 50\nFlights Flown')) +
  scale_color_manual(values = c('#a6cee3',
                                '#1f78b4',
                                '#b2df8a',
                                '#33a02c')) +
  labs(title = '2019 OMA AA Outbound Flights Flown vs. Flights Delayed',
       subtitle = 'Rank of flights flown vs. flights delayed per 50 flights by destination',
       caption = 'Alex Elfering | 30 Day Chart Challenge Day #5: Slope\nSource: Bureau of Transportation Stats',
       color = 'Destination:',
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
        axis.text.x.top = element_text(size = 12, color = 'black'),
        #axis.line.y = element_blank(),
        axis.ticks.length.x.top = unit(.5, "cm"),
        axis.ticks.y.left = element_blank(),
        axis.line.x.top = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 
  
aa_chart

ggsave(aa_chart,
       file = 'American Airlines Slope.png', 
       width = 7, 
       height = 6, 
       units = 'in')
  