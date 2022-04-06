# #30DayChartChallenge Day 5
# Slope

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)

wn_dal <- read.csv('~/wn dal flights.csv')
wn_dal_cancellations <- read.csv('~/wn dal cancellations.csv')

windowsFonts(sans="IBM Plex Sans")
loadfonts(device="win")
loadfonts(device="postscript")

# quick data cleaning
flights_names <- c('carrier_code', 'date', 'flight_number', 'tail_number', 'destination_airport', 'sch_dep_time', 'act_dep_time', 'sch_elap_time', 'act_elap_time', 'dep_del', 'wheels_off_time', 'taxi_out_time', 'delay_carrier', 'delay_weather', 'delay_nas', 'delay_security', 'delay_late_air_arrival')
colnames(wn_dal) <- flights_names

cancel_names <- c('carrier_code', 'date', 'flight_number', 'tail_number', 'destination')
colnames(wn_dal_cancellations) <- cancel_names

wn_dal_no_cancel <- anti_join(wn_dal, wn_dal_cancellations ) %>% distinct()

wn_dal_stats <- wn_dal_no_cancel %>%
  mutate(delayed = ifelse(dep_del > 15, 1, 0),
         flights = 1,
         date = mdy(date),
         year_date = year(date),
         month_date = floor_date(date, unit = 'months')) %>%
  filter(year_date %in% c(2018, 2019)) %>%
  group_by(destination_airport,
           year_date) %>%
  summarise(delayed = sum(delayed),
            flights = sum(flights)) %>%
  ungroup() %>%
  filter(flights >= 100) %>%
  group_by(destination_airport) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(pct_delayed = delayed/flights,
         rank_flights = dense_rank(desc(flights)),
         rank_delayed = dense_rank(desc(delayed)),
         delayed_50 = (100*delayed)/flights,
         rank_per_50 = dense_rank(desc(delayed_50))) %>%
  group_by(destination_airport) %>%
  mutate(change = delayed_50-lag(delayed_50)) %>%
  ungroup() %>%
  arrange((change))

largest_falls <- wn_dal_stats %>%
  filter(row_number() <= 5)

largest_jumps <- wn_dal_stats %>%
  arrange(desc(change)) %>%
  filter(row_number() <= 5,
         change > 0)

year_bands <- wn_dal_stats %>%
  group_by(year_date) %>%
  summarise(min_delay = min(delayed_50),
            max_delay = max(delayed_50)) %>%
  ungroup()

wn_chart <- wn_dal_stats %>%
  ggplot() +
  geom_line(#wn_dal_stats,
    mapping = aes(x = year_date,
                  y = delayed_50,
                  group = destination_airport),
    size = 2,
    alpha = 0.4,
    color = 'gray') +
  geom_line(wn_dal_stats %>% filter(destination_airport %in% largest_falls$destination_airport),
            mapping = aes(x = year_date,
                          y = delayed_50,
                          group = destination_airport),
            size = 2,
            #alpha = 0.3,
            color = 'steelblue') +
  geom_point(wn_dal_stats %>% filter(destination_airport %in% largest_falls$destination_airport),
             mapping = aes(x = year_date,
                           y = delayed_50,
                           group = destination_airport),
             size = 3,
             #alpha = 0.3,
             color = 'steelblue') +
  geom_point(wn_dal_stats %>% filter(destination_airport %in% largest_falls$destination_airport),
             mapping = aes(x = year_date,
                           y = delayed_50,
                           group = destination_airport),
             size = 2,
             #alpha = 0.3,
             color = 'white') +
  geom_line(wn_dal_stats %>% filter(destination_airport %in% largest_jumps$destination_airport),
            mapping = aes(x = year_date,
                          y = delayed_50,
                          group = destination_airport),
            size = 2,
            #alpha = 0.3,
            color = 'darkorange') +
  geom_point(wn_dal_stats %>% filter(destination_airport %in% largest_jumps$destination_airport),
             mapping = aes(x = year_date,
                           y = delayed_50,
                           group = destination_airport),
             size = 3,
             #alpha = 0.3,
             color = 'darkorange') +
  geom_point(wn_dal_stats %>% filter(destination_airport %in% largest_jumps$destination_airport),
             mapping = aes(x = year_date,
                           y = delayed_50,
                           group = destination_airport),
             size = 2,
             #alpha = 0.3,
             color = 'white') +
  geom_text_repel(wn_dal_stats %>% filter(destination_airport %in% c(largest_jumps$destination_airport,largest_falls$destination_airport), year_date == max(year_date) ),
                  mapping = aes(x = year_date,
                                y = delayed_50,
                                group = destination_airport,
                                label = destination_airport,
                                color = destination_airport %in% largest_jumps$destination_airport),
                  #color = ,
                  fontface = 'bold',
                  vjust = 1,
                  hjust = -5) +
  scale_color_manual(values = c('TRUE' = 'darkorange',
                                'FALSE' = 'steelblue')) +
  scale_x_continuous(breaks = seq(2018, 2019, 1)) +
  expand_limits(x = c(2018, 2019.1)) +
  labs(title = 'Southwest Airlines Outbound DAL FLights Delayed per 100 Flights',
       subtitle = 'Markets with the largest increase (orange) vs. flights with the largest decrease (blue) 2018 vs. 2019',
       caption = 'Alex Elfering | 30 Day Chart Challenge Day #5: Slope\nSource: Bureau of Transportation Stats | Note: Excludes markets only served in 2018 or 2019',
       color = 'Destination:',
       x = '',
       y = '') +
  scale_y_continuous(position = 'left') +
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 9),
        axis.title = element_text(size = 12),
        axis.text.y.left = element_text(size = 12, color = 'black'),
        axis.text.x.bottom = element_text(size = 12, face = 'bold', color = 'black'),
        #axis.text.y.left = element_blank(),
        #axis.line.y = element_blank(),
        #axis.ticks.length.y = unit(.25, "cm"),
        #axis.ticks.y.left = element_blank(),
        axis.line.x.top = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        #axis.line.y = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

ggsave(wn_chart,
       file = 'WN Slope.png', 
       width = 9, 
       height = 8, 
       units = 'in')
