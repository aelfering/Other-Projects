# #30DayChartChallenge Day 1
# Part-to-Whole

library(ggplot2)
library(tidyverse)

grocery_spend <- read.csv('~/GroceryAllocation.csv') %>% select(-X)

top_stores <- c('Aldi', "Trader Joe's", 'Vite Ramen', 'Hy-Vee')

grocery_store_df <- grocery_spend %>%
  mutate(store = ifelse(transaction %in% top_stores, transaction, "Other"),
         store = factor(store, levels = c(top_stores, "Other")))
  
grocery_store_df %>%
  ggplot() + 
  geom_hline(yintercept = 1.05,
             color = NA) +
  geom_bar(mapping = aes(x = factor(month_date),
                         y = pct,
                         fill = store),
           #color = 'white',
           width = 0.5,
           position = 'stack',
           stat = 'identity') +
  geom_text(grcoery_store_df,
            mapping = aes(x = factor(month_date),
                          y = pct,
                          fill = store,
                          label = ifelse(store == 'Other', NA, paste0(round(pct*100), '%'))),
            color = 'white',
            fontface = 'bold',
            position = position_stack(vjust = 0.5)
  ) +
  scale_y_continuous(labels = c('0', '25', '50', '75', '100%'),
                     breaks = seq(0,1,0.25),
                     expand = c(0,0),
                     position = 'right') +
  scale_x_discrete(labels = c('Jan 22', 
                              'Feb 22', 
                              'Mar 22')) +
  labs(title = 'Where do my Grocery Dollars Go?',
       subtitle = 'Share of Grocery Spending by Month',
       fill = '',
       x = '',
       y = '') +
  scale_fill_manual(values = c('Aldi' = '#cc4c00',
                               "Trader Joe's" = '#e4770b',
                               'Vite Ramen' = '#f6a328',
                               'Hy-Vee' = '#ffd14c',
                               'Other' = '#cacaca')) +
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
        axis.text.y.right = element_text(size = 12, face = 'bold', color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 