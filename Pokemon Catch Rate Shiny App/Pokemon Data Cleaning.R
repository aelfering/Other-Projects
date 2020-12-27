# Pokemon catch rate data frame

# load the packages
list.of.packages <- c('reactable',
                      'tidyverse',
                      'ggplot2',
                      'htmltools',
                      'shiny',
                      'ggrepel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(ggplot2)
library(reactable)
library(htmltools)
library(shiny)
library(ggrepel)

setwd("~/GitHub/Other-Projects/Pokemon Catch Rate Shiny App")

# bring in the data
pokedex_multiplier <- read.csv('PokeDex Multiplier.csv')
status_conditions <- read.csv('Status Conditions.csv')
ball_multipliers <- read.csv('Ball Multiplier.csv')
pokemon_list <- read.csv('Pokemon List.csv')

# data cleaning
status_col_names <- c('Status',
                      'Condition')

ball_col_names <- c('Ball',
                    'Rate')

pokemon_col_names <- c('ID',
                       'Pokemon_Name',
                       'Catch_Rate')

colnames(status_conditions) <- status_col_names
colnames(ball_multipliers) <- ball_col_names
colnames(pokemon_list) <- pokemon_col_names

pokemon <- dplyr::mutate(pokemon_list, Catch_Rate = as.numeric(gsub('\\*', '', Catch_Rate)))

# create the dataframe and combinations of variables
catch_rate_values <- unique(as.numeric(pokemon$Catch_Rate))
ball_values <- unique(as.numeric(ball_multipliers$Rate))
critical_values <- unique(as.numeric(pokedex_multiplier$Multiplier))
status_values <- unique(as.numeric(status_conditions$Condition))

percent_health <- seq(0.1, 1, by = 0.1)


full_df_combos <- expand.grid(catch_rate_values, 
                              ball_values, 
                              percent_health, 
                              critical_values, 
                              status_values)
colnames(full_df_combos) <- c('Catch_Rate', 
                              'Ball_Multipliers', 
                              'Percent_Health', 
                              'PokeDex_Multiplier', 
                              'Status_Condition')

# join the variables names
full_df_joins <- full_df_combos %>%
  inner_join(pokemon) %>%
  inner_join(ball_multipliers,
             by = c('Ball_Multipliers' = 'Rate')) %>%
  inner_join(pokedex_multiplier,
             by = c('PokeDex_Multiplier' = 'Multiplier')) %>%
  inner_join(status_conditions,
             by = c('Status_Condition' = 'Condition')) %>%
  mutate(Max_HP = 500)