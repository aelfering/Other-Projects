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

#setwd("~/GitHub/Other-Projects/Pokemon Catch Rate Shiny App")

# bring in the data
pokedex_multiplier <- read.csv('PokeDex Multiplier.csv')
status_conditions <- read.csv('Status Conditions.csv')
ball_multipliers <- read.csv('Ball Multiplier.csv')
pokemon_list <- read.csv('Pokemon List.csv')

#### data cleaning  ####
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



####  App ####
ui <- fluidPage(
  
  #titlePanel("College Football Team Performance"),    
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput("pokemon", "Select a Pokemon:",
                              sort(unique(full_df_joins$Pokemon_Name))),
                  selectInput("status", "Select a Status:",
                              sort(unique(full_df_joins$Status))),
                  selectInput("species", "How Many Pokemon have you Caught?",
                              factor(unique(full_df_joins$SpeciesCaught),levels=c("> 600", "451-600", "301-450", "151-300", "31-150", "<= 30"))),
                  selectInput("range", "Percent of Health Remaining:",
                              seq(1, 0.1, by = -0.1)),
                  selectInput("odds", "What Do You Want Your Odds of Catching to be? (%)",
                              c(0.25, 0.5, 0.75, 0.99)),
                  h5("Table created by Alex Elfering"),
                  width=2),
                mainPanel(plotOutput("plot", width = "100%"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          plotOutput("critfreq", width = "100%"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          plotOutput("crit", width = "100%"))
  )) 

server <- function(input, output, session){
  
  output$plot <- renderPlot({
    
    adj_calculations <- full_df_joins %>%
      # what are the odds of regular catching a pokemon?
      mutate(Remaining_Health = Percent_Health * Max_HP,
             catch_value = (( (3 * Max_HP) - (2 * Remaining_Health) ) * (Catch_Rate * Ball_Multipliers ) / (3 * Max_HP) ) * Status_Condition,
             shake_prob = (65536 / (255/catch_value)^0.1875 ),
             #  if the catch value exceeds 255 then the catch automatically succeeds else catch round((shake_prob/65535) ^ 4, 3)
             catch_prob = case_when(catch_value > 255 ~ 1,
                                    catch_value <= 255 ~ round((shake_prob/65535) ^ 4, 3))) %>%
      # what are the odds of a critical catch occurring and succeeding?
      mutate(crit_catch_value = floor((catch_value*PokeDex_Multiplier)/6),
             crit_prob = crit_catch_value/256,
             #  if the probability is 0 the chance of succeeding is 0, else cube root of catch_prob
             crit_succeed = case_when(crit_prob == 0 ~ 0,
                                      crit_prob > 0 ~ catch_prob^(1/3) ))
    
    adj_filtered <- dplyr::filter(adj_calculations, 
                                  Pokemon_Name == input$pokemon, 
                                  Status == input$status,
                                  SpeciesCaught == input$species,
                                  Percent_Health == input$range)
    
    # calculate the odds of capture for 1-1,000 throws
    balls_int <- seq(1, 1000) * 1.0
    
    ball_list_test <- list()
    for(i in seq_along(balls_int)){
      
      mark2 <- adj_filtered %>%
        group_by(Catch_Rate,
                 Ball_Multipliers,
                 Status_Condition,
                 Percent_Health,
                 PokeDex_Multiplier) %>%
        mutate(Ball.Integer = balls_int[i]) %>%
        ungroup() %>%
        mutate
      
      ball_list_test[[i]] <- mark2
    }
    
    bind.test <- do.call(rbind, ball_list_test)
    
    odds_within_throws <- dplyr::mutate(bind.test, Odds = 1-(1-catch_prob)^Ball.Integer)
    
    # find the minimum balls needed by type to catch within a certain odds
    odds_needed <- odds_within_throws %>%
      filter(Odds >= input$odds) %>%
      group_by(Catch_Rate,
               Ball_Multipliers,
               Status_Condition,
               Percent_Health,
               PokeDex_Multiplier) %>%
      slice(which.min(Ball.Integer)) %>%
      ungroup() %>%
      select(Catch_Rate,
             Odds,
             Ball_Multipliers,
             Ball.Integer)
    
    regular_balls <- dplyr::filter(odds_needed, Ball_Multipliers == 1)
    dusk_balls <- dplyr::filter(odds_needed, Ball_Multipliers == 3)
    
    # visualization
    ggplot(subset(odds_within_throws, Odds <= 0.9999),
           aes(x = Ball.Integer,
               y = Odds,
               group = Ball_Multipliers,
               color = factor(Ball_Multipliers))) +
      geom_hline(yintercept = 0,
                 linetype = 'dashed') +
      geom_line(size = 2,
                alpha = 0.6) +
      geom_hline(yintercept = as.numeric(input$odds),
                 size = 1,
                 linetype = 'dashed') +
      geom_point(data = odds_needed,
                 mapping = aes(x = Ball.Integer,
                               y = Odds,
                               color = factor(Ball_Multipliers)),
                 size = 4) +
      geom_point(data = odds_needed,
                 mapping = aes(x = Ball.Integer,
                               y = Odds,
                               color = factor(Ball_Multipliers)),
                 shape = 1,
                 size = 4,
                 colour = "black") +
      geom_label_repel(data = odds_needed,
                       mapping = aes(x = Ball.Integer,
                                     y = Odds,
                                     label = Ball.Integer,
                                     color = factor(Ball_Multipliers)), 
                       size = 6,
                       box.padding = 2,
                       show.legend = FALSE) +
      scale_color_discrete(labels = c('Poke Ball', 'Ultra Ball', 'Great Ball', 'Dusk Ball*')) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste("The Odds of Catching ", input$pokemon, ' at ', scales::percent(as.numeric(input$range)), ' Health\nwith ', input$status, ' Status', sep = ''),
           subtitle = paste('To have at least a ', scales::percent(as.numeric(input$odds)), ' chance of catching ', input$pokemon, ', you need as few as ', dusk_balls$Ball.Integer, ' Dusk Balls\nor as many as ', regular_balls$Ball.Integer, ' Poke Balls\n', sep = ''),
           color = 'Balls',
           caption = '\nVisualization by Alex Elfering\nData Sources: Formula Courtesy of The Cave of Dragonflies; Bulbapedia, The Pokemon Company and Nintendo\nBased on Generation VIII; Dusk Balls are only effective at night or in a cave',
           y = 'Odds\n',
           x = '\nTotal Poke Ball Throws') +
      theme(plot.title = element_text(face = 'bold', 
                                      size = 20, 
                                      color = 'black',
                                      family = 'Arial'),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 16,
                                       color = 'black',
                                       family = 'Arial'),
            legend.title = element_text(size = 16, 
                                        family = 'Arial'),
            plot.subtitle = element_text(size = 19, 
                                         family = 'Arial', 
                                         color = 'black'),
            plot.caption = element_text(size = 16, 
                                        family = 'Arial', 
                                        color = 'black'),
            axis.title = element_text(size = 16,
                                      family = 'Arial', 
                                      color = 'black'),
            axis.text = element_text(size = 16, 
                                     family = 'Arial', 
                                     color = 'black'),
            strip.text = ggplot2::element_text(size = 16, 
                                               hjust = 0, 
                                               face = 'bold', 
                                               color = 'black', 
                                               family = 'Arial'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", 
                                     linetype = "solid"),
            panel.grid.major.y = element_line(colour = "#c1c1c1", 
                                              linetype = "dashed"),
            panel.grid.major.x = element_line(colour = "#c1c1c1", 
                                              linetype = "dashed")) 
    
  }, height = 900)
  
  
  output$critfreq <- renderPlot({
    adj_calculations <- full_df_joins %>%
      # what are the odds of regular catching a pokemon?
      mutate(Remaining_Health = Percent_Health * Max_HP,
             catch_value = (( (3 * Max_HP) - (2 * Remaining_Health) ) * (Catch_Rate * Ball_Multipliers ) / (3 * Max_HP) ) * Status_Condition,
             shake_prob = (65536 / (255/catch_value)^0.1875 ),
             #  if the catch value exceeds 255 then the catch automatically succeeds else catch round((shake_prob/65535) ^ 4, 3)
             catch_prob = case_when(catch_value > 255 ~ 1,
                                    catch_value <= 255 ~ round((shake_prob/65535) ^ 4, 3))) %>%
      # what are the odds of a critical catch occurring and succeeding?
      mutate(crit_catch_value = floor((catch_value*PokeDex_Multiplier)/6),
             crit_prob = crit_catch_value/256,
             #  if the probability is 0 the chance of succeeding is 0, else cube root of catch_prob
             crit_succeed = case_when(crit_prob == 0 ~ 0,
                                      crit_prob > 0 ~ catch_prob^(1/3) ),
             times_effective = crit_succeed/catch_prob)
    
    adj_filtered <- dplyr::filter(adj_calculations, 
                                  Pokemon_Name == input$pokemon, 
                                  Status == input$status,
                                  SpeciesCaught == input$species,
                                  Percent_Health == input$range) %>%
      mutate(Frequency = crit_prob * 1000)
    
    ggplot(adj_filtered,
           aes(x = reorder(Ball, Frequency),
               y = Frequency)) +
      geom_bar(stat = 'identity',
               position = 'identity',
               fill = 'steelblue',
               width = 0.6) +
      geom_text(aes(label = scales::comma(round(Frequency)), 
                    hjust = 1.2), 
                size = 5,
                color = "white") +
      geom_hline(yintercept = 0,
                 size = 1) +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste('Critical Catch Frequency When Capturing ', input$Pokemon, ' at ', scales::percent(as.numeric(input$range)), ' Health with ', input$status, ' Status', sep = ''),
           subtitle = 'Critical catches will occur depending on the total Pokemon caught and its catch rate.\nIf you threw different types of Pokeballs 1,000 times, how many of those would be critical catches?\n',
           caption = '\nVisualization by Alex Elfering\nData Sources: Formula Courtesy of The Cave of Dragonflies; Bulbapedia, The Pokemon Company and Nintendo\nBased on Generation VIII; Dusk Balls are only effective at night or in a cave',
           x = '',
           y = '\nTotal Throws') +
      theme(plot.title = element_text(face = 'bold', 
                                      size = 20, 
                                      color = 'black',
                                      family = 'Arial'),
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 16,
                                       color = 'black',
                                       family = 'Arial'),
            legend.title = element_text(size = 16, 
                                        family = 'Arial'),
            plot.subtitle = element_text(size = 19, 
                                         family = 'Arial', 
                                         color = 'black'),
            plot.caption = element_text(size = 16, 
                                        family = 'Arial', 
                                        color = 'black'),
            axis.title = element_text(size = 16,
                                      family = 'Arial', 
                                      color = 'black'),
            axis.text = element_text(size = 16, 
                                     family = 'Arial', 
                                     color = 'black'),
            strip.text = ggplot2::element_text(size = 16, 
                                               hjust = 0, 
                                               face = 'bold', 
                                               color = 'black', 
                                               family = 'Arial'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", 
                                     linetype = "solid"),
            panel.grid.major.y = element_line(colour = "#c1c1c1", 
                                              linetype = "dashed"),
            panel.grid.major.x = element_line(colour = "#c1c1c1", 
                                              linetype = "dashed")) 
    
    
    
    
  }, height = 600 )
  
  
  output$crit <- renderPlot({
    adj_calculations <- full_df_joins %>%
      # what are the odds of regular catching a pokemon?
      mutate(Remaining_Health = Percent_Health * Max_HP,
             catch_value = (( (3 * Max_HP) - (2 * Remaining_Health) ) * (Catch_Rate * Ball_Multipliers ) / (3 * Max_HP) ) * Status_Condition,
             shake_prob = (65536 / (255/catch_value)^0.1875 ),
             #  if the catch value exceeds 255 then the catch automatically succeeds else catch round((shake_prob/65535) ^ 4, 3)
             catch_prob = case_when(catch_value > 255 ~ 1,
                                    catch_value <= 255 ~ round((shake_prob/65535) ^ 4, 3))) %>%
      # what are the odds of a critical catch occurring and succeeding?
      mutate(crit_catch_value = floor((catch_value*PokeDex_Multiplier)/6),
             crit_prob = crit_catch_value/256,
             #  if the probability is 0 the chance of succeeding is 0, else cube root of catch_prob
             crit_succeed = case_when(crit_prob == 0 ~ 0,
                                      crit_prob > 0 ~ catch_prob^(1/3) ),
             times_effective = crit_succeed/catch_prob)
    
    adj_filtered <- dplyr::filter(adj_calculations, 
                                  Pokemon_Name == input$pokemon, 
                                  Status == input$status,
                                  SpeciesCaught == input$species,
                                  Percent_Health == input$range) %>%
      mutate(crit_pct = ifelse(crit_succeed == 0, catch_prob, crit_succeed),
             time_label = ifelse(times_effective == 0, 'Odds unchanged', paste('Odds increase\n', round(times_effective, 1), 'x times', sep= '')  ))
    
    ggplot(adj_filtered,
           aes(x = reorder(Ball, times_effective),
               y = catch_prob)) +
      geom_segment(aes(x = reorder(Ball, times_effective),
                       xend = reorder(Ball, times_effective),
                       y = catch_prob, 
                       yend = crit_pct), 
                   color = "grey",
                   size = 3) +
      geom_point(aes(x = reorder(Ball, times_effective),
                     y = crit_pct), 
                 color = 'steelblue', 
                 size = 5) +
      geom_point(aes(x = reorder(Ball, times_effective),
                     y = catch_prob), 
                 color = 'orange', 
                 size = 5) +
      geom_text(data = subset(adj_filtered, times_effective == max(times_effective)),
                mapping = aes(x = reorder(Ball, times_effective),
                              y = catch_prob),
                vjust = 1.3,
                size = 5,
                color = 'orange',
                label = 'Normal Catch\nOdds') +
      geom_text(data = subset(adj_filtered, times_effective == max(times_effective)),
                mapping = aes(x = reorder(Ball, times_effective),
                              y = crit_pct,
                              label = ifelse(crit_pct == catch_prob, '', 'Critical Catch\nOdds')),
                vjust = 1.3,
                size = 5,
                color = 'steelblue') +
      geom_text(mapping = (aes(x = reorder(Ball, times_effective),
                               y = crit_pct,
                               label = time_label)),
                hjust = 0.5,
                size = 5,
                vjust = -0.5) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste('How a Critical Catch Impacts Chances of Capturing ', input$pokemon, ' at ', scales::percent(as.numeric(input$range)), ' Health\nwith ', input$status, ' Status', sep = ''),
           subtitle = 'The odds of catching a Pokemon increase if a critical catch succeeds\n',
           caption = '\nVisualization by Alex Elfering\nData Sources: Formula Courtesy of The Cave of Dragonflies; Bulbapedia, The Pokemon Company and Nintendo\nBased on Generation VIII; Dusk Balls are only effective at night or in a cave',
           x = '',
           y = '\nOdds of Capture') +
      theme(plot.title = element_text(face = 'bold', 
                                      size = 20, 
                                      color = 'black',
                                      family = 'Arial'),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 16,
                                       color = 'black',
                                       family = 'Arial'),
            legend.title = element_text(size = 16, 
                                        family = 'Arial'),
            plot.subtitle = element_text(size = 19, 
                                         family = 'Arial', 
                                         color = 'black'),
            plot.caption = element_text(size = 16, 
                                        family = 'Arial', 
                                        color = 'black'),
            axis.title = element_text(size = 16,
                                      family = 'Arial', 
                                      color = 'black'),
            axis.text = element_text(size = 16, 
                                     family = 'Arial', 
                                     color = 'black'),
            strip.text = ggplot2::element_text(size = 16, 
                                               hjust = 0, 
                                               face = 'bold', 
                                               color = 'black', 
                                               family = 'Arial'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", 
                                     linetype = "solid"),
            panel.grid.major.y = element_line(colour = "#c1c1c1", 
                                              linetype = "dashed"),
            panel.grid.major.x = element_line(colour = "#c1c1c1", 
                                              linetype = "dashed")) 
    
    
    
    
  }, height = 600 )
  
  
}

shinyApp(ui, server)