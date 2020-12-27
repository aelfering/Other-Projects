
source('Pokemon Data Cleaning.R')

# building the r shiny dashboard
ui <- fluidPage(
  
  titlePanel("College Football Team Performance"),    
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
                mainPanel(plotOutput("plot", width = "100%"))
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
      scale_color_discrete(labels = c('Poke Ball', 'Ultra Ball', 'Great Ball', 'Dusk Ball')) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste("The Odds of Catching ", input$pokemon, ' at ', scales::percent(as.numeric(input$range)), ' Health with ', input$status, ' Status', sep = ''),
           subtitle = paste('To have at least a ', scales::percent(as.numeric(input$odds)), ' chance of catching ', input$pokemon, ', you need as few as ', dusk_balls$Ball.Integer, ' Dusk Balls or as many as ', regular_balls$Ball.Integer, ' Poke Balls\n', sep = ''),
           color = 'Balls',
           caption = '\nVisualization by Alex Elfering\nData Sources: Formula Courtesy of The Cave of Dragonflies; Bulbapedia, The Pokemon Company and Nintendo\nBased on Generation VIII',
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
  
  
}

shinyApp(ui, server)