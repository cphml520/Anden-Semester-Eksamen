library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(jsonlite)
library(ggsoccer)

data_shots <- readRDS("data_shots.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Skud Freeze Frame Oversigt"),
  dashboardSidebar(
    selectizeInput("league", "Liga", choices = NULL),
    selectizeInput("game", "Kamp", choices = NULL),
    selectInput("team", "Hold", choices = NULL),
    selectInput("player", "Spiller", choices = NULL),
    selectInput("timestamp", "Tidspunkt", choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      column(width = 4,
             box(title = "Statistik på skud", width = NULL, solidHeader = TRUE, status = "info",
                 tableOutput("matchStats"))
      ),
      column(width = 8,
             box(title = "Freeze Frame Plot", width = NULL, solidHeader = TRUE, status = "primary",
                 plotOutput("shotPlot", height = "700px"))
      )
    )
  )
)

server <- function(input, output, session) {
 
  observe({
    updateSelectizeInput(session,
                         inputId = "league",
                         choices = unique(data_shots$competition.competition_name),
                         server = TRUE)
  })
  
  observeEvent(input$league, {
    req(input$league)
    league_games <- data_shots %>%
      filter(competition.competition_name == input$league) %>%
      pull(game) %>%
      unique()
    
    updateSelectizeInput(
      session,
      inputId = "game",
      choices = sort(league_games),
      server = TRUE
    )
  })
  
  # Update team choices
  observeEvent(c(input$league, input$game), {
    req(input$league, input$game)
    teams <- data_shots %>%
      filter(competition.competition_name == input$league, game == input$game) %>%
      distinct(team.name) %>%
      pull()
    updateSelectInput(session, "team", choices = teams)
  })
  
  # Update player choices
  observeEvent(c(input$league, input$game, input$team), {
    req(input$league, input$game, input$team)
    players <- data_shots %>%
      filter(competition.competition_name == input$league, game == input$game, team.name == input$team) %>%
      distinct(player.name) %>%
      pull()
    updateSelectInput(session, "player", choices = players)
  })
  
  # Update timestamp choices
  observeEvent(c(input$league, input$game, input$team, input$player), {
    req(input$league, input$game, input$team, input$player)
    times <- data_shots %>%
      filter(competition.competition_name == input$league, game == input$game, team.name == input$team, player.name == input$player) %>%
      pull(timestamp)
    updateSelectInput(session, "timestamp", choices = times)
  })
  
  # Reactive shot row
  one_shot <- reactive({
    req(input$league, input$game, input$team, input$player, input$timestamp)
    data_shots %>%
      filter(
        competition.competition_name == input$league,
        game == input$game,
        team.name == input$team,
        player.name == input$player,
        timestamp == input$timestamp
      ) %>%
      slice(1)
  })
  
  # Reactive freeze frame
  one_freezeframe <- reactive({
    req(one_shot())
    raw <- one_shot()$shot.freeze_frame[[1]]
    ff <- jsonlite::flatten(as.data.frame(raw))
    ff %>%
      mutate(location = gsub('c\\(|\\)|\\"', '', location)) %>%
      separate(location, into = c("x", "y"), sep = ", ", convert = TRUE) %>%
      mutate(across(c(x, y), as.numeric))
  })
  
  # Plot
  output$shotPlot <- renderPlot({
    req(one_shot(), one_freezeframe())
    
    shot <- one_shot()
    freeze <- one_freezeframe()
    
    goal_x <- 120
    goal_y1 <- 36
    goal_y2 <- 44
    
    # Base plot
    p <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "forestgreen") +
      theme_minimal() +
      coord_fixed(xlim = c(60, 120), ylim = c(0, 80)) +
      geom_point(data = shot, aes(x = x, y = y, color = "Skud"), size = 3, inherit.aes = FALSE) +
      geom_segment(data = shot,
                     aes(x = x, y = y, xend = end.x, yend = end.y),
                     color = "red", linewidth = 1, arrow = arrow(length = unit(0.15, "inches"))) +
      geom_polygon(data = data.frame(
        x = c(shot$x, goal_x, goal_x),
        y = c(shot$y, goal_y1, goal_y2)
      ), aes(x = x, y = y), fill = "red", alpha = 0.2) +
      
      geom_point(data = freeze, aes(x = x, y = y, color = as.character(teammate)), size = 3) +
      
      geom_text_repel(data = freeze, aes(x = x, y = y, label = player.name),
                      color = "white", size = 4, max.overlaps = Inf) +
            scale_color_manual(
        name = "Spillere",
        values = c("TRUE" = "blue", "FALSE" = "yellow", "Skud" = "red"),
        labels = c("TRUE" = "Medspiller", "FALSE" = "Modstander", "Skud" = "Skud")) +
            theme(
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      )
    
    
    # Optional pass suggestions
    if (!shot$no_valid_pass) {
      pass_names <- unlist(shot$pos_passes)
      pass_targets <- freeze %>% filter(player.name %in% pass_names)
      
      if (nrow(pass_targets) > 0) {
        p <- p +
          geom_segment(data = pass_targets, aes(x = shot$x, y = shot$y, xend = x, yend = y),
                       color = "red", linetype = "dashed", linewidth = 1) +
          geom_point(data = pass_targets, aes(x = x, y = y), shape = 17, color = "red", size = 4)
      }
    }
    
    p
  })
  
  output$matchStats <- renderTable({
    req(one_shot())
    shot <- one_shot()
    
    game <- shot$game
    xG <- round(shot$shot.statsbomb_xg*100)
    outcome <- shot$shot.outcome.name
    should_have_passed <- ifelse(!shot$no_valid_pass, "Ja", "Nej")
    potential_passes <- ifelse(!shot$no_valid_pass,
                               paste(unlist(shot$pos_passes), collapse = ", "),
                               "N/A")
    
    data.frame(
      Stat = c("Kamp", "xG %", "Resultat", "Skulle have været en aflevering", "Mulige afleveringer til:"),
      Værdi = c(game, xG, outcome, should_have_passed, potential_passes),
      stringsAsFactors = FALSE
    )
  })
  
}

shinyApp(ui, server)
