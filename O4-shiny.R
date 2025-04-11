library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(ggsoccer)
library(ggrepel)
library(ggforce)

cluster_summary_7 <- readRDS("cluster_summary_7.rds")
passes_clustered_7 <- readRDS("passes_clustered_7.rds")
factor_props <- readRDS("factor_props.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Klynge Analyse"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Oversigt", tabName = "variable", icon = icon("chart-bar")),
      menuItem("Pr. klynge", tabName = "klynge", icon = icon("chart-pie")),
      menuItem("Pr. kamp", tabName = "kamp", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
            tabItem(tabName = "variable",
              fluidRow(
                h3("Oversigt over de variabler inkluderet i clustering", style = "margin-left:15px;")
               ),
              fluidRow(
                box(plotOutput("locationPlot"), width = 12)
              ),
              fluidRow(
                box(plotOutput("lengthPlot"), width = 6),
                box(plotOutput("anglePlot"), width = 6)
              ),
              fluidRow(
                box(plotOutput("accuracyPlot"), width = 4),
                box(plotOutput("headPlot"), width = 4),
                box(plotOutput("recoveryPlot"), width = 4)
              ),
              fluidRow(
                box(selectInput("variable2", "Filter efter faktorvariable:", choices = c("Kampintervaller", "Højde", "Positioner"), selected = "Kampintervaller"), width = 6),
                box(selectInput("level", "Valg en faktorværdi:", choices = NULL), width = 6)
              ),
              fluidRow(
                box(plotOutput("variablePlot"), width = 12)
              )
         ),
            tabItem(tabName = "klynge",
                    fluidRow(
                      box(selectInput("cluster", "Valge en klynge:", choices = unique(factor_props$cluster), selected = "1"), width = 6)
                    ),
                    fluidRow(
                      box(title = NULL, width = 12, solidHeader = TRUE, h3(textOutput("clusterTitle")))
                    ),
                    fluidRow(
                      box(plotOutput("arrowPlot"), width = 6),
                      box(plotOutput("heatmapPlot"), width = 6)
                    ),
                    fluidRow(
                      box(tableOutput("summaryTable"), width = 12, height = "150px")
                    ),
                    fluidRow(
                      box(plotOutput("periodPlot"), width = 4),
                      box(plotOutput("highPlot"), width = 4),
                      box(plotOutput("positionPlot"), width = 4)
                    ),
            ),
            tabItem(tabName = "kamp",
                    fluidRow(
                      box(selectInput("kamp", "Valge en kamp:", choices = unique(passes_clustered_7$game)), selected = NULL, width = 6)
                    ),
                    fluidRow(
                      box(plotOutput("clusterPlot"), width = 6),
                      box(plotOutput("allPlot"), width = 6)
                    )
                  )
            )
        )
)

server <- function(input, output, session) {
  
  observeEvent(input$variable2, {
    var_map <- c(
      "Kampintervaller" = "MATCHPERIOD2",
      "Højde" = "HEIGHT",
      "Positioner" = "ROLENAME"
    )
    
    selected_var <- var_map[input$variable2]
    
    levels <- factor_props %>%
      filter(variable == selected_var) %>%
      mutate(
        variable = recode(variable,
                          "MATCHPERIOD2" = "Kampintervaller",
                          "HEIGHT" = "Højde",
                          "ROLENAME" = "Positioner"),
        level = case_when(
          variable == "Højde" ~ recode(level, "high" = "høj", "no" = "ikke høj"),
          variable == "Positioner" ~ recode(level,
                                            "Defender" = "Forsvar",
                                            "Midfielder" = "Midbane",
                                            "Forward" = "Angreb",
                                            "Goalkeeper" = "Målmænd"),
          TRUE ~ level
        ),
        level = case_when(
          variable == "Positioner" ~ factor(level, levels = c("Målmænd", "Forsvar", "Midbane", "Angreb")),
          variable == "Kampintervaller" ~ factor(level, levels = c("00-14", "15-29", "30-44", "1H-OT", "45-59", "60-74", "75-89", "2H-OT")),
          TRUE ~ as.factor(level)
        )
      ) %>%
      pull(level) %>%
      unique()
    
    updateSelectInput(session, "level", choices = levels, selected = levels[1])
  })
  
  output$periodPlot <- renderPlot({
    plot_1 <- factor_props %>%
      filter(cluster == input$cluster, variable == "MATCHPERIOD2") %>%
      mutate(level = factor(level, levels = c("00-14", "15-29", "30-44", "1H-OT", "45-59", "60-74", "75-89", "2H-OT")))
    
    ggplot(plot_1, aes(x = level, y = percentage, fill = level)) +
      geom_col() +
      scale_fill_discrete(name = "Faktorværdier") +
      labs(title = paste("Andel af forskellige kampintervaller i Klynge", input$cluster),
        x = "Faktor", y = "Andel (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$highPlot <- renderPlot({
    plot_2 <- factor_props %>%
      filter(cluster == input$cluster, variable == "HEIGHT") %>%
      mutate(level = recode(level, "high" = "høj", "no" = "ikke høj"))
    
    ggplot(plot_2, aes(x = level, y = percentage, fill = level)) +
      geom_col() +
      scale_fill_discrete(name = "Faktorværdier") +
      labs(title = paste("Andel af høj afleveringeri Klynge", input$cluster),
           x = "Faktor", y = "Andel (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$positionPlot <- renderPlot({
    plot_3 <- factor_props %>%
      filter(cluster == input$cluster, variable == "ROLENAME") %>%
      mutate(level = recode(level,
                            "Defender" = "Forsvar",
                            "Midfielder" = "Midbane",
                            "Forward" = "Angreb",
                            "Goalkeeper" = "Målmænd"),
             level = factor(level, levels = c("Målmænd", "Forsvar", "Midbane", "Angreb")))
    
    ggplot(plot_3, aes(x = level, y = percentage, fill = level)) +
      geom_col() +
      scale_fill_discrete(name = "Faktorværdier") +
      labs(title = paste("Andel af postioner i Klynge", input$cluster),
           x = "Faktor", y = "Andel (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$arrowPlot <- renderPlot({
    data <- cluster_summary_7 %>% filter(cluster == input$cluster)
    
    ggplot() +
      annotate_pitch(dimensions = pitch_opta, fill = "forestgreen", colour = "white") +
      geom_circle(aes(x0 = data$LOCATIONX_mean, y0 = data$LOCATIONY_mean, 
                      r = pmax(data$LOCATIONX_sd, data$LOCATIONY_sd)), 
                  fill = "red", alpha = 0.3) +
      geom_circle(aes(x0 = data$ENDLOCATIONX_mean, y0 = data$ENDLOCATIONY_mean, 
                      r = pmax(data$ENDLOCATIONX_sd, data$ENDLOCATIONY_sd)), 
                  fill = "blue", alpha = 0.3) +
      geom_point(aes(x = data$LOCATIONX_mean, y = data$LOCATIONY_mean), size = 4, color = "red") +
      geom_point(aes(x = data$ENDLOCATIONX_mean, y = data$ENDLOCATIONY_mean), size = 4, color = "blue") +
      geom_segment(aes(x = data$LOCATIONX_mean, y = data$LOCATIONY_mean, 
                       xend = data$ENDLOCATIONX_mean, yend = data$ENDLOCATIONY_mean), 
                   arrow = arrow(length = unit(0.2, "inches")), size = 1, color = "black") +
      labs(title = paste("Gennemsnitlig start- og slutlokation for afleveringer for klynge", input$cluster,"
                         samt circle til at vise standard afvigelse for lokationer")) +
      coord_fixed() +
      theme_pitch()
  })

  output$heatmapPlot <- renderPlot({
    data2 <- passes_clustered_7 %>% 
      filter(cluster == input$cluster) %>%
      mutate(
        mid_x = (LOCATIONX + ENDLOCATIONX) / 2,
        mid_y = (LOCATIONY + ENDLOCATIONY) / 2
      )
    
    ggplot(data2) +
      annotate_pitch(dimensions = pitch_opta, fill = "forestgreen", colour = "white") +
      stat_density_2d(
        aes(x = mid_x, y = mid_y, fill = after_stat(density)), 
        geom = "raster", contour = FALSE, alpha = 0.7
      ) +
      scale_fill_viridis_c(name = "Tæthed") +
      labs(title = paste("Tæthedskort af afleveringer i klynge", input$cluster)) +
      coord_fixed() +
      theme_pitch()+
      theme(legend.position = "none")
  })
  
  output$summaryTable <- renderTable({
    cluster_summary_7 %>%
      filter(cluster == input$cluster) %>%
      mutate(`Andel af afleveringer` = Count / 287158 * 100) %>%
      transmute(
        `Andel af afleveringer` = round(`Andel af afleveringer`, 1),
        `Gennemsnitlig vinkel` = ANGLE_mean,
        `Standard afvigelse for vinkel` = ANGLE_sd,
        `Gennemsnitlig længde` = LENGTH_mean,
        `Standard afvigelse for længde` = LENGTH_sd,
        `Andel af succesfulde afleveringer` = ACCURATE_pct_true*100,
        `Andel af hoverafleveringer` = head_pass_pct_true*100,
        `Andel af generobringer` = recovery_pct_true*100
      )
  })
  
  output$variablePlot <- renderPlot({
    data3 <- factor_props %>%
      mutate(
        variable = recode(variable,
                          "MATCHPERIOD2" = "Kampintervaller",
                          "HEIGHT" = "Højde",
                          "ROLENAME" = "Positioner"),
        level = case_when(
          variable == "Højde" ~ recode(level, "high" = "høj", "no" = "ikke høj"),
          variable == "Positioner" ~ recode(level,
                                            "Defender" = "Forsvar",
                                            "Midfielder" = "Midbane",
                                            "Forward" = "Angreb",
                                            "Goalkeeper" = "Målmænd"
          ),
          TRUE ~ level
        ),
        level = case_when(
          variable == "Positioner" ~ factor(level, levels = c("Målmænd", "Forsvar", "Midbane", "Angreb")),
          variable == "Kampintervaller" ~ factor(level, levels = c("00-14", "15-29", "30-44", "1H-OT", "45-59", "60-74", "75-89", "2H-OT")),
          TRUE ~ as.factor(level)
        )
      ) %>% 
      filter(level == input$level)
    
    ggplot(data3, aes(x = as.factor(cluster), y = pct_from_total, fill = as.factor(cluster))) +
      geom_col() +
      labs(
        x = "Klynge",
        y = "Andel (%)",
        title = paste(input$level, "afleveringer fra variabel", input$variable2, "fordelt i klynger")) +
      scale_fill_discrete(name = "Klynge") +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$clusterTitle <- renderText({
    titles <- c(
      "1" = "Lave afleveringer fra midtbane- og angrebsspillere primært i anden halvleg",
      "2" = "Korte fremadrettede afleveringer på højre fløj",
      "3" = "Meget præcise afleveringer med mange generobringer fra forsvarsspillere og målmænd på højre fløj",
      "4" = "Meget præcise forsvarsafleveringer på venstre fløj (den mest almindelige type)",
      "5" = "Lange høje upræcise afleveringer med mange hovedafleveringer, primært mod slutningen af kampen",
      "6" = "Ekstreme lange afleveringer på tværs af banen",
      "7" = "Korte afleveringer fra midtbane og angreb på venstre fløj"
    )
    titles[as.character(input$cluster)]
  })
  
  output$locationPlot <- renderPlot({
    ggplot() +
      annotate_pitch(dimensions = pitch_opta, fill = "forestgreen", colour = "white") +
      geom_circle(data = cluster_summary_7, aes(x0 = LOCATIONX_mean, y0 = LOCATIONY_mean, 
                                   r = pmax(LOCATIONX_sd, LOCATIONY_sd), fill = as.factor(cluster)), 
                  alpha = 0.3, color = NA) +
      geom_circle(data = cluster_summary_7, aes(x0 = ENDLOCATIONX_mean, y0 = ENDLOCATIONY_mean, 
                                   r = pmax(ENDLOCATIONX_sd, ENDLOCATIONY_sd), fill = as.factor(cluster)), 
                  alpha = 0.3, color = NA) +
      geom_point(data = cluster_summary_7, aes(x = LOCATIONX_mean, y = LOCATIONY_mean, color = as.factor(cluster)), size = 4) +
      geom_point(data = cluster_summary_7, aes(x = ENDLOCATIONX_mean, y = ENDLOCATIONY_mean, color = as.factor(cluster)), size = 4) +
      geom_segment(data = cluster_summary_7, aes(x = LOCATIONX_mean, y = LOCATIONY_mean, 
                                    xend = ENDLOCATIONX_mean, yend = ENDLOCATIONY_mean, color = as.factor(cluster)), 
                   arrow = arrow(length = unit(0.2, "inches")), size = 1) +
      labs(title = "Gennemsnitlig start- og slutlokation samt standard afvigelse af afleveringer") +
      scale_fill_discrete(name = "Klynge") +
      coord_fixed() +
      theme_pitch() +
      guides(color = "none")
  })
  
  output$lengthPlot <- renderPlot({
    ggplot(cluster_summary_7, aes(x = as.factor(cluster), y = LENGTH_mean, fill = as.factor(cluster))) +
      geom_col() +
      geom_errorbar(aes(ymin = LENGTH_mean - LENGTH_sd, ymax = LENGTH_mean + LENGTH_sd),
                    width = 0.2, color = "gray20") +
      labs(x = "Klynge", y = "Længde (gennemsnit ± standrad afvigelse)", title = "Gennemsnitlig længde per klynge") +
      scale_fill_discrete(name = "Klynge") +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$anglePlot <- renderPlot({
    data4 <- cluster_summary_7 %>%
      mutate(
        angle_min = (ANGLE_mean - ANGLE_sd) * pi / 180,
        angle_max = (ANGLE_mean + ANGLE_sd) * pi / 180,
        angle_dir = ANGLE_mean * pi / 180,
        cluster = as.factor(cluster)
      )
    
    ggplot(data4) +
      geom_arc_bar(
        aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
            start = angle_min, end = angle_max,
            fill = cluster),
        alpha = 0.3) +
      geom_arc_bar(
        aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
            start = angle_dir - 0.01, end = angle_dir + 0.01,
            fill = cluster),
        alpha = 1) +
      coord_fixed() +
      facet_wrap(~cluster, ncol = 4) +
      theme_void() +
      labs(title="Gennemsnitlig vinkel (± standard afvigelse) af afleveringer fordelt i klynger",
           subtitle = "Vinkler er tegnet som de ligger på en fodboldbane-plot") +
      theme(legend.position = "none")
  })
  
  output$accuracyPlot <- renderPlot({
    ggplot(cluster_summary_7, aes(x = as.factor(cluster), y = ACCURATE_pct_true, fill = as.factor(cluster))) +
      geom_col() +
      labs(title = "Andel af succesfulde afleveringer", x = "Klynge", y = "Præcision (%)") +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$headPlot <- renderPlot({
    ggplot(cluster_summary_7, aes(x = as.factor(cluster), y = head_pass_pct_true, fill = as.factor(cluster))) +
      geom_col() +
      labs(title = "Andel af hovedafleveringer", x = "Klynge", y = "Hovedafleveringer (%)") +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$recoveryPlot <- renderPlot({
    ggplot(cluster_summary_7, aes(x = as.factor(cluster), y = recovery_pct_true, fill = as.factor(cluster))) +
      geom_col() +
      labs(title = "Andel af succesfulde generobringer", x = "Klynge", y = "Generobring (%)") +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$clusterPlot <- renderPlot({
    kamp1 <- passes_clustered_7 %>%
      filter(game == input$kamp) %>%
      mutate(Total = n()) %>%
      group_by(cluster, Total) %>%
      summarize(Andel = n() / Total, .groups = "drop")
    
    ggplot(kamp1, aes(x = as.factor(cluster), y = Andel, fill = as.factor(cluster))) +
      geom_col() +
      labs(x = "Klynge", y = "Andel (%)", title = paste("Fordeling af afleveringer i", input$kamp)) +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$allPlot <- renderPlot({
    kamp2 <- passes_clustered_7 %>% 
      group_by(cluster) %>% 
      summarize(Andel = n() / 286158 * 100)
    
    ggplot(kamp2, aes(x = as.factor(cluster), y = Andel, fill = as.factor(cluster))) +
      geom_col() +
      labs(x = "Klynge", y = "Andel (%)", title = paste("Fordeling af afleveringer i alt")) +
      theme_minimal() +
      guides(fill = "none")
  })
}

shinyApp(ui, server)

