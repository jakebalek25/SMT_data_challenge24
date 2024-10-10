library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(rsconnect)

set.seed(147)

main_4A <- read.csv("data/Team1-data.csv")


ui <- fluidPage(
  titlePanel("Pre-Pitch Movement Display"),
  
  tabsetPanel(id = "tabs",
              tabPanel("Pre-Pitch Movement by Shortstop",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("shortstop", "Select Shortstop:", choices = NULL),
                           uiOutput("dynamic_game_str"),
                           numericInput("min_play_id", "Minimum Play ID:", value = 1),
                           numericInput("max_play_id", "Maximum Play ID:", value = 1),
                           selectInput("plotType", "Select Plot Type:",
                                       choices = c("pitch_type" = "pitch_type", "pitch_location_io" = "pitch_location_io"))
                         ),
                         mainPanel(
                           plotOutput("leftyPlot"),
                           plotOutput("rightyPlot")
                         )
                       )
              ),
              tabPanel("Pre-Pitch Postioning Heatmap",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("shortstop_heatmap", "Select Shortstop for Heatmap:", choices = NULL),
                           selectInput("heatmap_filter_type", "Select Filter Type:", 
                                       choices = c("pitch_location_io" = "pitch_location_io", "pitch_type" = "pitch_type")),
                           uiOutput("heatmap_filter_value_ui")
                         ),
                         mainPanel(
                           plotOutput("heatmapPlot")
                         )
                       )
              )
  )
)


server <- function(input, output, session) {
  
  observe({
    shortstop_choices <- unique(as.character(main_4A$shortstop))
    updateSelectInput(session, "shortstop", choices = shortstop_choices)
    updateSelectInput(session, "shortstop_heatmap", choices = shortstop_choices)
  })
  
  output$dynamic_game_str <- renderUI({
    req(input$shortstop)
    
    shortstop_data <- main_4A %>% filter(as.character(shortstop) == input$shortstop)
    
    valid_game_strs <- shortstop_data %>%
      group_by(game_str) %>%
      filter(event_code == "pre_pitch") %>%
      summarise(
        lefty_count = sum(handedness == "lefty", na.rm = TRUE),
        righty_count = sum(handedness == "righty", na.rm = TRUE)
      ) %>%
      filter(lefty_count > 0 & righty_count > 0) %>%
      pull(game_str)
    
    if (length(valid_game_strs) > 0) {
      selectInput("game_str", "Select Game String:", choices = valid_game_strs)
    } else {
      selectInput("game_str", "Select Game String:", choices = NULL)
    }
  })
  
  observe({
    req(input$shortstop, input$game_str)
    
    game_str_data <- main_4A %>%
      filter(as.character(shortstop) == input$shortstop, game_str == input$game_str) %>%
      filter(event_code == "pre_pitch")
    
    min_play_id <- 1
    max_play_id <- max(game_str_data$play_id, na.rm = TRUE)
    
    updateNumericInput(session, "min_play_id", value = min_play_id)
    updateNumericInput(session, "max_play_id", value = max_play_id)
  })
  
  output$leftyPlot <- renderPlot({
    req(input$shortstop, input$game_str, input$min_play_id, input$max_play_id)
    
    filtered_data <- main_4A %>%
      filter(
        as.character(shortstop) == input$shortstop, 
        event_code == "pre_pitch",
        game_str == input$game_str, 
        handedness == "lefty",
        play_id >= input$min_play_id, 
        play_id <= input$max_play_id,
        !is.na(pitch_type)  
      )
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    plot <- ggplot(filtered_data, aes(x = field_x, y = field_y)) +
      geom_point(aes(color = pitch_type)) +
      labs(title = paste("Lefty Batters, Shortstop:", input$shortstop),
           x = "Shortstop Position X",
           y = "Shortstop Position Y") +
      theme_minimal()
    
    plot
  })
  
  output$rightyPlot <- renderPlot({
    req(input$shortstop, input$game_str, input$min_play_id, input$max_play_id)
    
    filtered_data <- main_4A %>%
      filter(
        as.character(shortstop) == input$shortstop, 
        event_code == "pre_pitch",
        game_str == input$game_str, 
        handedness == "righty",
        play_id >= input$min_play_id, 
        play_id <= input$max_play_id,
        !is.na(pitch_type)  
      )
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    plot <- ggplot(filtered_data, aes(x = field_x, y = field_y)) +
      geom_point(aes(color = pitch_location_io)) +
      labs(title = paste("Righty Batters, Shortstop:", input$shortstop),
           x = "Shortstop Position X",
           y = "Shortstop Position Y") +
      theme_minimal()
    
    plot
  })
  
  output$heatmap_filter_value_ui <- renderUI({
    req(input$shortstop_heatmap)
    
    heatmap_data <- main_4A %>% filter(shortstop == input$shortstop_heatmap)
    
    if (input$heatmap_filter_type == "pitch_type") {
      choices <- unique(heatmap_data$pitch_type)
    } else if (input$heatmap_filter_type == "pitch_location_io") {
      choices <- unique(heatmap_data$pitch_location_io)
    } else {
      choices <- NULL
    }
    
    selectInput("heatmap_filter_value", "Select Filter Value:", choices = choices)
  })
  
  output$heatmapPlot <- renderPlot({
    req(input$shortstop_heatmap, input$heatmap_filter_type, input$heatmap_filter_value)
    
    filtered_data <- main_4A %>%
      filter(
        shortstop == input$shortstop_heatmap,  
        if (input$heatmap_filter_type == "pitch_type") {
          pitch_type == input$heatmap_filter_value
        } else {
          pitch_location_io == input$heatmap_filter_value
        }
      )
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    plot <- ggplot(filtered_data, aes(x = field_x, y = field_y)) +
      geom_bin2d(bins = 30) +
      scale_fill_viridis_c() +
      labs(title = paste("Heatmap for Shortstop:", input$shortstop_heatmap,
                         "Filter:", input$heatmap_filter_value),
           x = "Ball Position X",
           y = "Ball Position Y") +
      theme_minimal()
    
    infield <- data.frame(
      x = c(0, -63, 0, 63, 0),
      y = c(0, 63, 126, 63, 0)
    )
    
    baselines <- data.frame(
      x = c(0, -63, -160, 0, 63, 160),
      y = c(0, 63, 175, 0, 63, 175)
    )
    
    plot + 
      geom_polygon(data = infield, aes(x = x, y = y), fill = NA, color = "black") +
      geom_line(data = baselines, aes(x = x, y = y), color = "black")
  })
}

shinyApp(ui = ui, server = server)


