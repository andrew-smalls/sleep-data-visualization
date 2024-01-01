library(shiny)
library(ggplot2)
library(dplyr)

library(grid)
library(png)
library(forcats)


# Analyze first, show the important, zoom/filter, analyze further, details
# on demand

data <- read.csv("dataset.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Occupation, Stress, and Sleep Quality"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("occupation", "Select Occupation", choices = c(unique(data$Occupation), "Everyone"), selected = "Everyone")
    ),
    
    mainPanel(
      verbatimTextOutput("columns"),
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$columns <- renderPrint({
    names(data)
  })
  
  rescale <- function(x, new_min, new_max) {
    old_min <- min(x, na.rm = TRUE)
    old_max <- max(x, na.rm = TRUE)
    
    new_range <- new_max - new_min
    old_range <- old_max - old_min
    
    ((x - old_min) * new_range) / old_range + new_min
  }
  
  # Rescale 'Stress.Level' and 'Quality.of.Sleep' to new ranges (1-7)
  data$Stress.Level <- rescale(data$Stress.Level, 1, 6)
  data$Quality.of.Sleep <- rescale(data$Quality.of.Sleep, 1, 6)
  
  filtered_data <- reactive({
    filter_data <- data
    
    if (!is.null(input$occupation)) {
      if (input$occupation != "Everyone") {
        filter_data <- filter(filter_data, Occupation == input$occupation)
      }
    }
    

    return(filter_data)
  })
  
  total_people <- reactive({
    nrow(filtered_data())
  })
  
  sleep_counts <- reactive({
    filtered_data() %>% 
      count(Quality.of.Sleep)
  })  
  
  sleep_stress_counts <- reactive({
    filtered_data() %>% 
      count(Quality.of.Sleep, Stress.Level)
  })
  
  output$plot <- renderPlot({
  stress_colors <- c("1" = "lightgreen", "2" = "yellowgreen", "3" = "gold",
                     "4" = "orange", "5" = "tomato", "6" = "red")
  
  
  sleep_stress_counts_plot <- 
    ggplot(sleep_stress_counts(), aes(x = Quality.of.Sleep, y = n, fill = fct_rev(factor(Stress.Level)))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = stress_colors, 
                      name = "Stress Level",
                      breaks = c("1", "2", "3", "4", "5", "6"),
                      labels = c("Very Low Stress", "Low Stress", "Moderate Stress", 
                                 "High Stress", "Very High Stress", "Highest Stress")) +
    labs(title = paste("Quality of Sleep segmented by Stress Level for", input$occupation)) +
    theme_minimal()
  
  return(sleep_stress_counts_plot)
  })
}

shinyApp(ui = ui, server = server)
