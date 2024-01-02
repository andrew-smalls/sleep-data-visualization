library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)

library(grid)
library(png)
library(forcats)

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
  
  # Rescale 'Stress.Level' and 'Quality.of.Sleep' to new ranges (1-6)
  data$Stress.Level <- rescale(data$Stress.Level, 1, 6)
  data$Quality.of.Sleep <- rescale(data$Quality.of.Sleep, 1, 6)
  
  filtered_data <- reactive({
    filter_data <- data
    
    if (!is.null(input$occupation)) {
      if (input$occupation != "Everyone") {
        filter_data <- filter(filter_data, Occupation == input$occupation)
      }
    }
    
    
    if (!is.null(input$gender)) {
      if (input$gender != "Everyone") {
        filter_data <- filter(filter_data, Gender == input$gender)
      }
    }
    
    age_range <- input$age_range
    if (!is.null(input$age_range)) {
      filter_data <- filter(filter_data, Age >= age_range[1] & Age <= age_range[2])
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
  
  lowest_quality_occupation <- reactive({
    filtered_data <- filtered_data()
    
    if (is.null(filtered_data())) {
      return(NULL)  # Return NULL if no data after filtering
    }
    
    avg_quality_by_occupation <- filtered_data %>%
      group_by(Occupation) %>%
      summarise(avg_quality_sleep = mean(Quality.of.Sleep, na.rm = TRUE)) %>%
      arrange(avg_quality_sleep)  # Arrange by ascending average quality of sleep
    
    # Extract the occupation with the lowest average quality of sleep
    lowest_quality_occupation <- avg_quality_by_occupation$Occupation[1]  # Get the first (lowest) occupation
    
    # Extract the lowest average quality of sleep value
    lowest_avg_quality_sleep <- avg_quality_by_occupation$avg_quality_sleep[1]  # Get the lowest average quality of sleep
    
    return(list(occupation = lowest_quality_occupation, avg_quality_sleep = lowest_avg_quality_sleep))
    
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
      labs(title = paste("Quality of Sleep segmented by Stress Level for", input$occupation),
           y = "Number of People", 
           x = "Sleep Quality"
           ) +
      theme_minimal()
    
    return(sleep_stress_counts_plot)
  })
  
  output$lowest_quality_occupation_view <- renderText({
    lowest_occupation <- lowest_quality_occupation()
    
    # Display the occupation with the lowest quality of sleep
    if (!is.null(lowest_occupation)) {
      paste("Occupation with the lowest average quality of sleep:", lowest_occupation$occupation,
            "with an average quality of sleep:", round(lowest_occupation$avg_quality_sleep, 2))
    } else {
      "No data available"
    }
  })
}