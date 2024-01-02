library(shiny)

ui <- fluidPage(
  titlePanel("Occupation, Stress, and Sleep Quality"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("occupation", "Select Occupation", choices = c(unique(data$Occupation), "Everyone"), selected = "Everyone"),
      selectInput("gender", "Select Gender", choices = c(unique(data$Gender), "Everyone"), selected = "Everyone"),
      sliderInput("age_range", "Select Age Range", min = min(data$Age), max = max(data$Age), value = c(min(data$Age), max(data$Age)))
      
    ),
    
    
    mainPanel(
      verbatimTextOutput("columns"),
      plotOutput("plot"),
      textOutput("lowest_quality_occupation_view")
      
    )
  )
)