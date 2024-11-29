library(shiny)
library(ggplot2)
library(dplyr)
library(haven)

# Read the lab data
adlbc <- read_xpt("data/adlbc.xpt")

# Define the UI
ui <- fluidPage(
  titlePanel("Lab Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "param",
        label = "Select Lab Variable (PARAM):",
        choices = unique(adlbc$PARAM),
        selected = unique(adlbc$PARAM)[1]
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "linePlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Render the line plot
  output$linePlot <- renderPlot({
    # Filter data based on selected PARAM
    filtered_data <- adlbc %>% filter(PARAM == input$param)
    
    # Summarize data to calculate mean AVAL for each VISIT
    summarized_data <- filtered_data %>%
      group_by(VISIT) %>%
      summarise(mean_AVAL = mean(AVAL, na.rm = TRUE)) %>%
      arrange(VISIT)
    
    # Create the line plot
    ggplot(summarized_data, aes(x = VISIT, y = mean_AVAL)) +
      geom_line(group = 1, color = "blue") +
      geom_point(color = "blue") +
      theme_minimal() +
      labs(
        title = paste("Mean AVAL for", input$param),
        x = "Visit",
        y = "Mean AVAL"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

