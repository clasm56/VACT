library(shiny)
library(dplyr)
library(haven)
library(ggplot2)

# Load dataset
adsl <- read_xpt("data/adsl.xpt")

# Define UI
ui <- fluidPage(
  titlePanel("Subject Counts by Race"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "mmse_range",
        "Select MMSE Range:",
        min = min(adsl$MMSETOT, na.rm = TRUE),
        max = max(adsl$MMSETOT, na.rm = TRUE),
        value = c(min(adsl$MMSETOT, na.rm = TRUE), max(adsl$MMSETOT, na.rm = TRUE))
      ),
      radioButtons(
        "stack_var",
        "Stack By:",
        choices = list(
          "None (Race Totals)" = "none",
          "Dose (TRT01A)" = "TRT01A",
          "Age Group (AGEGR1)" = "AGEGR1",
          "BMI Group (BMIBLGR1)" = "BMIBLGR1"
        ),
        selected = "none"
      )
    ),
    mainPanel(
      plotOutput("race_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    adsl %>%
      filter(MMSETOT >= input$mmse_range[1], MMSETOT <= input$mmse_range[2])
  })
  
  output$race_plot <- renderPlot({
    plot_data <- filtered_data()
    
    if (input$stack_var == "none") {
      # Group by Race only for totals
      plot_data <- plot_data %>%
        group_by(RACE) %>%
        summarize(Count = n(), .groups = "drop")
      
      ggplot(plot_data, aes(x = RACE, y = Count)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(
          title = "Counts of Subjects by Race",
          x = "Race",
          y = "Count"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Group by Race and the selected variable for stacking
      plot_data <- plot_data %>%
        group_by(RACE, .data[[input$stack_var]]) %>%
        summarize(Count = n(), .groups = "drop")
      
      ggplot(plot_data, aes(x = RACE, y = Count, fill = .data[[input$stack_var]])) +
        geom_bar(stat = "identity", position = "stack") +
        labs(
          title = paste("Counts of Subjects by Race (Stacked by", input$stack_var, ")"),
          x = "Race",
          y = "Count",
          fill = input$stack_var
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


