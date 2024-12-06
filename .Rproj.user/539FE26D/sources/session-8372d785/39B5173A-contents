library(shiny)
library(dplyr)
library(ggplot2)
library(haven)

advs<- read_xpt("Data/advs.xpt")
ui <- fluidPage(
  titlePanel("ADVS Systolic Blood Pressure Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y_axis_stat",
        label = "Choose Y-axis statistic:",
        choices = c("Minimum" = "min", "Maximum" = "max", "Mean" = "mean", "Last" = "last")
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatter_plot")
    )
  )
)

server <- function(input, output, session) {
  # Filter data for SYSBP and calculate baseline
  advs_filtered <- reactive({
    advs %>%
      filter(PARAMCD == "SYSBP") %>%
      group_by(USUBJID) %>%
      mutate(Baseline = first(AVAL)) %>%
      ungroup()
  })
  
  # Generate scatter plot
  output$scatter_plot <- renderPlot({
    plot_data <- advs_filtered() %>%
      group_by(USUBJID, TRTA) %>% # Include TRTA for grouping
      summarize(
        Baseline = first(Baseline),
        StatValue = case_when(
          input$y_axis_stat == "min" ~ min(AVAL, na.rm = TRUE),
          input$y_axis_stat == "max" ~ max(AVAL, na.rm = TRUE),
          input$y_axis_stat == "mean" ~ mean(AVAL, na.rm = TRUE),
          input$y_axis_stat == "last" ~ AVAL[which.max(row_number())] # Using row_number() to get the last value
        )
      )
    
    ggplot(plot_data, aes(x = Baseline, y = StatValue, color = TRTA)) +
      geom_point() +
      geom_smooth(method = "lm", aes(group = TRTA), se = FALSE, linetype = "dashed") + # Group-specific trend lines
      geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "solid") +    # Overall trend line
      labs(
        x = "Baseline Systolic BP",
        y = paste(input$y_axis_stat, "Systolic BP"),
        color = "Treatment Group",
        title = "Systolic Blood Pressure Analysis by Treatment Group with Trend Lines"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

# advs_filtered <- advs %>%
#   filter(PARAMCD == "SYSBP") %>%
#   group_by(USUBJID) %>%
#   mutate(Baseline = first(AVAL)) %>%
#   ungroup()

