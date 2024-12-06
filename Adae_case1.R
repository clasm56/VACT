library(haven)
library(ggplot2)
library(patchwork)
library(dplyr)
library(forcats)
library(shiny)

# Read in your adae dataset
adae <- read_xpt("data/adae.xpt")

# Shiny app
ui <- fluidPage(
  titlePanel("Adverse Events Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group_var", "Group by:",
                  choices = c("None", "AGEGR1", "RACE", "AESEV"), 
                  selected = "None"),
      width = 3
    ),
    mainPanel(
      plotOutput("barplot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  # Render bar plot
  output$barplot <- renderPlot({
    # Start with the base dataset
    data <- adae
    
    # Dynamically group the data
    if (input$group_var != "None") {
      data <- data %>%
        group_by(AEBODSYS, TRTA, .data[[input$group_var]]) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      data <- data %>%
        count(AEBODSYS, TRTA, name = "Count")
    }
    
    # Arrange and factorize AEBODSYS for ordering
    data <- data %>%
      arrange(desc(Count)) %>%
      mutate(AEBODSYS = factor(AEBODSYS, levels = unique(AEBODSYS)))
    
    # Create the bar plot
    ggplot(data, aes(x = AEBODSYS, y = Count, fill = TRTA)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(rows = if (input$group_var != "None") vars(.data[[input$group_var]]) else vars(TRTA), 
                 scales = "free_y") +
      theme_minimal() +
      labs(
        title = "Adverse Events by Body System and Treatment",
        x = "Body System (AEBODSYS)",
        y = "Count of Events",
        fill = "Treatment"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
 
# adae <- adae %>% 
#   filter(AEBODSYS == "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" | 
#            AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"|
#            AEBODSYS =="CARDIAC DISORDERS" |
#            AEBODSYS =="RENAL AND URINARY DISORDERS")
# 
# # Reorder AEBODSYS by Count (descending order)
# bar_data <- bar_data %>%
#   mutate(AEBODSYS = fct_reorder(AEBODSYS, Count, .desc = TRUE))
# 
# # Create the plot
# ggplot(bar_data, aes(x = AEBODSYS, y = Count, fill = TRTA)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(rows = vars(TRTA), scales = "free_y") +
#   theme_minimal() +
#   labs(
#     title = "Adverse Events by Body System and Treatment",
#     x = "Body System (AEBODSYS)",
#     y = "Count of Events",
#     fill = "Treatment"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))