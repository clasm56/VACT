# Load ggplot2 library
library(ggplot2)

# Create example data without explicit labels
set.seed(123)
data <- data.frame(
  Category = factor(rep(1:3, each = 50)), # Numeric categories 1, 2, 3
  Value = c(rnorm(50, mean = 9, sd = 1),  # Top position
            rnorm(50, mean = 5, sd = 1.5), # Middle position
            rnorm(50, mean = 2, sd = 1.2)) # Bottom position
)

# Create the violin plot
violin_plot <- ggplot(data, aes(x = Category, y = Value, fill = Category)) +
  geom_violin(trim = FALSE) +
  labs(
    x = NULL,  # No x-axis label
    y = NULL   # No y-axis label
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0, 10)) # Ensures all violins fit well

# Display the plot
print(violin_plot)
