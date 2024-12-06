library(gt)
library(dplyr)
adae <- as.data.frame(read_xpt("Data/adae.xpt"))
adae <- as.data.frame(read_xpt("Data/adae.xpt"))

# Data summary
summary_table <- adae %>% 
  group_by(AEBODSYS, TRTA, AESEV, AGEGR1) %>% 
  summarise(N = n(), .groups = "drop") %>% 
  gt()

# Print the GT table
summary_table

  #   
  #   Count = n(), 
  #   .groups = "drop"
  # )

#GT table 
gt_table <- summary_table %>% 
  gt() %>% 
  tab_header(
    title = "Grouped Summary Table", 
    subtitle = "Place Holder"
  ) %>% 
  #row_group_order(sort(unique(summary_table$AEBODSYS))) %>% 
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body(
      columns = vars(AEBODSYS, TRTA, AESEV), 
      rows = duplicated(data$AEBODSYS) | duplicated(data$TRTA) | duplicated(data$AESEV)
    )
                
  )

gt_table

###EX2 
summary_table <- adae %>%
  group_by(AEBODSYS, TRTA, AESEV, AGEGR1) %>%
  summarise(Count = sum(Count), .groups = "drop")



# Create the GT table
gt_table <- summary_table %>%
  gt(groupname_col = "AEBODSYS") %>%
  tab_header(
    title = "Adverse Events by Body System",
    subtitle = "Grouped by Treatment, Severity, and Age Group"
  ) %>%
  cols_label(
    TRTA = "Treatment",
    AESEV = "Severity",
    AGEGR1 = "Age Group",
    Count = "Frequency"
  )

# Print the GT table
gt_table