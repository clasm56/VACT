library(dplyr)
library(flextable)
library(haven)

adae <- read_xpt("Data/adae.xpt")

# Assuming your data is loaded in a data frame named `adae`
summary_table <- adae %>%
  filter(
    AEBODSYS %in% c(
      "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", 
      "NERVOUS SYSTEM DISORDERS", 
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
    )
  ) %>%
  group_by(AEBODSYS, TRTA, AESEV, AGEGR1) %>%
  summarise(N = n(), .groups = "drop")

# Create a flextable with AEBODSYS at the top and merged rows
flex_table <- flextable(summary_table) %>%
  merge_v(j = "AEBODSYS") %>%  # Merge the AE Body System column
  set_header_labels(
    AEBODSYS = "AE Body System",
    TRTA = "Treatment Arm",
    AESEV = "AE Severity",
    AGEGR1 = "Age Group",
    N = "Count"
  ) %>%
  theme_box() %>%             # Apply a simple boxed theme
  autofit()                   # Automatically adjust column widths

# Print the flextable
flex_table