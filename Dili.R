library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(haven)
library(ggplot2)
library(tidyr)
library(plotly)


adlb <- read_xpt("data/adlbc.xpt")

adlb <- adlb %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI"))

adlb <- adlb %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI")) %>%
  mutate(
    # Combine Xanomeline doses into a single category
    TRTA = case_when(
      TRTA %in% c("Xanomeline High Dose", "Xanomeline Low Dose") ~ "Xanomeline",
      TRUE ~ TRTA
    )
  )

adlb_annotated <- adlb %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI")) %>% 
  mutate(
    # Calculate the ratio AVAL/ULN using A1HI as the reference
    AVAL_ULN = AVAL / A1HI,
    
    # Define CRIT1 descriptions
    CRIT1 = case_when(
      PARAMCD == "AST" ~ "AST >=3xULN",
      PARAMCD == "ALT" ~ "ALT >=3xULN",
      PARAMCD == "BILI" ~ "BILI >=2xULN"
    ),
    
    # Define flags based on criteria
    CRIT1FL = if_else(
      (AVAL_ULN >= 3 & PARAMCD %in% c("AST", "ALT")) |
        (AVAL_ULN >= 2 & PARAMCD == "BILI"),
      "Y",
      NA_character_
    )
  ) %>%
  # Group by USUBJID and PARAMCD
  group_by(USUBJID, PARAMCD) %>%
  # For duplicates, select the row with the earliest ADY
  filter(
    AVAL_ULN == max(AVAL_ULN, na.rm = TRUE) &
      ADY == min(ADY[AVAL_ULN == max(AVAL_ULN, na.rm = TRUE)], na.rm = TRUE)
  ) %>%
  ungroup() %>%  # Ungroup after filtering
  select(STUDYID, USUBJID, TRTA, PARAMCD, LBSEQ, ADT, AVISIT, ADY, AVAL, A1HI, AVAL_ULN, CRIT1, CRIT1FL)

# # Calculate peaks for ALT/ULN and BILI/ULN
# peak_data <- adlb_annotated %>%
#   #filter(CRIT1FL == "Y") %>%
#   group_by(USUBJID) %>%
#   summarize(
#     Peak_ALT_ULN = max(ifelse(PARAMCD %in% c("ALT", "AST"), AVAL / A1HI, NA), na.rm = TRUE),
#     Peak_BILI_ULN = max(ifelse(PARAMCD == "BILI", AVAL / A1HI, NA), na.rm = TRUE),
#     .groups = "drop"
#   )
# Create a new column to flag the highest AVAL value for PARAMCD == "ALT"
adlb_flagged <- adlb_annotated %>%
  group_by(USUBJID) %>%
  mutate(
    ALT_MAX_FLAG = if_else(PARAMCD == "ALT" & AVAL == max(AVAL[PARAMCD == "ALT"], na.rm = TRUE), "Y", "N")
  ) %>%
  ungroup()

alt_max_subjects <- adlb_flagged %>%
  filter(ALT_MAX_FLAG == "Y")

# Join to get BILI and AST values from the same AVISIT and USUBJID
subset_data <- adlb_annotated %>%
  semi_join(alt_max_subjects, by = c("USUBJID", "AVISIT")) %>%
  filter(PARAMCD %in% c("ALT", "BILI", "AST")) %>%
  arrange(USUBJID, AVISIT, PARAMCD)

ggplot(subset_data)

# Prepare the data for plotting (use AVAL_ULN instead of AVAL)
scatter_data <- adlb_annotated %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI")) %>%  # Keep AST, ALT, and BILI rows
  select(USUBJID, TRTA, AVISIT, PARAMCD, AVAL_ULN) %>%  # Retain TRTA and AVISIT
  pivot_wider(
    names_from = PARAMCD,  # Use PARAMCD values as column names
    values_from = AVAL_ULN  # Use AVAL_ULN values as the cell values
  ) %>%
  filter(!is.na(ALT) & !is.na(BILI))  # Keep rows where both ALT and BILI are present


# Create the scatter plot using ggplot2
plot <- ggplot(scatter_data, aes(x = ALT, y = BILI)) +
  geom_point() +  # Add scatter points
  geom_hline(yintercept = 2, linetype = "dotted", color = "red") +  # Add horizontal reference line at y = 2
  geom_vline(xintercept = 3, linetype = "dotted", color = "red") +  # Add vertical reference line at x = 3
  labs(
    title = "Scatter Plot of ALT vs BILI",
    x = "Alanine Aminotransferase (U/L) [xULN]",
    y = "Bilirubin (umol/L) [xULN]"
  ) +
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +  # Set x-axis range and tick marks
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +  # Set y-axis range and tick marks
  # Add labels for quadrants
  annotate("text", x = 4.5, y = 4.5, label = "Potential Hys Law", color = "blue", size = 5, hjust = 0.5) +  # Upper right
  annotate("text", x = 1.5, y = 4.5, label = "Hyperbilirubinemia", color = "blue", size = 5, hjust = 0.5) +  # Upper left
  annotate("text", x = 4.5, y = 0.5, label = "Temple's Corollary", color = "blue", size = 5, hjust = 0.5) +  # Lower right
  theme_minimal()  # Use a clean theme


# Render the plot
print(plot)


# Render the plot
print(plot)
colnames(adlb_annotated)
