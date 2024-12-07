library(dplyr)
library(tidyr)
library(haven)
library(gt)


advs<- read_xpt("Data/advs.xpt")

process_advs <- function(advs, statistic) {
  advs %>%
    filter(PARAMCD == "SYSBP") %>%
    group_by(USUBJID, TRTA) %>%
    summarise(SYSBP = case_when(
      statistic == "min" ~ min(AVAL, na.rm = TRUE),
      statistic == "max" ~ max(AVAL, na.rm = TRUE),
      statistic == "mean" ~ mean(AVAL, na.rm = TRUE),
      statistic == "last" ~ tail(AVAL[!is.na(AVAL)], 1) # Corrected last() handling
    ), .groups = "drop") %>%
    mutate(Category = case_when(
      SYSBP < 90 ~ "<90",
      SYSBP >= 90 & SYSBP < 120 ~ ">=90 and <120",
      SYSBP >= 120 & SYSBP < 140 ~ ">=120 and <140",
      SYSBP >= 140 & SYSBP < 160 ~ ">=140 and <160",
      SYSBP >= 160 & SYSBP < 180 ~ ">=160 and <180",
      SYSBP >= 180 ~ ">=180"
    )) %>%
    group_by(Category, TRTA) %>%
    summarise(n = n(),
              pct = round(100 * n / sum(n), 1), .groups = "drop") %>%
    pivot_wider(names_from = TRTA, values_from = c(n, pct), values_fill = 0) %>%
    arrange(factor(Category, levels = c("<90", ">=90 and <120", ">=120 and <140", 
                                        ">=140 and <160", ">=160 and <180", ">=180")))
}

# Apply for all statistics
tables <- list(
  "min" = process_advs(advs, "min"),
  "max" = process_advs(advs, "max"),
  "mean" = process_advs(advs, "mean"),
  "last" = process_advs(advs, "last")
)

# Print tables
names(tables) <- c("Min SYSBP", "Max SYSBP", "Mean SYSBP", "Last SYSBP")
tables



# Display one table using gt
gt_table <- gt(tables[["Min SYSBP"]])
print(gt_table)



###Flextable
library(flextable)
library(officer)

# Function to process `advs` dataset for a specific statistic
process_advs_stat <- function(advs, statistic) {
  # Filter and calculate the statistic for each subject
  advs_summary <- advs %>%
    filter(PARAMCD == "SYSBP") %>%
    group_by(USUBJID, TRTA) %>%
    summarise(
      SYSBP = case_when(
        statistic == "min" ~ min(AVAL, na.rm = TRUE),
        statistic == "max" ~ max(AVAL, na.rm = TRUE),
        statistic == "mean" ~ mean(AVAL, na.rm = TRUE),
        statistic == "last" ~ last(AVAL[!is.na(AVAL)])
      ), .groups = "drop"
    ) %>%
    mutate(
      BP_Category = case_when(
        SYSBP < 90 ~ "<90",
        SYSBP >= 90 & SYSBP < 120 ~ ">=90 and <120",
        SYSBP >= 120 & SYSBP < 140 ~ ">=120 and <140",
        SYSBP >= 140 & SYSBP < 160 ~ ">=140 and <160",
        SYSBP >= 160 & SYSBP < 180 ~ ">=160 and <180",
        SYSBP >= 180 ~ ">=180"
      )
    ) %>%
    group_by(TRTA) %>%
    mutate(total_in_group = n()) %>%
    group_by(BP_Category, TRTA) %>%
    summarise(
      n = n(),
      pct = round(100 * n / first(total_in_group), 1),
      .groups = "drop"
    ) %>%
    mutate(
      `n (%)` = paste0(n, " (", pct, "%)")
    ) %>%
    select(BP_Category, TRTA, `n (%)`) %>%
    pivot_wider(names_from = TRTA, values_from = `n (%)`, values_fill = "0 (0%)")
  
  return(advs_summary)
}

# Generate tables for each statistic
tables <- list(
  "Min SYSBP" = process_advs_stat(advs, "min"),
  "Max SYSBP" = process_advs_stat(advs, "max"),
  "Mean SYSBP" = process_advs_stat(advs, "mean"),
  "Last SYSBP" = process_advs_stat(advs, "last")
)

# Function to create a flextable for a single table with a dynamic header title
create_flextable <- function(summary_table, title) {
  row_labels <- summary_table$BP_Category
  summary_table <- summary_table %>%
    select(-BP_Category)  # Remove BP_Category column for the table body
  
  # Combine row labels into the main table
  merged_table <- cbind(
    `Systolic Blood Pressure (mm Hg)` = row_labels,
    summary_table
  )
  
  # Dynamically rename the first column header
  ft <- flextable(merged_table) %>%
    set_header_labels(
      `Systolic Blood Pressure (mm Hg)` = title
    ) %>%
    align(j = 2:ncol(merged_table), align = "center", part = "all") %>%
    autofit() %>%
    set_caption(paste0(title, ":\n")) %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body", border = fp_border(width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1))
  
  return(ft)
}

# Create flextables with specific headers for each statistic
flextables <- list(
  create_flextable(tables[["Min SYSBP"]], "Min Systolic Blood Pressure (mm Hg)"),
  create_flextable(tables[["Max SYSBP"]], "Max Systolic Blood Pressure (mm Hg)"),
  create_flextable(tables[["Mean SYSBP"]], "Mean Systolic Blood Pressure (mm Hg)"),
  create_flextable(tables[["Last SYSBP"]], "Last Systolic Blood Pressure (mm Hg)")
)

# Display all tables (example for the first one)
flextables[[1]]