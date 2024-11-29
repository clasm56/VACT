library(clinDataReview)
library(plotly)
library(clinUtils)
library(haven)

# Load data
adlbc <- read_xpt("data/adlbc.xpt")

# Subset data for ALT and BILI
dataALT <- subset(adlbc, PARAMCD == "ALT")
dataBILI <- subset(adlbc, PARAMCD == "BILI")

# Merge the two datasets
byVar <- c("USUBJID", "VISIT")
dataPlot <- merge(
  x = dataALT, 
  y = dataBILI[, c(byVar, "LBSTRESN")], 
  by = c("USUBJID", "VISIT"), 
  suffixes = c(".ALT", ".BILI"),
  all = TRUE
)

# Initialize labelVars as a list
labelVars <- list()

# Add labels for ALT and BILI
labelVars[paste0("LBSTRESN.", c("ALT", "BILI"))] <- paste(
  "Actual value of", 
  getLabelParamcd(
    paramcd = c("ALT", "BILI"), 
    data = adlbc, 
    paramcdVar = "PARAMCD", 
    paramVar = "PARAM"
  )
)

# Link to patient profiles if interactive session
if (interactive()) {
  dataPlot$patientProfilePath <- paste0(
    "patientProfiles/subjectProfile-", 
    sub("/", "-", dataPlot$USUBJID), ".pdf"
  )
}

# Create scatterplot per visit
scatterplotClinData(
  data = dataPlot, 
  xVar = "LBSTRESN.ALT", 
  yVar = "LBSTRESN.BILI",
  xLab = getLabelParamcd(
    paramcd = "ALT", 
    data = adlbc, 
    paramcdVar = "PARAMCD", 
    paramVar = "PARAM"
  ),
  yLab = getLabelParamcd(
    paramcd = "BILI", 
    data = adlbc, 
    paramcdVar = "PARAMCD", 
    paramVar = "PARAM"
  ),
  aesPointVar = list(color = "VISIT", fill = "VISIT"),
  xTrans = "log10", 
  yTrans = "log10",
  hoverVars = "USUBJID",
  labelVars = labelVars,
  table = FALSE, 
  pathVar = if (interactive()) "patientProfilePath"
)
