library(readxl)
library(dplyr)

# Raw Excel file paths
raw_param1 <- "data/LogisticModelParameters-WithinOneYear.xlsx"
raw_param2 <- "data/LogisticModelParameters-WithinTwoYears.xlsx"
raw_roc1   <- "data/ROC-RefluxResolve-WithinOneYearOfVisit.xlsx"
raw_roc2   <- "data/ROC-RefluxResolve-WithinTwoYearOfVisit.xlsx"

# Function to load model parameters from all sheets
load_params <- function(path) {
  sheets <- excel_sheets(path)
  setNames(
    lapply(sheets, function(s) {
      df <- read_excel(path, sheet = s, range = cell_cols("A:C"), col_names = FALSE, skip = 1)
      names(df) <- c("Parameter", "Level", "Estimate")
      df <- df %>%
        filter(!is.na(Estimate), Parameter != "Parameter")
    }),
    sheets
  )
}

# Function to load 50% specificity cutoffs from all sheets
load_thresholds <- function(path) {
  sheets <- excel_sheets(path)
  sapply(sheets, function(s) {
    df  <- read_excel(path, sheet = s, col_names = FALSE)
    mat <- as.matrix(df)
    loc <- which(mat == "50% specificity", arr.ind = TRUE)
    if (nrow(loc)) {
      as.numeric(df[[ loc[1, "row"], loc[1, "col"] + 1 ]])
    } else NA_real_
  }, USE.NAMES = TRUE)
}

# Read and consolidate
params1 <- load_params(raw_param1)
params2 <- load_params(raw_param2)
thresh1 <- load_thresholds(raw_roc1)
thresh2 <- load_thresholds(raw_roc2)

names(params1) <- tolower(names(params1))
names(params2) <- tolower(names(params2))
names(thresh1) <- tolower(names(thresh1))
names(thresh2) <- tolower(names(thresh2))

# Save as RDS for quick app load
saveRDS(list(params1 = params1, params2 = params2), file = "data/reflux_model_params.rds")
saveRDS(list(thresh1 = thresh1, thresh2 = thresh2), file = "data/reflux_model_thresholds.rds")
