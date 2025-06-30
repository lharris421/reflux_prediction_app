library(readxl)
library(dplyr)

## ── Raw Excel file paths ───────────────────────────────────────────────
raw_param1 <- "data/LogisticModelParameters-WithinOneYear.xlsx"
raw_param2 <- "data/LogisticModelParameters-WithinTwoYears.xlsx"
raw_roc1   <- "data/ROC-RefluxResolve-WithinOneYearOfVisit.xlsx"
raw_roc2   <- "data/ROC-RefluxResolve-WithinTwoYearOfVisit.xlsx"

## ── Helper: read all parameter sheets, incl. Std. Error (col D) ────────
load_params <- function(path) {
  sheets <- excel_sheets(path)
  setNames(
    lapply(sheets, function(s) {
      read_excel(path, sheet = s,
                 range = cell_cols("A:D"),   #  ⬅ range extended to D
                 col_names = FALSE, skip = 1) |>
        setNames(c("Parameter", "Level", "Estimate", "SE")) |>
        mutate(across(c(Estimate, SE), as.numeric)) |>
        filter(!is.na(Estimate), Parameter != "Parameter")
    }),
    sheets)
}

## ── Helper: ROC cut-point finder (unchanged) ───────────────────────────
load_roc_metrics <- function(path, desired_specificity = 0.80) {
  sheets <- excel_sheets(path)
  sapply(
    sheets,
    simplify = FALSE,
    function(s) {
      raw <- read_excel(path, sheet = s, col_names = FALSE)
      loc <- which(raw == "Threshold", arr.ind = TRUE)
      if (!nrow(loc))
        return(list(threshold = NA_real_, sensitivity = NA_real_))

      header_row <- loc[1, "row"]
      roc <- read_excel(path, sheet = s, skip = header_row - 1)

      needed <- c("Threshold", "1-Specificity", "Sensitivity")
      if (!all(needed %in% names(roc)))
        return(list(threshold = NA_real_, sensitivity = NA_real_))

      roc <- roc |>
        filter(!is.na(Threshold)) |>
        mutate(spec = 1 - as.numeric(`1-Specificity`),
               sens = as.numeric(Sensitivity))

      hit <- roc |>
        mutate(row_id = row_number()) |>
        filter(spec >= desired_specificity) |>
        slice_min(row_id, n = 1)

      if (nrow(hit) == 0)
        list(threshold = NA_real_, sensitivity = NA_real_)
      else
        list(threshold   = as.numeric(hit$Threshold),
             sensitivity = as.numeric(hit$sens))
    })
}

## ── Read and consolidate ───────────────────────────────────────────────
params1 <- load_params(raw_param1)
params2 <- load_params(raw_param2)
thresh1 <- load_roc_metrics(raw_roc1, desired_specificity = 0.80)
thresh2 <- load_roc_metrics(raw_roc2, desired_specificity = 0.80)

names(params1) <- tolower(names(params1))
names(params2) <- tolower(names(params2))
names(thresh1) <- tolower(names(thresh1))
names(thresh2) <- tolower(names(thresh2))

## ── Persist to disk ────────────────────────────────────────────────────
saveRDS(list(params1 = params1, params2 = params2),
        file = "data/reflux_model_params.rds")
saveRDS(list(thresh1 = thresh1, thresh2 = thresh2),
        file = "data/reflux_model_thresholds.rds")
