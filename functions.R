library(dplyr)

# 1) Extract the tiny lookup for one model
get_coefs <- function(model, params) {
  df <- if (grepl("1yr$", model)) params$params1[[model]] else params$params2[[model]]

  intercept <- df %>%
    filter(Parameter == "Intercept") %>%
    pull(Estimate) %>%
    as.numeric()

  lnage <- df %>%
    filter(grepl("^ln", Parameter, ignore.case = TRUE), is.na(Level)) %>%
    pull(Estimate) %>%
    as.numeric() %>%
    `[`(1)

  make_map <- function(p) {
    sub <- df %>% filter(Parameter == p)
    if (nrow(sub)==0) return(NULL)
    setNames(as.numeric(sub$Estimate), sub$Level)
  }

  volume_coef <- df %>%
    filter(Parameter == "Volume_OnsetPct") %>%
    pull(Estimate) %>% as.numeric()
  if (length(volume_coef)==0) volume_coef <- 0

  udr_coef <- df %>%
    filter(Parameter == "UDR") %>%
    pull(Estimate) %>% as.numeric()
  if (length(udr_coef)==0) udr_coef <- 0

  list(
    intercept     = intercept,
    lnage         = lnage,
    sex_map       = make_map("Sex_female"),
    bilateral_map = make_map("Reflux_Bilateral"),
    psx_map       = make_map("PresentingSx"),
    gr_map        = make_map("reflux_gr"),
    volume_coef   = volume_coef,
    udr_coef      = udr_coef
  )
}

# 2) One‐call wrapper to predict over the whole grid
predict_for_model <- function(full_grid, model, params) {
  coefs <- get_coefs(model, params)

  # Which columns does this model actually need?
  needed <- "age"
  use_sex       <- !is.null(coefs$sex_map)
  use_bilateral <- !is.null(coefs$bilateral_map)
  use_psx       <- !is.null(coefs$psx_map)
  use_gr        <- !is.null(coefs$gr_map)
  use_vol       <- coefs$volume_coef != 0
  use_udr       <- coefs$udr_coef    != 0

  if (use_sex)       needed <- c(needed, "sex")
  if (use_bilateral) needed <- c(needed, "bilateral")
  if (use_psx)       needed <- c(needed, "psx")
  if (use_gr)        needed <- c(needed, "grade")
  if (use_vol)       needed <- c(needed, "Volume_OnsetPct")
  if (use_udr)       needed <- c(needed, "UDR")
  needed <- unique(needed)

  full_grid %>%
    select(any_of(needed)) %>%
    distinct() %>%
    mutate(
      pred = plogis(
        coefs$intercept +
          coefs$lnage * log(age) +
          (if (use_sex)       coefs$sex_map[sex]               else 0) +
          (if (use_bilateral) coefs$bilateral_map[bilateral]   else 0) +
          (if (use_psx)       coefs$psx_map[psx]               else 0) +
          (if (use_gr)        coefs$gr_map[grade]              else 0) +
          (if (use_vol)       coefs$volume_coef * Volume_OnsetPct else 0) +
          (if (use_udr)       coefs$udr_coef    * UDR             else 0)
      )
    )
}
