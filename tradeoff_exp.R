#–––  Predictive value helpers  ––––––––––––––––––––––––––––––––––––––––––––––#
ppv <- function(sensitivity, specificity, prevalence, na.rm = TRUE) {
  ## Input checks ------------------------------------------------------------
  if (any(prevalence   < 0 | prevalence   > 1, na.rm = na.rm))
    stop("`prevalence` must be in [0, 1].", call. = FALSE)
  if (any(sensitivity  < 0 | sensitivity  > 1, na.rm = na.rm))
    stop("`sensitivity` must be in [0, 1].", call. = FALSE)
  if (any(specificity  < 0 | specificity  > 1, na.rm = na.rm))
    stop("`specificity` must be in [0, 1].", call. = FALSE)

  ## Formula:  PPV = Se·π / {Se·π + (1−Sp)(1−π)}
  num <- sensitivity * prevalence
  den <- num + (1 - specificity) * (1 - prevalence)
  num / den
}

npv <- function(sensitivity, specificity, prevalence, na.rm = TRUE) {
  ## Re-use same validations as above (or wrap in another small helper)
  if (any(prevalence   < 0 | prevalence   > 1, na.rm = na.rm))
    stop("`prevalence` must be in [0, 1].", call. = FALSE)
  if (any(sensitivity  < 0 | sensitivity  > 1, na.rm = na.rm))
    stop("`sensitivity` must be in [0, 1].", call. = FALSE)
  if (any(specificity  < 0 | specificity  > 1, na.rm = na.rm))
    stop("`specificity` must be in [0, 1].", call. = FALSE)

  ## Formula:  NPV = Sp·(1−π) / {(1−Se)·π + Sp·(1−π)}
  num <- specificity * (1 - prevalence)
  den <- (1 - sensitivity) * prevalence + num
  num / den
}

# 50%
ppv(0.06522, 1 - 0.0221, 230 / 814)
npv(0.06522, 1 - 0.0221, 230 / 814)

# 33%
ppv(0.34, .9, 230 / 814)
npv(0.34, .9, 230 / 814)

# 28%
ppv(0.41739, 1 - .14324, 230 / 814)
npv(0.41739, 1 - .14324, 230 / 814)

# 24%
ppv(0.52, .8, 230 / 814)
npv(0.52, .8, 230 / 814)
