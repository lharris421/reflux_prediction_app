yr1_udr_grade <- function(age, sex, bilateral, psx, grade, udr) {
  # female = 1
  # psx: uti = 1, febrile uti = 2, antenatal hydor = 3
  lp <- -2.3895 + -0.3211*log(age) + -0.6591*sex + -0.1072*bilateral + -2.6451*udr

  if (psx == 1) {
    lp <- lp + 0.5752
  } else if (psx == 2) {
    lp <- lp + -0.0176
  } else if (psx == 3) {
    lp <- lp + 0.2191
  }

  if (grade == 1) {
    lp <- lp + 2.5531
  } else if (grade == 2) {
    lp <- lp + 1.5172
  } else if (grade == 3) {
    lp <- lp + 1.1412
  }

  exp(lp) / (1 + exp(lp))

}

yr1_grade <- function(age, sex, bilateral, psx, grade) {
  # female = 1
  # psx: uti = 1, febrile uti = 2, antenatal hydor = 3
  lp <- -3.4072 + -0.3161*log(age) + -0.6471*sex + -0.1286*bilateral

  if (psx == 1) {
    lp <- lp + 0.4594
  } else if (psx == 2) {
    lp <- lp + 0.0024
  } else if (psx == 3) {
    lp <- lp + 0.046
  }

  if (grade == 1) {
    lp <- lp + 3.343
  } else if (grade == 2) {
    lp <- lp + 2.6749
  } else if (grade == 3) {
    lp <- lp + 1.6581
  }

  exp(lp) / (1 + exp(lp))

}


yr2_grade_volume <- function(age, sex, bilateral, psx, grade, volume) {
  # female = 1
  # psx: uti = 1, febrile uti = 2, antenatal hydro = 3
  lp <- -3.7776 + -0.3467*log(age) + -0.5834*sex + -0.228*bilateral + 0.0178*volume

  if (psx == 1) {
    lp <- lp + 0.7344
  } else if (psx == 2) {
    lp <- lp + -0.2627
  } else if (psx == 3) {
    lp <- lp + 0.059
  }

  if (grade == 1) {
    lp <- lp + 3.1643
  } else if (grade == 2) {
    lp <- lp + 2.4622
  } else if (grade == 3) {
    lp <- lp + 1.6565
  }

  exp(lp) / (1 + exp(lp))

}
