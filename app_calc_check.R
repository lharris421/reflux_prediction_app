library(dplyr)

yr1_grade <- function(age, sex, bilateral, psx, grade) {
  lp <- -3.4072 - 0.3161*log(age) - 0.6471*sex - 0.1286*bilateral

  lp <- lp +
    ifelse(psx == 1, 0.4594,
           ifelse(psx == 2, 0.0024,
                  ifelse(psx == 3, 0.046, 0)))

  lp <- lp + dplyr::case_when(
    grade == 1 ~ 3.343,
    grade == 2 ~ 2.6749,
    grade == 3 ~ 1.6581,
    TRUE       ~ 0
  )

  plogis(lp)
}

yr1_volume <- function(age, sex, bilateral, psx, volume) {
  lp <- -1.9789 - 0.1891*log(age) - 0.4423*sex - 0.326*bilateral + 0.0167*volume

  lp <- lp +
    ifelse(psx == 1, 0.4789,
           ifelse(psx == 2, -0.3602,
                  ifelse(psx == 3, -0.5821, 0)))

  plogis(lp)
}
yr1_volume(3, 1, 0, 2, 10)

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

yr2_grade <- function(age, sex, bilateral, psx, grade) {
  # female = 1
  # psx: uti = 1, febrile uti = 2, antenatal hydro = 3
  lp <- -2.2395 + -0.2935*log(age) + -0.5202*sex + -0.2693*bilateral

  if (psx == 1) {
    lp <- lp + 0.232
  } else if (psx == 2) {
    lp <- lp - 0.4477
  } else if (psx == 3) {
    lp <- lp - 0.1572
  }

  if (grade == 1) {
    lp <- lp + 3.0606
  } else if (grade == 2) {
    lp <- lp + 2.4413
  } else if (grade == 3) {
    lp <- lp + 1.5203
  }

  exp(lp) / (1 + exp(lp))

}
yr2_grade(.5, 0, 1, 1, 2)


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

