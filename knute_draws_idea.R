# ---------------------------------------------------------------------------
# 0.  Coefficient vector and SEs  (taken from the table you supplied)
# ---------------------------------------------------------------------------
coef_hat <- c(
  `(Intercept)`      = -1.6681,
  lnAge              = -0.4290,
  sex_female         = -0.6555,
  reflux_bilat_Y     = -0.3254,
  sx_UTI             =  0.9474,
  sx_FebrileUTI      =  0.0259,
  sx_AntenatalHydro  = -0.1353,
  reflux_gr1         =  2.6008,
  reflux_gr2         =  1.6135,
  reflux_gr3         =  1.5021,
  UDR_present        = -2.9271
)

se_hat <- c(
  `(Intercept)`      = 0.7475,
  lnAge              = 0.0972,
  sex_female         = 0.3684,
  reflux_bilat_Y     = 0.2987,
  sx_UTI             = 0.5086,
  sx_FebrileUTI      = 0.5167,
  sx_AntenatalHydro  = 0.5196,
  reflux_gr1         = 0.7082,
  reflux_gr2         = 0.6223,
  reflux_gr3         = 0.6181,
  UDR_present        = 1.0982
)

# ---------------------------------------------------------------------------
# 1.  Define ONE patient profile you want to predict for ---------------------
#     (1 = present, 0 = absent; intercept handled automatically)
# ---------------------------------------------------------------------------
x <- c(
  `(Intercept)`      = 1,
  lnAge              = log(12),      # say age = 12 months
  sex_female         = 1,            # female
  reflux_bilat_Y     = 0,            # no bilateral reflux
  sx_UTI             = 1,            # presenting symptom = UTI
  sx_FebrileUTI      = 0,
  sx_AntenatalHydro  = 0,
  reflux_gr1         = 0,
  reflux_gr2         = 1,            # VUR grade = 2
  reflux_gr3         = 0,
  UDR_present        = 0             # no UDR
)

# Make sure coefficient and covariate vectors align
stopifnot(all(names(coef_hat) == names(x)))

# ---------------------------------------------------------------------------
# 2.  (i) Wald CI on linear predictor, then back-transform -------------------
# ---------------------------------------------------------------------------
eta_hat   <- sum(x * coef_hat)
se_eta    <- sqrt(sum( (x * se_hat)^2 ))   # independence assumption
z97_5     <- qnorm(0.975)

eta_limits <- eta_hat + c(-1, 1) * z97_5 * se_eta
wald_CI    <- plogis(eta_limits)           # to probability scale

# ---------------------------------------------------------------------------
# 3.  (ii) Parametric simulation (1 000 draws) -------------------------------
# ---------------------------------------------------------------------------
set.seed(123)
B          <- 100000
beta_draws <- matrix(
  rnorm(length(coef_hat) * B,
        mean = rep(coef_hat, each = B),
        sd   = rep(se_hat,   each = B)),
  nrow = B, byrow = FALSE, dimnames = list(NULL, names(coef_hat))
)
p_draws <- plogis(beta_draws %*% x)        # B × 1 vector
sim_CI  <- quantile(p_draws, c(0.025, 0.975))

# ---------------------------------------------------------------------------
# 4.  Display results --------------------------------------------------------
# ---------------------------------------------------------------------------
round(rbind(
  point_estimate = plogis(eta_hat),
  Wald_CI        = wald_CI,
  Sim_CI         = sim_CI
), 4)
