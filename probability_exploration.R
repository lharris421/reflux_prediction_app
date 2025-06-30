source("functions.R")
params <- readRDS("data/reflux_model_params.rds")
thresh <- readRDS("data/reflux_model_thresholds.rds")

grid <- expand.grid(
  age             = c(0.25, 1.5, 4),
  sex             = c("Female","male"),
  bilateral       = c("Y","n"),
  psx             = c("Screening","UTI","Febrile UTI","Antenatal Hydro"),
  grade           = c("1","2","3","4-5"),
  Volume_OnsetPct = seq(20,200,by=20),
  UDR             = seq(.1,.9,by=.1),
  stringsAsFactors = FALSE
)

names(params[[1]])

predictions <- predict_for_model(grid,
                                 model  = "refluxgrade-resolve1yr",
                                 params = params) %>%
  arrange(desc(pred))

