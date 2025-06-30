library(shiny)
library(dplyr)
library(shinyjs)
library(glue)
library(stringr)

## ── Data ────────────────────────────────────────────────────────────────
dat_params <- readRDS("data/reflux_model_params.rds")
params1 <- dat_params$params1          # 1-year model coefficients
params2 <- dat_params$params2          # 2-year model coefficients

## ── User interface ───────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(
    "#info_sidebar { position: fixed; top: 0; right: -300px; width:300px;
       height:100%; background:#f9f9f9; border-left:1px solid #ccc;
       padding:15px; z-index:999; transition:right .3s ease; overflow-y:auto;}
     #info_sidebar.visible{right:0;}
     .btn-info-circle{position:fixed; top:15px; right:15px; z-index:1001;
       background:#007ACC; border:none; color:#fff; width:40px; height:40px;
       border-radius:50%; display:flex; align-items:center; justify-content:center;
       box-shadow:0 2px 6px rgba(0,0,0,.2); transition:.2s;}
     .btn-info-circle:hover{background:#005A9E; transform:scale(1.1);}
     #logo{position:fixed; top:15px; right:70px; z-index:1000; transition:.3s;}
     #logo.shifted{right:370px;}"))),

  tags$div(id="logo",
           tags$img(src="UIHC_SFCH_H_GoldBlack.svg",
                    height="50px", alt="UIowa logo")),

  actionButton("toggle_info", label = NULL, icon = icon("info-circle"),
               class = "btn-info-circle"),

  titlePanel("Reflux Resolution Prediction"),

  fluidRow(
    ## ── Sidebar ──────────────────────────────────────────────────────────
    column(width = 3,
           wellPanel(
             h4("Patient Information"),
             numericInput("age", "Age", value = NA, min = 0, max = 10),
             helpText("Years (0.01 – 10)", style =
                        "margin-top:-10px;margin-bottom:10px;font-size:80%;color:#555;"),
             selectInput("sex", "Sex",
                         choices = c("Select..."="", "Female"="female", "Male"="male")),
             selectInput("reflux_bilat", "Bilateral Reflux",
                         choices = c("Select..."="", "Yes"="y", "No"="n")),
             selectInput("presenting_sx", "Presenting Symptom", choices = c(
               "Select..."="", "UTI"="uti", "Febrile UTI"="febrile uti",
               "Antenatal Hydro"="antenatal hydro", "Screening"="screening")),
             hr(),

             h4("Measurements"),
             helpText("At least one of the three following measurements must be provided. However, please only enter either UDR or Volume at Onset (one must be left blank). Also note that either can be entered with Reflux Grade."),
             numericInput("udr", "UDR", value = NA, min = 0.036, max = 0.747),
             helpText("Validated range: 0.04 – 0.75", style =
                        "margin-top:-10px;margin-bottom:10px;font-size:80%;color:#555;"),
             numericInput("volume_pct", "Volume at Onset (%)",
                          value = NA, min = 8.1, max = 117.9),
             helpText("Validated range: 8% – 118%", style =
                        "margin-top:-10px;margin-bottom:10px;font-size:80%;color:#555;"),
             tags$div(textOutput("meas_warn"),
                      style="color:#d9534f;font-weight:600;margin-top:-5px;"),
             selectInput("reflux_gr", "Reflux Grade",
                         choices = c("Select..."="","1","2","3","4-5")),
             div(id="predict-wrapper", actionButton("predict", "Predict"))
           )),

    ## ── Main panel ──────────────────────────────────────────────────────
    column(width = 7,
           conditionalPanel(
             condition = "input.predict == 0",
             wellPanel(
                 h4("Welcome"),
                 p("This calculator is a predictive tool developed by the Pediatric Urology Department of Iowa Health Care."),
                 h4("How to use this tool"),
                 p("1. Fill in the patient information on the left."),
                 p("2. Enter EITHER UDR OR Volume at Onset (and Grade if you have it)."),
                 p("3. Click ", tags$strong("Predict"), " to see calculate the chance of resolution.")
             )
           ),
           conditionalPanel(
             "input.predict > 0",
             wellPanel(
               fluidRow(
                 column(
                   width = 6,
                   h4("Patient Information"),
                   div(
                     style = "white-space:pre-line; font-size:18px;",
                     textOutput("input_summary")
                   )
                 ),
                 column(
                   width = 6,
                   h4("Chance of resolution"),
                   div(
                     style = "white-space:pre-line; font-size:18px;",
                     textOutput("prediction_summary")
                   )
                 )
               )
             )
           )

    )
  ),

  ## ── Sliding info sidebar ──────────────────────────────────────────────
  tags$div(id="info_sidebar",
           h4("Information"),
           p("Several different models were fit for prediction based on the measurements available. The fitted models contain variables including presenting symptoms, laterality of reflux, grade of reflux, distal ureteral diameter ratio (UDR), and bladder volume at the onset of reflux as a percentage of predicted bladder capacity."),
           p("The model used for prediction is based on the available data for your patient, and the predicted chance of resolution is based on your patients individualized data."),
           p(
             tags$strong("Note:"),
             " When bilateral reflux is present, the highest grade of either side is used to calculate predicted resolution."
           ),
           hr(),
           actionButton("file_bug", "File a bug",
                        onclick="window.open('https://github.com/lharris421/reflux_prediction_app/issues','_blank')",
                        class="btn external-btn")
  )
)

## ── Server ──────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  disable("predict")
  runjs('$("#info_sidebar").removeClass("visible"); $("#logo").removeClass("shifted");')
  runjs('$("#predict-wrapper").attr("title","Fill patient information to enable");')

  lbl_sex   <- c(female="Female",  male="Male")
  lbl_bilat <- c(y="Yes", n="No")
  lbl_sx    <- c(`uti`="UTI", `febrile uti`="Febrile UTI",
                 `antenatal hydro`="Antenatal Hydro", screening="Screening")

  ## ── Helpers ───────────────────────────────────────────────────────────
  valid_model <- reactive({
    has_udr <- !is.na(input$udr)
    has_vol <- !is.na(input$volume_pct)
    has_gr  <- nzchar(input$reflux_gr)
    if (has_udr && has_gr && !has_vol) "udr+grade"
    else if (has_udr && !has_gr && !has_vol) "udr"
    else if (!has_udr && has_gr && has_vol) "grade+volume"
    else if (!has_udr && has_gr && !has_vol) "refluxgrade"
    else if (!has_udr && !has_gr && has_vol) "volume"
    else NULL
  })

  base_ok <- reactive({
    !is.na(input$age) &&
      nzchar(input$sex) &&
      nzchar(input$reflux_bilat) &&
      nzchar(input$presenting_sx)
  })

  observe({
    if (base_ok() && !is.null(valid_model()))
      enable("predict")
    else
      disable("predict")
  })

  output$meas_warn <- renderText({
    if (!is.na(input$udr) && !is.na(input$volume_pct))
      "Please enter EITHER UDR OR Volume—one must be blank."
  })

  ## ── Core prediction routine ───────────────────────────────────────────
  make_prediction <- function(hor, sheet_key) {

    plist     <- if (hor=="1yr") params1 else params2

    df       <- plist[[sheet_key]] |> mutate(across(c(Estimate, SE), as.numeric))

    lp      <- 0

    for (i in seq_len(nrow(df))) {
      code <- tolower(df$Parameter[i])
      lvl  <- tolower(df$Level[i])
      est  <- df$Estimate[i]
      se   <- df$SE[i]

      x_i <- switch(code,
                    intercept        = 1,
                    `ln(age)`        = log(input$age),
                    udr              = input$udr,
                    volume_onsetpct  = input$volume_pct,
                    sex_female       = as.numeric(input$sex           == lvl),
                    reflux_bilateral = as.numeric(input$reflux_bilat  == lvl),
                    presentingsx     = as.numeric(input$presenting_sx == lvl),
                    reflux_gr        = as.numeric(input$reflux_gr     == lvl),
                    0)

      lp     <- lp     + est * x_i

    }

    z     <- qnorm(0.975)
    prob  <- plogis(lp)

    data.frame(
      horizon      = hor,
      probability  = prob,
      model_sheet  = sheet_key,
      stringsAsFactors = FALSE
    )
  }


  observeEvent(input$predict, {
    key <- valid_model(); req(key)
    horizons <- c("1yr", "2yr")
    sheets   <- paste0(key, "-resolve", horizons)

    preds <- purrr::map2_dfr(horizons, sheets, make_prediction)
    preds$yrs <- c("1 year", "2 years")

    ## ── Patient info summary ───────────────────────────────────────────
    sex_lbl   <- lbl_sex[input$sex]
    bilat_lbl <- lbl_bilat[input$reflux_bilat]
    sx_lbl    <- lbl_sx[input$presenting_sx]

    input_summary <- c(
      glue("Age: {input$age}"),
      glue("Sex: {sex_lbl}"),
      glue("Reflux Bilateral: {bilat_lbl}"),
      glue("Presenting Symptom: {sx_lbl}"),
      if (!is.na(input$udr))        glue("UDR: {input$udr}"),
      if (!is.na(input$volume_pct)) glue("Volume: {input$volume_pct}%"),
      if (nzchar(input$reflux_gr))  glue("Grade: {input$reflux_gr}")
    ) |>
      glue_collapse(sep = "\n")

    output$input_summary <- renderText(input_summary)

    ## ── Predictions (chance only) ──────────────────────────────────────
    pred_txt <- preds |>
      # mutate(txt = glue("{yrs}: {round(probability*100,1)}% ",
      #                   "(95% CI {round(ci_lo*100,1)}–{round(ci_hi*100,1)}%)")) |>
      mutate(txt = glue("{yrs}: {round(probability*100,1)}% ")) |>
      pull(txt) |>
      glue_collapse(sep = "\n")
    output$prediction_summary <- renderText(pred_txt)

  })

  ## ── Toggle info sidebar ───────────────────────────────────────────────
  observeEvent(input$toggle_info,{
    runjs("$('#info_sidebar').toggleClass('visible');")
    runjs("$('#logo').toggleClass('shifted');")
  })
}

shinyApp(ui, server)
