library(shiny)
library(dplyr)
library(shinyjs)
library(glue)
library(stringr)

# Load preprocessed model data
dat_params <- readRDS("data/reflux_model_params.rds")
dat_thresh <- readRDS("data/reflux_model_thresholds.rds")
params1 <- dat_params$params1
params2 <- dat_params$params2
thresh1 <- dat_thresh$thresh1
thresh2 <- dat_thresh$thresh2

ui <- fluidPage(
  useShinyjs(),
  # Custom CSS for sliding sidebar and logo positioning
  tags$head(tags$style(HTML(
    "#info_sidebar { position: fixed; top: 0; right: -300px; width: 300px; height: 100%; background: #f9f9f9; border-left: 1px solid #ccc; padding: 15px; z-index: 999; transition: right 0.3s ease; overflow-y: auto; }",
    "#info_sidebar.visible { right: 0; }",
    ".btn-info-circle { position: fixed; top: 15px; right: 15px; z-index:1001; background-color: #007ACC; border: none; color: white; width: 40px; height: 40px; border-radius: 50%; display: flex; align-items: center; justify-content: center; box-shadow: 0 2px 6px rgba(0,0,0,0.2); transition: background-color 0.2s ease, transform 0.2s ease; }",
    ".btn-info-circle:hover { background-color: #005A9E; transform: scale(1.1); }",
    "#logo { position: fixed; top: 15px; right: 70px; z-index:1000; transition: right 0.3s ease; }",
    "#logo.shifted { right: 370px; }"
  ))),

  # University logo
  tags$div(id = "logo",
           tags$img(src = "https://brand.uiowa.edu/sites/brand.uiowa.edu/files/styles/widescreen__1312_x_738/public/2020-05/Block%20IOWA-black%20on%20gold%402x.png?h=42ab2369&itok=EeG8mSs7",
                    height = "50px", alt = "UIowa logo")
  ),

  # Modern Info toggle button as circle with icon
  actionButton("toggle_info", label = NULL, icon = icon("info-circle"), class = "btn-info-circle"),

  titlePanel("Reflux Resolution Prediction"),

  fluidRow(
    column(width = 3,
           wellPanel(
             # Time Horizon section
             h4("Time Horizon"),
             selectInput("horizon", "", choices = c("1 Year" = "1yr", "2 Years" = "2yr")),
             hr(),

             # Patient Information
             h4("Patient Information"),
             numericInput("age", "Age", value = NA, min = 0, max = 11),
             helpText("Enter age in years (0–11)", style = "margin-top: -10px; margin-bottom: 10px; font-size: 80%; color: #555;"),
             selectInput("sex", "Sex", choices = c("Select..." = "", "Female" = "female", "Male" = "male")),
             selectInput("reflux_bilat", "Bilateral Reflux", choices = c("Select..." = "", "Yes" = "y", "No" = "n")),
             selectInput("presenting_sx", "Presenting Symptom", choices = c(
               "Select..." = "", "UTI" = "uti", "Febrile UTI" = "febrile uti",
               "Antenatal Hydro" = "antenatal hydro", "Screening" = "screening"
             )),
             hr(),

             # Measurements
             h4("Measurements"),
             numericInput("udr", "UDR", value = NA, min = 0),
             numericInput("volume_pct", "Volume at Onset (%)", value = NA, min = 7.3, max = 202.3),
             helpText("Validated range: 7.3–202.3", style = "margin-top: -10px; margin-bottom: 10px; font-size: 80%; color: #555;"),
             selectInput("reflux_gr", "Reflux Grade", choices = c("Select..." = "", "1" = "1", "2" = "2", "3" = "3", "4-5" = "4-5")),
             div(id = "predict-wrapper", actionButton("predict", "Predict"))
           )
    ),
    column(width = 6,
           conditionalPanel(
             condition = "input.predict > 0",
             h4("Summary:"),
             div(
               style = "white-space: pre-line; margin: 0; padding: 0; font-size: 18px;",
               textOutput("summary")
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      h4("Patient Information:"),
                      div(
                        style = "white-space: pre-line; margin: 0; padding: 0; font-size: 18px;",
                        textOutput("input_summary")
                      )
               ),
               column(width = 6,
                      h4("Prediction:"),
                      div(
                        style = "white-space: pre-line; margin: 0; padding: 0; font-size: 18px;",
                        textOutput("prediction_summary")
                      )
               )
             )
           )
    )

  ),

  # Sliding Info Sidebar
  tags$div(id = "info_sidebar",
           h4("Information"),
           p("This sidebar can contain user guidance, documentation, or instructions."),
           NULL
  )
)

server <- function(input, output, session) {
  # Initialize
  disable("predict")
  runjs('$("#info_sidebar").removeClass("visible");')
  runjs('$("#logo").removeClass("shifted");')
  runjs('$("#predict-wrapper").attr("title","Fill patient information and valid measurements to enable");')

  # Toggle sidebar visibility and logo shift
  observeEvent(input$toggle_info, {
    runjs("$('#info_sidebar').toggleClass('visible');")
    runjs("$('#logo').toggleClass('shifted');")
  })

  # Valid model logic
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

  base_check <- reactive({
    if (is.na(input$age) || !nzchar(input$sex) || !nzchar(input$reflux_bilat) || !nzchar(input$presenting_sx)) {
      FALSE
    } else {
      TRUE
    }
  })

  model_sheet <- reactive({
    key <- valid_model(); if (is.null(key)) return(NULL)
    glue("{key}-resolve{input$horizon}")
  })

  # Enable Predict and set tooltip
  observe({
    base_ok  <- base_check()
    combo_ok <- !is.null(model_sheet())
    if (base_ok && combo_ok) {
      enable("predict")
      runjs('$("#predict-wrapper").attr("title","Click to predict");')
    } else {
      disable("predict")
      msg <- if (!base_ok) "Please provide patient information." else "Enter a valid measurement combination: Reflux grade, Volume, Grade+Volume, UDR, or UDR+Grade."
      runjs(sprintf('$("#predict-wrapper").attr("title","%s");', gsub('"','\\"', msg)))
    }
  })

  # Prediction logic
  observeEvent(input$predict, {
    sheet <- model_sheet(); req(sheet)
    plist <- if (input$horizon == "1yr") params1 else params2
    tlist <- if (input$horizon == "1yr") thresh1 else thresh2
    df <- plist[[sheet]] %>% mutate(Estimate = as.numeric(Estimate))
    lp <- sum(sapply(seq_len(nrow(df)), function(i) {
      code <- tolower(df$Parameter[i])
      lvl <- tolower(df$Level[i])
      est <- df$Estimate[i]
      if (code == "intercept") return(est)
      if (code == "ln(age)") return(est * log(input$age))
      if (code == "udr") return(est * input$udr)
      if (code == "volume_onsetpct") return(est * input$volume_pct)
      val <- switch(code,
                    sex_female       = input$sex,
                    reflux_bilateral = input$reflux_bilat,
                    presentingsx     = input$presenting_sx,
                    reflux_gr        = input$reflux_gr,
                    NA_character_)
      if (!is.na(val) && val == lvl) return(est)
      0
    }))
    prob   <- exp(lp) / (1 + exp(lp))
    ## cutoff <- tlist[[sheet]]
    cutoff <- 0.5
    # output$model_used  <- renderText(paste("Model:", sheet))
    output$result_prob <- renderText(paste("Predicted probability of resolution:", round(prob, 3)))

    odds <- prob / (1 - prob)
    if (odds < 1) {
      num <- 1
      denom <- round(1 / odds)
    } else {
      num <- round(odds)
      denom <- 1
    }

    if (prob >= cutoff) {
      res <- "resolve"
    } else {
      res <- "not resolve"
    }

    prediction_summary <- glue(
      "Prediction: {res} \n",
      "Probability of resolution: {round(prob, 3)} \n",
      "Odds of resolution: {num} to {denom}"
    ) %>%
      str_replace_all(" {2,}", " ")

    input_summary <- glue(
      "Age: {input$age}\n",
      "Sex: {input$sex}\n",
      "Reflux Bilateral: {input$reflux_bilat}\n",
      "Presenting Symptom: {input$presenting_sx}\n",
      "{ifelse(is.na(input$udr), '', glue('UDR: {input$udr} \n'))}",
      "{ifelse(is.na(input$volume_pct), '', glue('Volume: {input$volume_pct} \n'))}",
      "{ifelse(!nzchar(input$reflux_gr), '', glue('Grade: {input$reflux_gr} \n'))}"
    ) %>%
      str_replace_all(" {2,}", " ")

    summary <- glue(
      "For a {input$age} year old {input$sex} with ",
      "{ifelse(input$reflux_bilat == 'y', 'bilateral reflux', 'unilateral reflux')} ",
      "presenting with {input$presenting_sx} and a ",
      "{ifelse(is.na(input$udr), '', glue('UDR of {input$udr}, '))} ",
      "{ifelse(is.na(input$volume_pct), '', glue('volume at onset of {input$volume_pct}%, '))}",
      "{ifelse(!nzchar(input$reflux_gr), '', glue('reflux grade of {input$reflux_gr}, '))}",
      " the patient is predicted to {res} within {input$horizon}. ",
      "The probability of resolution is {round(prob, 3)}, ",
      "which gives an odds of resolution of {num} to {denom}."
    ) %>%
      str_replace_all(" {2,}", " ")

    output$summary <- renderText(summary)
    output$input_summary <- renderText(input_summary)
    output$prediction_summary <- renderText(prediction_summary)
  })
}

shinyApp(ui, server)
