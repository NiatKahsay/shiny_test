# app.R
library(shiny)
library(tidyverse)
library(fpp3)
library(gt)
library(plotly)

# Load data
wines_raw <- readr::read_csv("AustralianWines.csv", show_col_types = FALSE)

wine_long <- wines_raw |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "value") |>
  mutate(
    Month = yearmonth(Month),
    Date  = as_date(Month)
  ) |>
  as_tsibble(index = Month, key = Varietal)

# Pre-compute models for performance
if (file.exists("fit_all.rds")) {
  fit <- readRDS("fit_all.rds")
} else {
  train <- wine_long |> filter(Month <= yearmonth("1993 Dec"))
  fit <- train |>
    model(
      tslm = TSLM(value ~ trend() + season()),
      ets = ETS(value),
      arima = ARIMA(value)
    )
  saveRDS(fit, "fit_all.rds")
}

# UI
ui <- fluidPage(
  titlePanel("Australian Wines Forecasting Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("varietals_select", "Select Wine Varietals:",
                     choices = unique(wine_long$Varietal),
                     selected = unique(wine_long$Varietal),
                     multiple = TRUE),
      
      dateInput("train_cutoff_date", "Training Cutoff Date:",
                value = as_date("1993-12-01"),
                min = min(wine_long$Date),
                max = max(wine_long$Date)),
      
      numericInput("horizon_input", "Forecast Horizon (months):",
                   value = 12, min = 1, max = 36),
      
      actionButton("update_btn", "Update Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview",
                 plotlyOutput("overview_plot_output")),
        
        tabPanel("Model Building",
                 radioButtons("view_radio", "View:",
                              choices = c("Model Specifications" = "specs",
                                          "Training Accuracy" = "train_acc",
                                          "Test Accuracy" = "test_acc",
                                          "Forecast Plots" = "forecast_plots"),
                              selected = "specs"),
                 uiOutput("dynamic_ui_output")),
        
        tabPanel("Forecast Evaluation",
                 gt_output("best_models_gt"),
                 plotlyOutput("eval_plot_output")),
        
        tabPanel("Individual Analysis",
                 selectInput("varietal_select", "Select Varietal:",
                             choices = unique(wine_long$Varietal),
                             selected = "Fortified"),
                 gt_output("indiv_accuracy_gt"),
                 plotlyOutput("indiv_plot_output"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data based on selections
  analysis_data <- eventReactive(input$update_btn, {
    list(
      train = wine_long |> 
        filter(Varietal %in% input$varietals_select,
               Month <= yearmonth(input$train_cutoff_date)),
      test = wine_long |> 
        filter(Varietal %in% input$varietals_select,
               Month > yearmonth(input$train_cutoff_date))
    )
  })
  
  # Reactive models
  current_models <- reactive({
    train_data <- analysis_data()$train
    train_data |>
      model(
        tslm = TSLM(value ~ trend() + season()),
        ets = ETS(value),
        arima = ARIMA(value)
      )
  })
  
  # Reactive forecasts
  current_forecasts <- reactive({
    current_models() |> forecast(h = input$horizon_input)
  })
  
  # Data Overview Plot
  output$overview_plot_output <- renderPlotly({
    data <- analysis_data()$train
    
    p <- data |>
      autoplot(value) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(title = "Selected Wine Varietals - Sales Over Time") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Dynamic UI for Model Building
  output$dynamic_ui_output <- renderUI({
    req(current_models())
    
    switch(input$view_radio,
           "specs" = gt_output("specs_gt"),
           "train_acc" = gt_output("train_acc_gt"),
           "test_acc" = gt_output("test_acc_gt"),
           "forecast_plots" = plotlyOutput("model_plots_output")
    )
  })
  
  # Model Specifications
  output$specs_gt <- render_gt({
    current_models() |>
      glance() |>
      select(Varietal, .model, model_desc = ifelse(.model == "tslm", "Linear", 
                                            ifelse(.model == "ets", "ETS", "ARIMA"))) |>
      gt() |>
      tab_header(title = "Model Specifications")
  })
  
  # Training Accuracy
  output$train_acc_gt <- render_gt({
    current_models() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, MAPE) |>
      gt() |>
      fmt_number(decimals = 1) |>
      tab_header(title = "Training Accuracy Metrics")
  })
  
  # Test Accuracy
  output$test_acc_gt <- render_gt({
    fc <- current_forecasts()
    test_data <- analysis_data()$test
    
    fc |>
      accuracy(test_data) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, MAPE) |>
      gt() |>
      fmt_number(decimals = 1) |>
      tab_header(title = "Test Accuracy Metrics")
  })
  
  # Forecast Plots in Model Building
  output$model_plots_output <- renderPlotly({
    train_data <- analysis_data()$train
    fc <- current_forecasts()
    
    p <- fc |>
      autoplot(train_data, level = 80) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(title = "Model Forecasts Comparison") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Best Models Table
  output$best_models_gt <- render_gt({
    fc <- current_forecasts()
    test_data <- analysis_data()$test
    
    best_models <- fc |>
      accuracy(test_data) |>
      group_by(Varietal) |>
      slice_min(MAPE, n = 1) |>
      select(Varietal, .model, RMSE, MAE, MAPE)
    
    best_models |>
      gt() |>
      fmt_number(decimals = 1) |>
      tab_header(title = "Best Models by Varietal (Lowest MAPE)")
  })
  
  # Evaluation Forecast Plot
  output$eval_plot_output <- renderPlotly({
    train_data <- analysis_data()$train
    fc <- current_forecasts()
    
    p <- fc |>
      autoplot(train_data, level = 80) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(title = "Final Forecasts") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Individual Analysis Accuracy
  output$indiv_accuracy_gt <- render_gt({
    varietal <- input$varietal_select
    train_data <- analysis_data()$train |> filter(Varietal == varietal)
    test_data <- analysis_data()$test |> filter(Varietal == varietal)
    
    if(nrow(train_data) > 0) {
      individual_fit <- train_data |>
        model(
          tslm = TSLM(value ~ trend() + season()),
          ets = ETS(value),
          arima = ARIMA(value)
        )
      
      individual_fc <- individual_fit |> forecast(h = input$horizon_input)
      
      individual_fc |>
        accuracy(test_data) |>
        select(.model, RMSE, MAE, MAPE) |>
        arrange(MAPE) |>
        gt() |>
        fmt_number(decimals = 1) |>
        tab_header(title = paste(varietal, "- Test Accuracy"))
    }
  })
  
  # Individual Forecast Plot
  output$indiv_plot_output <- renderPlotly({
    varietal <- input$varietal_select
    train_data <- analysis_data()$train |> filter(Varietal == varietal)
    
    if(nrow(train_data) > 0) {
      individual_fit <- train_data |>
        model(
          tslm = TSLM(value ~ trend() + season()),
          ets = ETS(value),
          arima = ARIMA(value)
        )
      
      individual_fc <- individual_fit |> forecast(h = input$horizon_input)
      
      p <- individual_fc |>
        autoplot(train_data) +
        labs(title = paste(varietal, "- Individual Forecast"))
      
      ggplotly(p)
    }
  })
}

# Run the app
shinyApp(ui, server)