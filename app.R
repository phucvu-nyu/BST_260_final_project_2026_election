# Loading necessary library
library(ggplot2)
library(shiny)
library(tidyverse)

source("./helper_functions/helper_functions.R")

# Define the UI
ui <- fluidPage(
  titlePanel("Bayesian Forest Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "state_input",
        "Select State:",
        choices = unique(senator_running_2024$State),
        selected = unique(senator_running_2024$State)[1]
      ),
      checkboxGroupInput(
        "population_input",
        "Select Population(s) of surveyed voter:",
        choices = unique(clean_dat$population),
        selected = unique(clean_dat$population)
      ),
      selectInput(
        "grade_input",
        "Select Lowest Grade of polls:",
        choices = unique(c("A", "B", "C", "D")),
        selected = "D"
      ),
      dateInput(
        "prior_from",
        "Select how far you want to use data to consider for prior:",
        value = as.Date("2016-01-01"),
        format = "yyyy-mm-dd"
      ),
      sliderInput(
        "tau_input",
        "Select your prior belief about standard deviation of poll:",
        min = 0.01, max = 0.1, value = 0.035, step = 0.001
      ),
      sliderInput(
        "factor_input",
        "Select Factor Parameter:",
        min = 0, max = 10, value = 5, step = 0.1
      ),
      dateInput(
        "polls_from",
        "Select Polls From Date:",
        value = as.Date("2018-01-01"),
        format = "yyyy-mm-dd"
      ),
      actionButton("run", "Generate Forest Plot"),
      actionButton("run_chair_plot", "Generate Chair Plot"), # New button for chair plot
      br(),
      downloadButton("download_data", "Download Plot Data") # Add download button
    ),
    
    mainPanel(
      h4("Forest Plot:"),
      plotOutput("forest_plot", width = "100%", height = "1000px"), # Display the forest plot
      textOutput("no_data_message"), # Add a text output for messages
      h4("Chair Prediction Plot:"), # New header for chair plot
      plotOutput("chair_plot", width = "100%", height = "500px") # Display the chair plot
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Reactive expression to collect inputs when "Generate Plot" is clicked
  user_inputs <- eventReactive(input$run, {
    list(
      state_input = input$state_input,
      population_input = input$population_input,
      grade_input = input$grade_input,
      prior_from = input$prior_from,
      tau_input = input$tau_input,
      factor_input = input$factor_input,
      polls_from = input$polls_from
    )
  })
  
  # Generate the plot data when inputs are updated
  plot_data <- reactive({
    req(user_inputs()) # Ensure user_inputs() is not NULL
    bayesian_estimate(
      data = clean_dat,
      state_input = user_inputs()$state_input,
      population_input = user_inputs()$population_input,
      grade_input = user_inputs()$grade_input,
      prior_from = user_inputs()$prior_from,
      cycle_input = 2024,
      tau_input = user_inputs()$tau_input,
      factor = user_inputs()$factor_input, # Pass the new factor parameter
      polls_from = user_inputs()$polls_from
    )
  })
  
  # Render the forest plot
  output$forest_plot <- renderPlot({
    req(plot_data()) # Ensure plot_data() is not NULL
    plot_forest(plot_data())
  })
  
  # Display message if no data is available
  output$no_data_message <- renderText({
    req(plot_data()) # Ensure plot_data() is not NULL
    if (nrow(plot_data()) == 0) {
      return("No polling data for this state available")
    } else {
      return("") # Return an empty string if data is available
    }
  })
  
  # Reactive expression for chair prediction inputs
  chair_results <- eventReactive(input$run_chair_plot, {
    chair_prediction(
      prior_from = input$prior_from,
      population_input = input$population_input,
      grade_input = input$grade_input,
      polls_from = input$polls_from,
      tau_input = input$tau_input,
      factor = input$factor_input
    )
  })
  
  # Render the chair plot
  output$chair_plot <- renderPlot({
    req(chair_results()) # Ensure chair_results() is not NULL
    chair_plot(chair_results())
  })
  
  # Download handler for the plot data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("plot_data_", Sys.Date(), ".csv") # Generate file name with date
    },
    content = function(file) {
      write.csv(plot_data(), file, row.names = FALSE) # Write reactive data to CSV
    }
  )
}

# Run the app
shinyApp(ui, server)
