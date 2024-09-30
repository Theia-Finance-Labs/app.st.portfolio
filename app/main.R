# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, HTML, uiOutput, conditionalPanel, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader, icon]
)

# Load required modules and logic files
box::use(
  # modules
  app/view/sidebar_parameters,
    # logic
  app/logic/constant[
    trisk_input_path,
    available_vars,
    hide_vars
  ]
)


# Define the UI function
#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny.semantic::semanticPage(
    shinyjs::useShinyjs(), # Initialize shinyjs
    # CONTENT PAGE
    tags$div(
      class = "header", # Add a loading overlay
    ),
    dashboardPage(
      title = "Crispy",
      # dashboardHeader
      dashboardHeader(title = "Portfolio ST"),
      # dashboardSidebar
      dashboardSidebar(
        tags$div(
          sidebar_parameters$ui(
            ns("sidebar_parameters")
          ),
          shiny::img(
            src = "static/logo_life_stress.jpg",
            height = "30%", width = "auto",
            style = "
              display: block;
              margin-left: auto;
              margin-right: auto;
              margin-top: 10px;
              margin-bottom: 10px;"
          )
        ),
        size = "very wide",
        visible = TRUE
      ),

      # dashboardBody
      dashboardBody(
        div(
          class = "ui container",
          NULL
        )
      )
    )
  )
}


# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  possible_trisk_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(trisk_input_path)    # the TRISK runs are generated In the sidebar module
  
    sidebar_parameters_out <- sidebar_parameters$server(
      "sidebar_parameters",
      max_trisk_granularity = max_trisk_granularity, # constant
      possible_trisk_combinations = possible_trisk_combinations, # computed constant
      backend_trisk_run_folder = backend_trisk_run_folder, # constant
      trisk_input_path = trisk_input_path, # constant
      available_vars = available_vars, # constant
      hide_vars = hide_vars # constant
    )

    perimeter <- sidebar_parameters_out$perimeter

  })
}
