# Load required packages
box::use(
  semantic.dashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
  shiny[div, h1, moduleServer, NS, observe, observeEvent, reactive, reactiveVal, tags],
  shiny.semantic[semanticPage],
  shinyjs[useShinyjs],
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$button(
      id = ns("run_trisk"),
      class = "ui fluid button ", # Added custom class for styling
      "Run Trisk"
    )
  )
}

####### Server

server <- function(
    id,
    assets_data,
    scenarios_data,
    financial_data,
    carbon_data,
    portfolio_data_r,
    trisk_run_params_r) {
  moduleServer(id, function(input, output, session) {
    # TRISK COMPUTATION =========================
    trisk_results_r <- reactiveVal(NULL)

    # fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = T, {
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())

      analysis_data <- do.call(
        trisk.analysis::run_trisk_on_portfolio,
        c(
          trisk_run_params,
          list(
            assets_data = assets_data,
            scenarios_data = scenarios_data,
            financial_data = financial_data,
            carbon_data = carbon_data,
            portfolio_data = portfolio_data_r()
          )
        )
      )

      trisk_results_r(analysis_data)
    })

    return(
      trisk_results_r
    )
  })
}
