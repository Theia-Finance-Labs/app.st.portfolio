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
    tags$div(
      id = ns("mymodal"),
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait while the model is being run with the chosen parameters. This may take up to 10 minutes.")
      )
    ),
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




  # Wrap the process in a tryCatch block to handle errors
      tryCatch({
        #open modal dialog
        shinyjs::runjs(
              paste0(
                "$('#", session$ns("mymodal"), "').modal({closable: true}).modal('show');"
              )
            )
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
      }, error = function(e) {
        # Handle the error gracefully (log, show message, etc.)
        shiny::showNotification("Trisk run failed. No data added.", type = "error")
        # message("Error in run_trisk_sa: ", e$message)
      })

      # close the modal dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal('hide');"
        )
      )

    })

    return(
      trisk_results_r
    )
  })
}


