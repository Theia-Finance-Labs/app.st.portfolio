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
    useShinyjs(), # Initialize shinyjs
    # Custom Semantic UI Modal
    tags$div(
      id = ns("mymodal"),
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait while the model is being run with the chosen parameters. This may take up to 10 minutes.")
      )
    ),
    tags$div(
      id = ns("model_load_db"),
      class = "ui modal",
      tags$div(class = "header", "Fetching precomputed results from database..."),
      tags$div(
        class = "content",
        tags$p("This dialog should close automatically when the data is loaded. Click outside of it to close manually.")
      )
    ),
    tags$button(
      id = ns("run_trisk"),
      class = "ui fluid button ", # Added custom class for styling
      "Run Trisk (click again when switching tabs to refresh data)"
    )
  )
}

####### Server

server <- function(
    id,
    trisk_run_params_r,
    trisk_granularity_r,
    backend_trisk_run_folder,
    trisk_input_path,
    max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # TRISK COMPUTATION =========================
    npv_data_r <- reactiveVal(NULL)
    pd_data_r <- reactiveVal(NULL)
    company_trajectories_r <- reactiveVal(NULL)

    # fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = T, {
      trisk_outputs <- run_local_trisk()

      npv_data_r(trisk_outputs$npv_results)
      npv_data_r(trisk_outputs$pd_results)
      company_trajectories_r(trisk_outputs$company_trajectories)
    })

    return(
      list(
        "npv_data_r" = npv_data_r,
        "pd_data_r" = pd_data_r,
        "company_trajectories_r" = company_trajectories_r
      )
    )
  })
}
