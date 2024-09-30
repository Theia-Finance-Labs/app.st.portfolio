box::use(
  semantic.dashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
  shiny[
    div,
    img,
    moduleServer,
    NS,
    renderUI,
    tags,
    uiOutput
  ],
  shiny.semantic[semanticPage],
  shinyjs[useShinyjs],
)

box::use(
  app/logic/constant[
    AVAILABLE_VARS,
    HIDE_VARS,
    TRISK_INPUT_PATH,
    TRISK_POSTGRES_DB,
    TRISK_POSTGRES_HOST,
    TRISK_POSTGRES_PASSWORD,
    TRISK_POSTGRES_PORT,
    TRISK_POSTGRES_USER
  ],
  app/logic/data_load[download_db_tables_postgres, get_possible_trisk_combinations],
  app/view/sidebar_parameters,
  app/view/trisk_button,
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
        # Data Section
        tags$div(
          div(
            class = "sidebar-section",
            shiny::tags$div(class = "ui header", "Analysis"),
            shiny::tags$div(class = "ui divider"),
            # Run TRISK button
            trisk_button$ui(ns("trisk_button"))
          ),
          sidebar_parameters$ui(
            ns("sidebar_parameters"),
            available_vars = AVAILABLE_VARS
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
    if (!dir.exists(TRISK_INPUT_PATH)) { # Updated here
      dir.create(TRISK_INPUT_PATH) # Updated here
    }
    if (length(dir(TRISK_INPUT_PATH)) == 0) { # Updated here
      tables <- c(
        "assets",
        "scenarios",
        "ngfs_carbon_price",
        "financial_features"
      )

      download_db_tables_postgres(
        save_dir = TRISK_INPUT_PATH, # Updated here
        tables = tables,
        dbname = TRISK_POSTGRES_DB,
        host = TRISK_POSTGRES_HOST,
        port = TRISK_POSTGRES_PORT,
        user = TRISK_POSTGRES_USER,
        password = TRISK_POSTGRES_PASSWORD
      )
    }
    possible_trisk_combinations <- get_possible_trisk_combinations(save_dir = TRISK_INPUT_PATH) # Updated here
    trisk_run_params_r <- sidebar_parameters$server(
      "sidebar_parameters",
      possible_trisk_combinations = possible_trisk_combinations,
      available_vars = AVAILABLE_VARS, # Updated here
      hide_vars = HIDE_VARS # Updated here
    )

    results_r <- trisk_button$server(
      "trisk_button",
      trisk_run_params_r = trisk_run_params_r,
    )
  })
}
