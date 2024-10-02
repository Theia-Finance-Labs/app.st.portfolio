box::use(
  semantic.dashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
  shiny[
    div,
    HTML,
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
  app/view/display_portfolio,
  app/view/plots_equities,
  app/view/plots_loans,
  app/view/sidebar_parameters,
  app/view/trisk_button,
  app/view/upload_portfolio_button,
)



# Define the UI function
#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny.semantic::semanticPage(
    shinyjs::useShinyjs(), # Initialize shinyjs
    tags$div(
      class = "header", # Add a loading overlay
      tags$head(
        tags$style(HTML("
            #loading-overlay {
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              background: rgba(255, 255, 255, 0.8);
              z-index: 9999;
              display: flex;
              align-items: center;
              justify-content: center;
              font-size: 2em;
            }
          "))
      ),
      div(id = "loading-overlay", "Initializing...")
    ),
    dashboardPage(
      title = "Crispy",
      # dashboardHeader
      dashboardHeader(title = "Portfolio ST"),
      # dashboardSidebar
      dashboardSidebar(
        # Data Section
        tags$div(
          tags$div(
            class = "sidebar-section",
            shiny::tags$div(class = "ui header", "Analysis"),
            shiny::tags$div(class = "ui divider"),
            # Button container with vertical spacing
            tags$div(
              class = "ui stackable aligned grid", # Centered and stackable grid layout for better alignment
              tags$div(
                class = "row",
                upload_portfolio_button$ui(ns("upload_portfolio_button"))
              ),
              tags$div(
                class = "row",
                trisk_button$ui(ns("trisk_button"))
              )
            )
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
        shiny::tags$div(
          class = "ui stackable grid",
          display_portfolio$ui(ns("display_portfolio")),
          plots_equities$ui(ns("plots_equities")),
          plots_loans$ui(ns("plots_loans"))
        )
      )
    )
  )
}


# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    if (!dir.exists(TRISK_INPUT_PATH)) {
      dir.create(TRISK_INPUT_PATH)
    }
    if (length(dir(TRISK_INPUT_PATH)) == 0) {
      tables <- c(
        "assets",
        "scenarios",
        "ngfs_carbon_price",
        "financial_features"
      )

      download_db_tables_postgres(
        save_dir = TRISK_INPUT_PATH,
        tables = tables,
        dbname = TRISK_POSTGRES_DB,
        host = TRISK_POSTGRES_HOST,
        port = TRISK_POSTGRES_PORT,
        user = TRISK_POSTGRES_USER,
        password = TRISK_POSTGRES_PASSWORD
      )
    }

    assets_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "assets.csv"), show_col_types = FALSE)
    scenarios_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "scenarios.csv"), show_col_types = FALSE)
    financial_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "financial_features.csv"), show_col_types = FALSE)
    carbon_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "ngfs_carbon_price.csv"), show_col_types = FALSE)

    possible_trisk_combinations <- get_possible_trisk_combinations(scenarios_data = scenarios_data)
    trisk_run_params_r <- sidebar_parameters$server(
      "sidebar_parameters",
      possible_trisk_combinations = possible_trisk_combinations,
      available_vars = AVAILABLE_VARS,
      hide_vars = HIDE_VARS
    )
    portfolio_data_r <- upload_portfolio_button$server("upload_portfolio_button", assets_data)


    trisk_results_r <- trisk_button$server(
      "trisk_button",
      assets_data = assets_data,
      scenarios_data = scenarios_data,
      financial_data = financial_data,
      carbon_data = carbon_data,
      portfolio_data_r = portfolio_data_r,
      trisk_run_params_r = trisk_run_params_r
    )

    display_portfolio$server("display_portfolio", trisk_results_r)
    plots_equities$server("plots_equities", trisk_results_r = trisk_results_r)
    plots_loans$server("plots_loans", trisk_results_r = trisk_results_r)

    shinyjs::runjs('$("#loading-overlay").hide();')
  })
}
