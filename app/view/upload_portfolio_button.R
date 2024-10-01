box::use(
  shiny[fileInput, moduleServer, NS, observeEvent, reactiveVal, req, tags],
)

# Define UI for upload portfolio button
ui <- function(id) {
  ns <- NS(id)
  shiny.semantic::fileInput(ns("portfolio_file"),
    label = NULL,
    buttonLabel = "Upload Portfolio",
    accept = c(".xlsx", ".csv")
  )
}

# Define server logic for upload portfolio button
server <- function(id, assets_data) {
  moduleServer(id, function(input, output, session) {
    portfolio_data_r <- reactiveVal(create_random_portfolio(assets_data))

    observeEvent(input$portfolio_file, {
      req(input$portfolio_file)
      # Logic to handle the uploaded file
      portfolio_data <- read.csv(input$portfolio_file$datapath)
      output$upload_status <- renderText("Portfolio uploaded successfully!")
      portfolio_data_r(portfolio_data)
    })
    return(portfolio_data_r)
  })
}


create_random_portfolio <- function(assets_data, n_sample = 15) {
  mock_portfolio <- assets_data |>
    dplyr::distinct(company_name, sector, technology, country_iso2) |>
    dplyr::sample_n(n_sample) |>
    dplyr::mutate(
      exposure_value_usd = stats::runif(dplyr::n(), min = 10000, max = 10000000), # Random values between 10000 and 10000000
      term = sample(c(1:5, NA), dplyr::n(), replace = TRUE, prob = c(rep(0.2, 5), 0.1)), # Random values between 1 and 5, or NA
      loss_given_default = ifelse(is.na(.data$term), NA, stats::runif(dplyr::n(), min = 0, max = 1)) # Random values between 0 and 1, or NA if term is NA
    )
  return(mock_portfolio)
}
