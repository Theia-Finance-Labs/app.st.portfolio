box::use(
  shiny[fileInput, moduleServer, NS, observeEvent, reactiveVal, req, tags],
)

# Define UI for upload portfolio button
ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::fileInput(ns("portfolio_file"),
      label = NULL,
      buttonLabel = "Upload Portfolio",
      accept = c(".xlsx")
    ),
    shiny::downloadButton(ns("download_portfolio"), "Download Example Portfolio")
  )
}

# Define server logic for upload portfolio button
server <- function(id, assets_data) {
  moduleServer(id, function(input, output, session) {
    portfolio_data_r <- reactiveVal(NULL)

    observeEvent(input$portfolio_file, {
      req(input$portfolio_file)
      # Logic to handle the uploaded file
      portfolio_data <- readxl::read_excel(input$portfolio_file$datapath)
      output$upload_status <- shiny::renderText("Portfolio uploaded successfully!")
      portfolio_data_r(portfolio_data)
    })


    # Logic for downloading the portfolio example
    output$download_portfolio <- shiny::downloadHandler(
      filename = function() {
        "trisk_portfolio_example.xlsx"
      },
      content = function(file) {
        # Path to the example portfolio stored locally
        file.copy("app/data/trisk_portfolio.xlsx", file)
      }
    )

    return(portfolio_data_r)
  })
}
