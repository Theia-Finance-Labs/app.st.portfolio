box::use(
  DT[datatable, dataTableProxy, DTOutput, JS, renderDT],
  semantic.dashboard[box, icon],
  shiny[
    div,
    eventReactive,
    HTML,
    moduleServer,
    NS,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    reactiveValues,
    selectizeInput,
    tags,
    updateSelectizeInput,
    downloadHandler,
    downloadButton
  ],
  shiny.semantic[button, segment, semanticPage],
  shinyjs[runjs, useShinyjs],
  writexl[write_xlsx]  # Use writexl to save as an Excel file
)

ui <- function(id, portfolio_class = "") {
  ns <- NS(id)
  semantic.dashboard:::box(
    title = "Portfolio", width = 16, collapsible = TRUE,
    div(
      DT::dataTableOutput(outputId = ns("portfolio_table")),
      div(style = "margin-top: 10px;",
        shiny::downloadButton(ns("download_btn"), "Download Excel")  # Using shiny::downloadButton
      )
    )
  )
}

server <- function(
    id,
    trisk_results_r) {
  moduleServer(id, function(input, output, session) {
    
    # ReactiveVal to store table data
    table_to_display <- reactiveVal(NULL)
    
    # Update table_to_display whenever trisk_results_r changes
    observeEvent(trisk_results_r(), ignoreInit = TRUE, {
      computed_table <- trisk_results_r() |>
      trisk.analysis:::compute_analysis_metrics() |>
      dplyr::select(.data$company_id, .data$company_name, .data$sector, .data$technology, .data$country_iso2, .data$exposure_value_usd, .data$term, .data$loss_given_default, .data$crispy_perc_value_change, .data$expected_loss_shock) |>
      dplyr::rename(
        npv_change = .data$crispy_perc_value_change,
        expected_loss = .data$expected_loss_shock
      ) |>
      dplyr::mutate(
        sector = dplyr::if_else(is.na(.data$company_id), NA_character_, .data$sector),
        technology = dplyr::if_else(is.na(.data$company_id), NA_character_, .data$technology),
        country_iso2 = dplyr::if_else(is.na(.data$company_id), NA_character_, .data$country_iso2),
        exposure_value_usd = dplyr::if_else(is.na(.data$company_id), NA_real_, .data$exposure_value_usd),
        term = dplyr::if_else(is.na(.data$company_id), NA_real_, .data$term),
        loss_given_default = dplyr::if_else(is.na(.data$company_id), NA_real_, .data$loss_given_default),
        npv_change = dplyr::if_else(is.na(.data$company_id), NA_real_, .data$npv_change),
        expected_loss = dplyr::if_else(is.na(.data$company_id), NA_real_, .data$expected_loss)
      ) |>
      dplyr::select(-.data$company_id)  
      

      # Store computed table in reactiveVal
      table_to_display(computed_table)
    })
    
    # Render the datatable
    output$portfolio_table <- DT::renderDT({
      DT::datatable(
        table_to_display(),
        editable = FALSE, # Disable all cell editing
        selection = "multiple",
        options = list(
          lengthChange = FALSE, # Remove "Show XXX entries" option
          paging = FALSE, # Remove pagination
          searching = FALSE, # Remove search input
          info = FALSE, # Remove "Showing N of X entries"
          columnDefs = list(
            # Apply color change only to the npv_change column
            list(targets = 8, createdCell = JS(
              "function(cell, cellData, rowData) {
                  $(cell).css('color', cellData < 0 ? 'red' : 'green');
              }"
            ))
          )
        ),
        class = "display compact" # Fit the table to the container
      )
    })
    
    # Download handler for Excel download
    output$download_btn <- downloadHandler(
      filename = function() {
        paste("trisk_portfolio_", Sys.Date(), "_", format(Sys.time(), "%H-%M-%S"), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(table_to_display(), file)
      }
    )
  })
}
