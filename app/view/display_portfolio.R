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
    updateSelectizeInput
  ],
  shiny.semantic[button, segment, semanticPage],
  shinyjs[runjs, useShinyjs],
)



ui <- function(id, portfolio_class = "") {
  ns <- NS(id)
  semantic.dashboard:::box(
    title = "Portfolio", width = 16, collapsible = TRUE,
    DT::dataTableOutput(outputId = ns("portfolio_table")),
  )
}


server <- function(
    id,
    trisk_results_r) {
  moduleServer(id, function(input, output, session) {

    observeEvent(trisk_results_r(), ignoreInit=TRUE, {
        
        table_to_display <- trisk_results_r() |>
          trisk.analysis:::compute_analysis_metrics()|>
          dplyr::select(.data$sector, .data$technology, .data$country_iso2, .data$exposure_value_usd, .data$term, .data$loss_given_default, .data$crispy_perc_value_change, .data$expected_loss_shock) |>
          dplyr::rename(
            npv_change = .data$crispy_perc_value_change,
            expected_loss = expected_loss_shock
            )


        # Hardcode the index for npv_change (which column it is, 0-based index)
        colored_columns <- 7 # npv_change is in the 7th column, so index is 6 (0-based index)


        # Render the datatable
        output$portfolio_table <-     output$portfolio_table <- DT::renderDT(
      {
        DT::datatable(
            table_to_display,
            editable = FALSE, # Disable all cell editing
            selection = "multiple",
            options = list(
            lengthChange = FALSE, # Remove "Show XXX entries" option
            paging = FALSE, # Remove pagination
            searching = FALSE, # Remove search input
            info = FALSE, # Remove "Showing N of X entries"
            columnDefs = list(
                # Apply color change only to the npv_change column
                list(targets = colored_columns, createdCell = JS(
                "function(cell, cellData, rowData) {
                    $(cell).css('color', cellData < 0 ? 'red' : 'green');
                }"
                ))
            )
            ),
            class = "display compact" # Fit the table to the container
        )
      })
})

  })}
