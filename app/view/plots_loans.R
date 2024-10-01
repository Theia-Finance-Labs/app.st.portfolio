box::use(
  shiny[moduleServer, NS, observeEvent, plotOutput, renderPlot, tags],
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  shiny::fluidRow(
    semantic.dashboard::box(
      title = "PD Difference", width = 8, collapsible = FALSE,
      plotOutput(ns("pd_term_plot_output"), height = "100%")
    ),
    semantic.dashboard::box(
      title = "Expected Loss", width = 8, collapsible = FALSE,
      plotOutput(ns("expected_loss_plot_output"), height = "100%")
    )
  )
}

####### Server



server <- function(id, trisk_results_r) {
  moduleServer(id, function(input, output, session) {
    base_height_per_facet <- 150 # height in pixels # TODO GO IN CONF

    # PD PLOT

    observeEvent(trisk_results_r(), {
      if ((nrow(trisk_results_r()) > 0)) {
        num_facets <- length(trisk_results_r() |> dplyr::distinct(.data$sector) |> dplyr::pull())
        pd_term_plot <- trisk.analysis::pipeline_crispy_pd_term_plot(
          analysis_data = trisk_results_r(),
          facet_var = "sector"
        )
        # id value dynamically generated in the server, just above
        output$pd_term_plot_output <- shiny::renderPlot(
          {
            pd_term_plot
          },
          height = num_facets * base_height_per_facet
        )
      } else {
        output$pd_term_plot_output <- shiny::renderPlot(ggplot2::ggplot() +
          ggplot2::theme_void(), width = 1, height = 1)
      }
    })

    # EXPECTED LOSS PLOT

    observeEvent(trisk_results_r(), {
      if (nrow(trisk_results_r()) > 0) {
        num_facets <- length(trisk_results_r() |> dplyr::distinct(.data$sector) |> dplyr::pull())
        expected_loss_plot <- trisk.analysis::pipeline_crispy_expected_loss_plot(
          analysis_data = trisk_results_r(),
          facet_var = "sector"
        )

        output$expected_loss_plot_output <- shiny::renderPlot(
          {
            expected_loss_plot
          },
          height = num_facets * base_height_per_facet
        )
      } else {
        output$expected_loss_plot_output <- shiny::renderPlot(ggplot2::ggplot() +
          ggplot2::theme_void(), width = 1, height = 1)
      }
    })
  })
}
