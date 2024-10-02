box::use(
  shiny[fluidRow, moduleServer, NS, observeEvent, plotOutput, renderPlot],
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    semantic.dashboard::box(
      title = "NPV Change",
      width = 8,
      collapsible = FALSE,
      plotOutput(ns("crispy_npv_change_plot"))
    ),
    semantic.dashboard::box(
      title = "Exposure Change",
      width = 8,
      collapsible = FALSE,
      plotOutput(ns("exposure_change_plot"))
    )
  )
}

####### Server



server <- function(id, trisk_results_r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(trisk_results_r(), {
      crispy_npv_change_plot <- trisk.analysis::pipeline_crispy_npv_change_plot(
        trisk_results_r(),
        x_var = "technology"
      )
      output$crispy_npv_change_plot <- renderPlot({
        crispy_npv_change_plot
      })

      exposure_change_plot <- trisk.analysis::pipeline_crispy_exposure_change_plot(
        trisk_results_r(),
        x_var = "technology"
      )
      output$exposure_change_plot <- renderPlot({
        exposure_change_plot
      })
    })
  })
}
