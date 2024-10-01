box::use(
  semantic.dashboard[dashboardSidebar],
  shiny[
    conditionalPanel,
    div,
    eventReactive,
    HTML,
    img,
    moduleServer,
    NS,
    observe,
    observeEvent,
    p,
    reactiveVal,
    reactiveValues,
    tagList,
    tags
  ],
  shiny.semantic[dropdown_input, segment, slider_input, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
)

box::use(
  app/logic/renamings[rename_string_vector],
  app/view/modules/params_scenarios,
  app/view/modules/params_trisk,
)


####### UI
ui <- function(id, available_vars) {
  ns <- NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(HTML(paste0("
        .sidebar-section {
          padding: 20px;
          background-color: #f9f9f9;
          margin: 15px 0;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .sidebar-section .ui.header {
          font-size: 18px;
          color: #333;
          margin-bottom: 15px;
        }
        .ui.button {
          background-color: #d4d4d5;
          color: white;
          border: none;
          border-radius: 4px;
          margin: 8px 0;
          display: block;
          width: 100%; /* Ensures full width */
        }
        .ui.buttons {
          width: 50%; /* Ensures full width */
        }
        .ui.divider {
          margin: 20px 0;
        }
        .ui.dropdown {
          width: 100%;
          margin-bottom: 10px;
        }
      ")))
    ),

    # Scenario Choice Section
    div(
      class = "sidebar-section",
      shinyjs::useShinyjs(),
      shiny::tags$div(class = "ui header", "Scenario Choice"),
      shiny::tags$div(class = "ui divider"),
      # Scenario Choice
      params_scenarios$ui(ns("params_scenarios"))
    ),
    # TRISK Parameters Section
    div(
      class = "sidebar-section",
      shiny::tags$div(class = "ui header", "TRISK Parameters"),
      shiny::tags$div(class = "ui divider"),
      params_trisk$ui(ns("params_trisk"), available_vars)
    )
  )
}


####### Server


server <- function(id,
                   possible_trisk_combinations,
                   available_vars,
                   hide_vars) {
  moduleServer(id, function(input, output, session) {
    # get scenario config
    scenario_config_r <- params_scenarios$server(
      "params_scenarios",
      hide_vars = hide_vars,
      possible_trisk_combinations = possible_trisk_combinations
    )

    # get other trisk params confid
    trisk_config_r <- params_trisk$server("params_trisk", available_vars)

    # reactive variable containing trisk run parameters
    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        baseline_scenario = scenario_config_r()$baseline_scenario,
        target_scenario = scenario_config_r()$target_scenario,
        scenario_geography = "Global", # scenario_config_r()$scenario_geography,
        shock_year = trisk_config_r()$shock_year,
        discount_rate = trisk_config_r()$discount_rate,
        risk_free_rate = trisk_config_r()$risk_free_rate,
        growth_rate = trisk_config_r()$growth_rate,
        div_netprofit_prop_coef = trisk_config_r()$div_netprofit_prop_coef,
        carbon_price_model = trisk_config_r()$carbon_price_model,
        market_passthrough = trisk_config_r()$market_passthrough
      )
    })


    return(trisk_run_params_r)
  })
}
