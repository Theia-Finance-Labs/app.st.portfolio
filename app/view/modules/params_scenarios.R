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
    reactive,
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
)

ui <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    div(
      class = "description",
      div(
        class = "content",
        p("Baseline Scenario"),
        div(
          class = "description",
          shiny.semantic::dropdown_input(ns("baseline_scenario"),
            choices = NULL
          )
        )
      ),
      div(
        class = "content",
        p("Target Scenario"),
        div(
          class = "description",
          shiny.semantic::dropdown_input(ns("target_scenario"),
            choices = NULL
          )
        )
      )
      # ,
      # div(
      #   class = "content",
      #   p("Scenario Geography"),
      #   div(
      #     class = "description",
      #     shiny.semantic::dropdown_input(ns("scenario_geography"),
      #       choices = NULL
      #     )
      #   )
      # )
    )
  )
}

server <- function(id,
                   hide_vars,
                   possible_trisk_combinations) {
  moduleServer(id, function(input, output, session) {
    

    # synchronise dropdown choices  with the possible combinations
    update_scenarios_dropdowns(
      input = input,
      session = session,
      hide_vars = hide_vars,
      possible_trisk_combinations = possible_trisk_combinations
    )


    # Synchronise the scenarios available depending on user scenario choice
    selected_baseline_r <- reactive({
      choice <- input$baseline_scenario
      renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      return(renamed_choice)
    })
    selected_shock_r <- reactive({
      choice <- input$target_scenario
      renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      return(renamed_choice)
    })

    # RETURN THE SCENARIOS
    scenario_config_r <- reactive({
      reactiveValues(
        baseline_scenario = selected_baseline_r(),
        target_scenario = selected_shock_r()
      )
    })

    return(scenario_config_r)
  })
}





# Synchronise the scenarios available depending on user scenario choice
update_scenarios_dropdowns <- function(input, session,
                                       hide_vars,
                                       possible_trisk_combinations) {
  # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown
  observe({
    possible_baselines <- possible_trisk_combinations |>
      dplyr::distinct(.data$baseline_scenario) |>
      dplyr::filter(!is.na(.data$baseline_scenario)) |>
      dplyr::filter(!.data$baseline_scenario %in% hide_vars$hide_baseline_scenario) |>
      dplyr::pull()

    # rename the scenarios to front end appropriate name
    new_choices <- rename_string_vector(possible_baselines, words_class = "scenarios")

    # Update target_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "baseline_scenario", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update target_scenario dropdown
  observeEvent(input$baseline_scenario, ignoreInit = TRUE, {
    selected_baseline <- rename_string_vector(input$baseline_scenario, words_class = "scenarios", dev_to_ux = FALSE)

    possible_shocks <- possible_trisk_combinations |>
      dplyr::filter(.data$baseline_scenario == selected_baseline) |>
      dplyr::distinct(.data$target_scenario) |>
      dplyr::filter(!is.na(.data$target_scenario)) |>
      dplyr::filter(!.data$target_scenario %in% hide_vars$hide_shock_scenario) |>
      dplyr::pull()


    # rename the scenarios to front end appropriate name
    new_choices <- rename_string_vector(possible_shocks, words_class = "scenarios")

    # Update target_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "target_scenario", choices = new_choices)
  })

}
