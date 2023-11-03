### ------------------------------------------------------------------------------------------------
### --- UI Module for Sample Interaction -----------------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawSamplesMenu <- function(id) {
    ns <- shiny::NS(id)
    shinydashboard::menuItem(
        "Samples"
      , tabName = ns(id), icon = shiny::icon("vials")
      , shinydashboard::menuSubItem("Results", tabName = "results", icon = shiny::icon("table"))
    )
}
