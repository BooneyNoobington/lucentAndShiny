### ------------------------------------------------------------------------------------------------
### --- UI Module for Managing Objects -------------------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawObjectsMenu <- function(id) {
    ns <- shiny::NS(id)
    shinydashboard::menuItem(
        "Objects"
      , tabName = id, icon = shiny::icon("bullseye")
    )
}
