### ------------------------------------------------------------------------------------------------
### --- UI Module for spatial / geographic data ----------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawSpatialMenu <- function(id) {
    ns <- shiny::NS(id)

    shinydashboard::menuItem(
        "Locations"
      , tabName = ns(id)
      , icon = shiny::icon("map")

        # Display a menu for cities as a submenu.
      , shinydashboard::menuItem("Cities", tabName = "cities", icon = shiny::icon("city"))
    )
}
