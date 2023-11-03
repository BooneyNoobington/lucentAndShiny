### ------------------------------------------------------------------------------------------------
### --- UI Module for a shinydashboard Sidebar -----------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawSidebar <- function(id) {

    ns <- NS(id)

    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            DrawMenuSearch("dashbar_search")
          , shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = shiny::icon("dashboard"))
          , DrawSamplesMenu("samples")
          , DrawSpatialMenu("spatial")
          # shinydashboard::menuItem("Locations", tabName = "spatial", icon = shiny::icon("map"))
          , DrawSimpleMenu("objects", "Objects", "bullseye")
        )
    )
}
