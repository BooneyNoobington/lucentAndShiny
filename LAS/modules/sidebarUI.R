### ------------------------------------------------------------------------------------------------
### --- UI Module for a shinydashboard Sidebar -----------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawSidebarMenu <- function() {

    shinydashboard::sidebarMenu(
        # Global search field.
        shinydashboard::sidebarSearchForm(textId = "global_search" ,
             buttonId = "global_search_button", label = "Search...")
        # Dashboard. "Landing page".
      , shinydashboard::menuItem("Dashboard", tabName = "dashboard" ,
                                     icon = shiny::icon("dashboard"))
        # Collections. One of the hearts of lucentLIMS.
      , shinydashboard::menuItem("Collections", tabName = "collections",
                                            icon = shiny::icon("vials"))
        # Spatial information: Spots and Sites.
      , shinydashboard::menuItem("Locations", tabName = "locations", icon = shiny::icon("map"))
        # Units, formulas and such.
      , shinydashboard::menuItem("Scientific", tabName = "scientific", icon = shiny::icon("atom"))
    )

}
