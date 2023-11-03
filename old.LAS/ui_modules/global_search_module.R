### ------------------------------------------------------------------------------------------------
### --- UI Module for Menu Searches ----------------------------------------------------------------
### ------------------------------------------------------------------------------------------------

DrawMenuSearch <- function(id) {
    ns <- shiny::NS(id)
    shinydashboard::sidebarSearchForm(
        textId = id
      , buttonId = NS(id, "searchButton")
      , label = "Search..."
    )
}
