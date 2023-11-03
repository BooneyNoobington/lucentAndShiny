### ------------------------------------------------------------------------------------------------
### --- UI Module for a Simple Module Menu ---------------------------------------------------------
### ------------------------------------------------------------------------------------------------


# Draw A simple Menu. No Submenus. Just a name and an icon.
DrawSimpleMenu <- function(id, text, icon) {
    ns <- shiny::NS(id)
    shinydashboard::menuItem(
        text
      , tabName = id, icon = shiny::icon(icon)
    )
}
