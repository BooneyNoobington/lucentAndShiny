### ------------------------------------------------------------------------------------------------
### --- UI Module for a shinydashboard Sidebar -----------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawSidebarMenu <- function(i18n) {

    shinydashboard::sidebarMenu(

        # Global search field.
        shinydashboard::sidebarSearchForm(
            textId = "global_search"
          , buttonId = "global_search_button"
          , label = i18n$t("Search...")
        )

        # Dashboard. "Landing page".
      , shinydashboard::menuItem(
            i18n$t("Dashboard")
          , tabName = "dashboard"
          , icon = shiny::icon("dashboard")
        )

        # Collections. One of the hearts of lucentLIMS.
      , shinydashboard::menuItem(
            i18n$t("Collections")
          , tabName = "collections"
          , icon = shiny::icon("vials")
        )

        # Spatial information: Spots and Sites.
      , shinydashboard::menuItem(
            i18n$t("Locations")
          , tabName = "locations"
          , icon = shiny::icon("map")
        )

        # Units, formulas and such.
      , shinydashboard::menuItem(
            i18n$t("Scientific")
          , tabName = "scientific"
          , icon = shiny::icon("atom")
        )

      , shinydashboard::menuItem(
            i18n$t("Management")
          , tabName = "management"
          , icon = shiny::icon("list-check")
        )
    )

}
