### ------------------------------------------------------------------------------------------------
### --- Dialogs for the Spatial Module -------------------------------------------------------------
### ------------------------------------------------------------------------------------------------


DrawAddSpot <- function(ns, longitude.dbl = NULL, latitude.dbl = NULL) {

    shiny::modalDialog(

          title = "Add Investigation Spot"

        , shiny::textInput(ns("identifier"), NULL, placeholder = "Spot Name", value = NULL)
        , shiny::textInput(ns("description"), NULL, placeholder = "Further Description",
                                                                           value = NULL)
        , shiny::textInput(ns("x"), NULL, placeholder = "Longitude",
                                              value = longitude.dbl)
        , shiny::textInput(ns("y"), NULL, placeholder = "Latitude",
                                              value = latitude.dbl)

        , footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            , shiny::actionButton(ns("add_record"), NULL, shiny::icon("plus"))
          )

        , easyClose = TRUE
        , fade = TRUE
    )
}



DrawAddSite <- function(ns, borders.pol, crs.df) {
    shiny::modalDialog(
        title = "Add Investigation Site"

      , shiny::textInput(ns("site.identifier"), NULL, placeholder = "Site Name", value = NULL)
      , shiny::textInput(ns("site.class"), NULL, placeholder = "Type of Site", value = NULL)
      , shiny::textInput(ns("site.borders"), NULL, placeholder = "Borders", value = borders.pol)
      , shinyWidgets::pickerInput(
            inputId = "site.crs"
          , label = "Select a CRS"
          , choices = crs.df$identifier
          , options = list(
                "live-search" = TRUE  # Enable live search/autocomplete
            )
        )
      , shiny::textInput(ns("site.description"), NULL, placeholder = "Description", value = NULL)

      , footer = shiny::div(
            shiny::modalButton("Dismiss")
          , shiny::actionButton(ns("site.add_record"), NULL, shiny::icon("plus"))
        )

      , size = "s"
      , easyClose = TRUE
      , fade = TRUE
    )
}
