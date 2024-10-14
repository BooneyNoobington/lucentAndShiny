### ------------------------------------------------------------------------------------------------
### --- Dialogs for the Spatial Module -------------------------------------------------------------
### ------------------------------------------------------------------------------------------------


insertSpot <- function(ns, longitude.dbl = NULL, latitude.dbl = NULL) {

    shiny::showModal(
        shiny::modalDialog(

              title = "Add Investigation Spot"

            , shiny::textInput(ns("name"), NULL, placeholder = "Spot Name", value = NULL)
            , shiny::textInput(ns("description"), NULL, placeholder = "Further Description",
                                                                              value = NULL)
            , shiny::textInput(ns("x"), NULL, placeholder = "Longitude",
                                                  value = longitude.dbl)
            , shiny::textInput(ns("y"), NULL, placeholder = "Latitude",
                                                  value = latitude.dbl)

            , footer = shiny::tagList(
                  shiny::modalButton("Dismiss")
                , shiny::actionButton(ns("spot.add_record"), NULL, shiny::icon("plus"))
              )

            , easyClose = TRUE
            , fade = TRUE
        )
    )
    
}



insertSite <- function(ns, borders.pol, crs.df) {
    shiny::showModal(
        shiny::modalDialog(
            title = "Add Investigation Site"

          , shiny::textInput(ns("site.name"), NULL, placeholder = "Site Name", value = NULL)
          , shiny::textInput(ns("site.borders"), NULL, placeholder = "Borders",
                                                           value = borders.pol)

          , shiny::textInput(ns("site.description"), NULL, placeholder = "Site Description"
                                                                            , value = NULL)

          , footer = shiny::div(
                shiny::modalButton("Dismiss")
              , shiny::actionButton(ns("site.add_record"), NULL, shiny::icon("plus"))
            )

          , size = "s"
          , easyClose = TRUE
          , fade = TRUE
        )
    )
}
