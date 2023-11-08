### ------------------------------------------------------------------------------------------------
### --- UI Module for the "Spatial" Module ---------------------------------------------------------
### ------------------------------------------------------------------------------------------------

source("./modules/defaultCtrlUI.R")
source("./modules/locationsDialogs.R")



locationsUI <- function(id) {

    # Namespacing.
    ns <- shiny::NS(id)

    shiny::tagList(
        shiny::fluidRow(

                # First box displays the "locations" view.
                shinydashboard::box(
                      title = "Spots"
                    , status = "primary"
                    , DT::DTOutput(ns("master.tbl"))
                    , shiny::div(
                          shiny::div(style="display: inline-block; width: 150px ;", defaultCtrlUI(ns("master.ctrl")))
                        , shiny::div(style="display: inline-block; width: 75px ;", shiny::actionButton(ns("link_spot"), NULL, shiny::icon("link")))
                      )
                )

                  # Second box displays an Open Street map segment.
                , shinydashboard::box(
                    leaflet::leafletOutput(ns("master.map"))
                )

        )

      , shiny::fluidRow(
            shinydashboard::box(
                title = "Sites"
              , width = 12
              , DT::DTOutput(ns("sites.tbl"))
              , defaultCtrlUI(ns("sites.ctrl"))
            )
        )
    )
}
