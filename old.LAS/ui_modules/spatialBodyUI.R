### ------------------------------------------------------------------------------------------------
### --- UI Module for the "Spatial" Module ---------------------------------------------------------
### ------------------------------------------------------------------------------------------------

source("./ui_modules/defaultControlsUI.R")
source("./ui_modules/spatialDialogsUI.R")



DrawSpatialBody <- function(id) {

    # Namespacing.
    ns <- shiny::NS(id)

    shiny::tagList(
        shiny::fluidRow(

                # First box displays the "locations" view.
                shinydashboard::box(
                      title = "Spots"
                    , status = "primary"
                    , DT::DTOutput(ns("master.tbl"))
                    , DrawDefaultControls(ns("master.ctrl"))
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
              , DrawDefaultControls(ns("sites.ctrl"))
            )
        )
    )
}
