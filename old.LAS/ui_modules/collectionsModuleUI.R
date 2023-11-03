### ------------------------------------------------------------------------------------------------
### --- UI Module for the "Collections" Module -----------------------------------------------------
### ------------------------------------------------------------------------------------------------

source("./ui_modules/collectionsContextualControlsUI.R")



DrawCollectionsBody <- function(id) {

    # Namespacing.
    ns <- shiny::NS(id)

    # Prepare these UI elements.
    shiny::tagList(

        # First row.
        shiny::fluidRow(

                # First box displays the "locations" view.
                shinydashboard::box(
                      title = "Collections"
                    , status = "primary"
                    , DT::DTOutput(ns("collections.tbl"))

                      # Have default controls and contextual controls in one row.
                    , DrawCollectionClassBtns(ns("collections.ctrl"))
                )

                # Second box displays an Open Street map segment.
              , shinydashboard::box(
                    leaflet::leafletOutput(ns("master.map"))
                )

        )

        # "Detail row".
      , shiny::fluidRow(
            shinydashboard::box(
                title = "Details"
              , width = 12

              , shiny::tabsetPanel(

                    shiny::tabPanel(
                          "Values"
                        , DT::DTOutput(ns("values.tbl"))
                        , DrawDefaultControls(ns("sites.ctrls"))
                    )

                  , shiny::tabPanel(
                          "Actions"
                        , DT::DTOutput(ns("actions.tbl"))
                        , DrawDefaultControls(ns("actions.ctrl"))
                    )
                )

            )
        )

    )
}
