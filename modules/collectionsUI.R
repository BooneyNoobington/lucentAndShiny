### ------------------------------------------------------------------------------------------------
### --- UI Module for the "Collections" Module -----------------------------------------------------
### ------------------------------------------------------------------------------------------------

source("./modules/defaultCtrlUI.R")



collectionsUI <- function(id) {

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
                    , defaultCtrlUI(ns("collections.ctrl"))
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
                        , defaultCtrlUI(ns("sites.ctrls"))
                    )

                  , shiny::tabPanel(
                          "Actions"
                        , DT::DTOutput(ns("actions.tbl"))
                        , defaultCtrlUI(ns("actions.ctrl"))
                    )
                )

            )
        )

    )
}
