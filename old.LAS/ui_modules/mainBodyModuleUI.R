### ------------------------------------------------------------------------------------------------
### --- UI Module for spatial / geographix data ----------------------------------------------------
### ------------------------------------------------------------------------------------------------


source("./ui_modules/spatialBodyUI.R")


DrawMainBody <- function(id) {
    ns <- NS(id)

    shiny::tagList(
        shinydashboard::tabItems(
            shinydashboard::tabItem(DrawSpatialBody("spatial"))
        )
    )
}
