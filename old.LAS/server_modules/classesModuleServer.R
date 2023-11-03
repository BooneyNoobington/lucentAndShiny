### ------------------------------------------------------------------------------------------------
### --- Server Module for Interacting With Collections ---------------------------------------------
### ------------------------------------------------------------------------------------------------


## -- Import Functionality -------------------------------------------------------------------------
library(leaflet)                                # Use leaflet to display maps.
library(leaflet.extras)                         # Address search bar.
library(pool)                                   # Handle interaction with database.
library(shinyWidgets)                           # Nicher UI-Elements.
source("helpers.R")                             # Various helper functions.
source("./dbInterop.R")                         # Handle queries and statements.
source("./ui_modules/collectionsDialogsUI.R")   # Dynamic UI elements.

# Module server function
collectionsServer <- function(id, db.conn, coltrans.lst, sys.cnf) {
    shiny::moduleServer(
        id
      , function(input, output, session) {
        }
    )
}
