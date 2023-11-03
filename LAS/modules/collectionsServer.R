### ------------------------------------------------------------------------------------------------
### --- Server Module for Interacting With Collections ---------------------------------------------
### ------------------------------------------------------------------------------------------------


## -- Import Functionality -------------------------------------------------------------------------
library(leaflet)                                # Use leaflet to display maps.
library(leaflet.extras)                         # Address search bar.
library(pool)                                   # Handle interaction with database.
library(shinyWidgets)                           # Nicer UI-Elements.
source("helpers.R")                             # Various helper functions.
source("./dbInterop.R")                         # Handle queries and statements.
source("./modules/newCollectionUI.R")           # Dynamic UI elements.
source("./modules/newCollectionServer.R")       # Submodule for a new collection.


# Module server function
collectionsServer <- function(id, db.conn, coltrans.lst, sys.cnf) {
    shiny::moduleServer(
        id
      , function(input, output, session) {

            ## -- The master table -----------------------------------------------------------------
            collections.query <- shiny::reactiveVal(
                paste(readLines("./SQL/QUERY_COLLECTIONS.SQL"), collapse = " ")
            )

            # Start with an empty result set.
            collections.df <- shiny::reactiveVal(NULL)

            # Every time the query changes, execute the new query into a data frame.
            shiny::observeEvent(
                collections.query()
              , {collections.df(pool::dbGetQuery(db.conn, collections.query()))}
            )

            # Display the results in a table element.
            output$collections.tbl <- DT::renderDT(
                collections.df()
              , rownames = FALSE
              , colnames = unlist(coltrans.lst[which(coltrans.lst %in% names(collections.df()))])
            )


            ## -- Other Reactives ------------------------------------------------------------------
            object.stmt <- shiny::reactiveVal(NULL)


            ## -- Handle Adding a New Collection ---------------------------------------------------

            # Display the UI.
            shiny::observeEvent(
                input[["collections.ctrl-add_record"]]
              , {
                    # Launch an accompanying server.
                    newCollectionServer("new_collection", db.conn, sys.cnf)
                }
            )


        }
    )
}
