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

            shiny::observeEvent(
                input[["collections.ctrl-add_record"]]
              , {
                    # Compute the next identifier.
                    next.name.v <- GetNextIdentifier(
                        recent_ident.chr = GetMaxIdentifier(
                            db.conn, VectorizeNumberingSpec(sys.cnf$numbering)
                        )
                      , numbering.lst = VectorizeNumberingSpec(sys.cnf$numbering)
                    )

                    # This information is needed to provide an "add collection" dialog.
                    objects.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `object`")$name
                    matrices.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `matrix`")$name
                    col_classes.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `collection_class`")$name
                    spots.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `spot`")$name
                    sites.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `site`")$name

                    # Display a Modal Dialog that asks for the respective information.
                    shiny::showModal(
                        DrawAddCollection(
                            paste(next.name.v, collapse = "·")
                          , objects.v
                          , matrices.v
                          , col_classes.v
                          , spots.v
                          , sites.v
                        )
                    )
                }
            )


            ## -- Handle Adding a New Object -------------------------------------------------------

            # Show the modal dialog.
            shiny::observeEvent(
                input[["collections.ctrl-add_object"]]
              , {shiny::showModal(DrawAddObject(session$ns))}
            )

            # Observe clicks in the modal dialog.
            shiny::observeEvent(
                input$add_object.dialog.add_record
              , {
                    object.stmt(
                        CreateStatementByList(
                            table.chr = "object"
                          , kav.lst = list(
                                name = input$add_object.dialog.name
                              , description = input$add_object.dialog.description
                            )
                        )
                    )

                    # Close the dialog when done.
                    shiny::removeModal()
                }
            )

            # Observe the statement on `object`.
            shiny::observeEvent(
                object.stmt()  # Fire whenever the statement changes.
              , {pool::dbExecute(db.conn, object.stmt())}
            )

        }
    )
}
