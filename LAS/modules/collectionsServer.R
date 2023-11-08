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
source("./modules/newCollectionDialog.R")       # Dynamic UI elements.
source("./modules/aceDialog.R")                 # Submodule for SQL info.



# Module server function
collectionsServer <- function(id, db.conn, coltrans.lst, sys.cnf) {
    shiny::moduleServer(
        id
      , function(input, output, session) {


            ## -- The master table -----------------------------------------------------------------
            collections.query <- shiny::reactiveVal(
                paste(readLines("./SQL/QUERY_COLLECTIONS.SQL"), collapse = "\n")
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

                    # Compute the next identifier.
                    current.name.v <- GetMaxIdentifier(
                        db.conn
                      , VectorizeNumberingSpec(sys.cnf$numbering)
                    )

                    next.name.v <- GetNextIdentifier(
                          recent_ident.chr = current.name.v
                        , numbering.lst = VectorizeNumberingSpec(sys.cnf$numbering)
                    )


                    # This information is needed to provide an "add collection" dialog.
                    matrices.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `matrix`")$name
                    col_classes.v <- pool::dbGetQuery(
                        db.conn, "SELECT `name` FROM `collection_class`"
                    )$name
                    spots.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `spot`")$name
                    sites.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `site`")$name

                    # Display a Modal Dialog that asks for the respective information.
                    shiny::showModal(
                        newCollectionDialog(
                              session$ns
                            , paste(next.name.v, collapse = "·")
                            , objects.v
                            , matrices.v
                            , col_classes.v
                            , spots.v
                            , sites.v
                        )
                    )
                }
            )


            ## -- React to user input --------------------------------------------------------------
            # Add a new collection.
            shiny::observeEvent(
                input$add_collection
              , {

                    # Decide wether or not to insert a new matrix.
                    if(! input$matrix.name.set == "") {
                        # Insert new matrix to database.
                        pool::dbExecute(
                            db.conn
                          , CreateStatementByList(
                                table = "matrix"
                              , kav.lst = HandleParanthesisInput(input$matrix.name.set)
                           )
                        )

                        matrix.chr <- input$matrix.name.set
                    } else {  # If no matrix was provided, one was picked.
                        matrix.chr <- input$matrix.name.pick
                    }

                    # Same for a collection class.
                    if(! input$collection_class.name.set == "") {
                        pool::dbExecute(
                            db.conn
                          , CreateStatementByList(
                                table = "collection_class"
                              , kav.lst = HandleParanthesisInput(input$collection_class.name.set)
                           )
                        )

                        collection_class.chr <- input$collection_class.name.set
                    } else {
                        collection_class.chr <- input$collection_class.name.pick
                    }

                    # Actual insert.
                    new_collection.lst <- list(
                        name = input$collection.name
                      , id_matrix = GetIDFromName(db.conn, matrix.chr, "matrix")
                      , id_collection_class = GetIDFromName(db.conn, collection_class.chr
                                                                    , "collection_class")
                    )

                    # Based on what the user clicked, relate this new collection
                    # to a spot, site, compound, etc..
                    if (input$relate_to_choice == "Spot") {
                        new_collection.lst$id_spot = GetIDFromName(db.conn, input$spot.name.pick
                                                                                       , "spot")
                    } else if (input$relate_to_choice == "Site") {
                        new_collection.lst$id_site = GetIDFromName(db.conn, input$site.name.pick
                                                                                       , "site")
                    } else if (input$relate_to_choice == "Compound") {
                        new_collection.lst$id_comound = GetIDFromName(db.conn
                                      , input$compound.name.pick, "compound")
                    }

                    # Insert the new collection.
                    pool::dbExecute(
                        db.conn
                      , CreateStatementByList(table = "collection", kav.lst = new_collection.lst)
                    )

                    # Close the modal.
                    shiny::removeModal()

                    # Requery.
                    collections.query(paste(collections.query(), "-- requery after insert."))

                }
            )


            ## -- Show Query Information -----------------------------------------------------------

            shiny::observeEvent(
                input[["collections.ctrl-sql_info"]]
              , {
                    # Show the modal dialog.
                    # Other that the usual server/ui build up with modules
                    # a modal is dynamic so that its' inputs aren't available
                    # even if namespaced correctly.
                    aceDialog(session$ns, collections.query())
                }
            )

            # React on a click to requery.
            shiny::observeEvent(
                input$requery
              , {
                    collections.query(input$sql)
                    shiny::removeModal()
                }
            )

        }
    )
}
