### ------------------------------------------------------------------------------------------------
### --- Server Module for Interacting With Collections ---------------------------------------------
### ------------------------------------------------------------------------------------------------


## -- Import Functionality -------------------------------------------------------------------------
library(readr)                                  # Read SQL files
library(leaflet)                                # Use leaflet to display maps.
library(leaflet.extras)                         # Address search bar.
library(DBI)                                    # Handle interaction with database.
library(shinyWidgets)                           # Nicer UI-Elements.
source("helpers.R")                             # Various helper functions.
source("./dbInterop.R")                         # Handle queries and statements.
source("./modules/newCollectionDialog.R")       # Dynamic UI elements.
source("./modules/aceDialog.R")                 # Submodule for SQL info.



# Module server function
collectionsServer <- function(id) {
    shiny::moduleServer(
        id
      , function(input, output, session) {


            ## -- The master table -----------------------------------------------------------------
            collections.query <- shiny::reactiveVal(readr::read_file("./SQL/QUERY_COLLECTIONS.SQL"))

            # Start with an empty result set.
            collections.df <- shiny::reactiveVal(NULL)

            # Every time the query changes, execute the new query into a data frame.
            shiny::observeEvent(
                collections.query()
              , {collections.df(DBI::dbGetQuery(db.conn, collections.query()))}
            )

            # Display the results in a table element.
            output$collections.tbl <- DT::renderDT(
                collections.df()
              , rownames = FALSE
              , colnames = unlist(coltrans.cnf$collections$collections_table)
            )


            ## -- Other Reactives ------------------------------------------------------------------
            object.stmt <- shiny::reactiveVal(NULL)


            ## -- Handle Adding a New Collection ---------------------------------------------------

            # Display the UI.
            shiny::observeEvent(
                input[["collections.ctrl-add_record"]]
              , {

                    # Compute the next name for a collection
                    next.name.v <- GetNextIdentifier(
                          recent_ident.chr = (
                              DBI::dbGetQuery(
                                  conn = db.conn
                                , "
                                  SELECT `name` FROM `collection`
                                  WHERE `id_collection` = (SELECT MAX(`id_collection`) FROM `collection`)
                                  "
                              ) %>% strsplit(split = "·")
                          )[[1]]
                        , numbering.lst = sys.cnf$enumeration
                    )

                    # Display a Modal Dialog that asks for the respective information.
                    shiny::showModal(
                        newCollectionDialog(
                              session$ns
                            , paste(next.name.v, collapse = "·")
                            , DBI::dbGetQuery(db.conn, "SELECT `name` FROM `matrix`") %>% dplyr::pull(name)
                            , DBI::dbGetQuery(db.conn, "SELECT `name` FROM `collection_class`") %>% dplyr::pull(name)
                            , DBI::dbGetQuery(db.conn, "SELECT `name` FROM `spot`") %>% dplyr::pull(name)
                            , DBI::dbGetQuery(db.conn, "SELECT `name` FROM `site`") %>% dplyr::pull(name)
                        )
                    )
                }
            )


            ## -- React to user input --------------------------------------------------------------
            # Add a new collection.
            shiny::observeEvent(
                input$add_collection
              , {

                    tryCatch(
                        {
 
                            print(
                                glue::glue(
                                    "Inserting new collection with class {input$collection_class.name.pick} and matrix {input$matrix.name.pick}."
                                )
                            )
        
                            insert.stmt <- glue::glue_sql(
                                  .con = db.conn
                                , "
                                    INSERT INTO `collection` (`name`, `id_matrix`, `id_collection_class`, `id_spot`, `id_site`)
                                    VALUES(
                                        {input$collection.name}
                                      , {GetIDFromName(db.conn, input$matrix.name.pick, 'matrix')}
                                      , {GetIDFromName(db.conn, input$collection_class.name.pick, 'collection_class')}
                                      , {GetIDFromName(db.conn, input$spot.name.pick, 'spot')}
                                      , {GetIDFromName(db.conn, input$site.name.pick, 'site')}
                                    )
                                    "
                            )

                            # Insert the new collection.
                            DBI::dbExecute(
                                conn = db.conn
                              , statement = insert.stmt
                            )

        
                            # Close the modal.
                            shiny::removeModal()
        
                            # Requery.
                            glue::glue("{collections.query()} --requery after insert") %>%
                                collections.query()
                        }
                      , error = function(e) shinyWidgets::sendSweetAlert(
                            session = session
                          , title = "Problem trying to add new collection"
                          , type = "error"
                          , text = glue::glue("{e}; SQL was {insert.stmt}")
                        )
                    )

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
