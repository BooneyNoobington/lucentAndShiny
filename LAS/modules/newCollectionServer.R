### ------------------------------------------------------------------------------------------------
### --- Show a Dialog for Creating a New Collection ------------------------------------------------
### ------------------------------------------------------------------------------------------------


## -- Sources --------------------------------------------------------------------------------------
source("./modules/newCollectionDialog.R")   # Specifications for the dialogs.
source("./helpers.R")                       # Various helper functions.
source("./dbInterop.R")                     # Database related functions.


## -- Logic ----------------------------------------------------------------------------------------
newCollectionServer <- function(id, db.conn, sys.cnf) {

    # Wrap logic in a moduleServer function.
    shiny::moduleServer(
        id  # Namespacing.
        # Actual logic.
      , function (input, output, session) {


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
            col_classes.v <- pool::dbGetQuery(
                db.conn, "SELECT `name` FROM `collection_class`"
            )$name
            spots.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `spot`")$name
            sites.v <- pool::dbGetQuery(db.conn, "SELECT `name` FROM `site`")$name

            # Display a Modal Dialog that asks for the respective information.
            shiny::showModal(
                newCollectionDialog(
                      paste(next.name.v, collapse = "·")
                    , objects.v
                    , matrices.v
                    , col_classes.v
                    , spots.v
                    , sites.v
                )
            )


            ## -- React to user input --------------------------------------------------------------
            # Add a new collection.
            shiny::observeEvent(
                input$add_collection
              , {

                    # Decide wether or not to insert a new matrix.
                    if(! input$matrix.name.set == "") {
                        CreateStatementByList(
                            table = "matrix"
                          , kav.lst = list(name = input$matrix.name.set)
                        )
                    }

                    # Close the modal.
                    shiny::removeModal()

                }
            )

        }
    )
}
