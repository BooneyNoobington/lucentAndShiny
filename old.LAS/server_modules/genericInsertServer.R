### ------------------------------------------------------------------------------------------------
### --- Generic Insert Logic -----------------------------------------------------------------------
### --- Gathers Information About a Table Displays a Modal Dailog and Performs the Insert ----------
### ------------------------------------------------------------------------------------------------



### --- Source Helpers -----------------------------------------------------------------------------
source("./dbInterop.R")         # Get PK, FK and mandatory columns.



### --- Server Logic -------------------------------------------------------------------------------
GenericInsert <- function(id, db.conn, table.chr) {

    # Define namespace.
    ns <- shiny::NS(id)

    # Actual logic.
    shiny::modalServer <- function(input, output, session) {

        # Gather information about the table in question.
        pk.chr      <- GetPK(db.conn, table.chr)
        fk.df       <- GetFK(db.conn, table.chr)
        mandos.v    <- GetMandatories(db.conn, table.chr)

        # Query possible values for the foreign keys.
        for (i in nrow(fk.df)) {
            print(
                pool::dbGetQuery(
                    db.conn
                  , sprintf("SELECT DISTINCT `identifier` FROM `%s`;", fk.df[i, "detail.tbl"])
                )
            )
        }



    }
}
