### ------------------------------------------------------------------------------------------------
### --- Populate a table according to configuration ------------------------------------------------
### ------------------------------------------------------------------------------------------------



## -- General Helper Functions ---------------------------------------------------------------------

# Transform a POSIXct-object into a string.
TO_DATE <- function(s) paste0("TO_DATE('", format(s, "%d.%m.%Y %T"), "', 'TT.DD.YYYY HH24:MI:S')")


# Decide wether query.def refers to a file or a string.
InterpretSQLDef <- function(sql.def) {
    if (endsWith(sql.def, tolower(".sql"))) {
        return(readLines(sql.def, collapse = "\n"))
    } else {
        return(sql.def)
    }
}



## -- Obtain data ----------------------------------------------------------------------------------

# Get a data frame object from a select quersy.
SelectToDF <- function(rv.lst, query.path, mariadb.con, mod_config.lst) {
        # Populate the table.
        #rv.lst$log <- paste(rv.lst$log, paste("Loading query from:", query.path), sep = "\n")

        # Form the query.
        # TODO: Implement placeholders.
        query.chr <- paste(readLines(query.path), collapse = "\n")

        # Export the results to a list of reactive values.
        rv.lst$df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
}



InsertByList <- function(table.chr, kav.lst) {

    # Reform the values of the list into strings and numbers.
    # Qoute or don't quote them according to their datatype.
    kav.lst <- lapply(
        kav.lst
      , FUN = function(x) {
            if(typeof(x) %in% c("numeric", "double")) {  # List element is number.
                return(as.character(x))
            } else {  # List element is something else.
                return(paste("'", x, "'", sep = ""))
            }
        }
    )

    # TODO: Sanitize the user input which should be in the values.

    # Insert the table name into the generic statement.
    stmt.chr <- sprintf("INSERT INTO `%s` (keys) VALUES (values)", table.chr)

    # Replace the columns list with the respective values.
    stmt.chr <- stringr::str_replace(
        string = stmt.chr
      , pattern = "keys"
      , replacement = paste("`", names(kav.lst), "`", collapse = ", ", sep = "")
    )

    # Replace the values.
    stmt.chr <- stringr::str_replace(
        string = stmt.chr
      , pattern = "values"
      , replacement = paste(kav.lst, collapse = ", ")
    )

    # TODO: Whe have a functioning statement here. Sanitize here?
    return(stmt.chr)

}
