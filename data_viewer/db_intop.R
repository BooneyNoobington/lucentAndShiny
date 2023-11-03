### ------------------------------------------------------------------------------------------------
### --- Populate a table according to configuration ------------------------------------------------
### ------------------------------------------------------------------------------------------------



## -- Config Related Functions ---------------------------------------------------------------------
LoadDefaultQuery <- function(query.dir, module.chr) {
    message("Loading query from: ", paste0(query.dir, "/modules/", module.chr, "/DEFAULT.SQL"))
    query.chr <- paste(
        readLines(paste0(query.dir, "/modules/", module.chr, "/DEFAULT.SQL"))
      , collapse = "\n"
    )
    message(query.chr)
    return(query.chr)
}



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

# Get a data frame object from a select query.
SelectByList <- function(table.chr, kav.lst) {
    query.chr <- sprintf("SELECT * FROM `%s` WHERE where_clause", table.chr)
}



# Insert a new record according to a list. The lists names represent the columns.
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
