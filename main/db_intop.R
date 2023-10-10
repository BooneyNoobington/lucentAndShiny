### ------------------------------------------------------------------------------------------------
### --- Populate a table according to configuration ------------------------------------------------
### ------------------------------------------------------------------------------------------------



GetData <- function(rv.lst, query.path, mariadb.con, mod_config.lst) {
        # Populate the table.
        print(paste("Loading query from:", query.path))

        # Form the query.
        # TODO: Implement placeholders.
        query.chr <- paste(readLines(query.path), collapse = "\n")

        # Export the results to a list of reactive values.
        rv.lst$df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
}
