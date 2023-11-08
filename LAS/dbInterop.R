### ------------------------------------------------------------------------------------------------
### --- Database Related Functions -----------------------------------------------------------------
### ------------------------------------------------------------------------------------------------



## -- Insert by List -------------------------------------------------------------------------------
# Insert a new record according to a list. The lists names represent the columns.
CreateStatementByList <- function(table.chr, kav.lst) {

    # Reform the values of the list into strings and numbers.
    # Qoute or don't quote them according to their datatype.
    kav.lst <- lapply(
        kav.lst
      , FUN = function(x) {
            if(typeof(x) %in% c("numeric", "double")) {  # List element is number.
                return(as.character(x))
            } else if (is.null(x)) {  # Don't do anything aginst NULL here. *
                return(NULL)
            } else if (startsWith(x, "Polygon")){  # This is a MariaDB related function.
                return(x)  # Don't enclose in quotation marks.
            } else {  # List element is something else - probably text.
                return(paste("'", x, "'", sep = ""))
            }
        }
    )
    # * If the survive into this function, they should cause an sql error.

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

    print(stmt.chr)

    # Return the statement.
    return(stmt.chr)

}


### --- Gather Information about Tables and Columns ------------------------------------------------


## -- Get Allowed Values (Check Constraint) --------------------------------------------------------
GetAllowedValues <- function(db.conn, table.chr, column.chr) {
    # Check if the column has a unique constraint.
    query.chr <- paste("SELECT COLUMN_NAME, CONSTRAINT_NAME, CONSTRAINT_TYPE,",
                        "CHECK_CLAUSE, R_CHECK_SEARCH_CONDITION",
                        "FROM information_schema.constraint_column_usage",
                        "WHERE TABLE_NAME = ? AND COLUMN_NAME = ?")


    # Obtain a data frame about the contraint on column x. Can be NULL.
    contstraints.df <- pool::dbGetQuery(db.conn, query.chr, params = list(table.chr, column.chr))

    if (nrow(contstraints.df) > 0) {

        # Only return the allowed values for check constraints.
        if (contstraints.df$CONSTRAINT_TYPE[1] == "CHECK") {

            # Return a vector of all the allowed values.
            return(unlist(strsplit(gsub("[()]", "", contstraints.df$CHECK_CLAUSE[1]), ",")))
        }
    }


    return(NULL)  # No constraint found or not a CHECK constraint.
}


## -- Get the Primary Key of a Column --------------------------------------------------------------

# This function finds the primary key column of a table.
GetPK <- function(db.conn, table.chr) {

    result.df <- pool::dbGetQuery(
        db.conn
      , sprintf("SHOW KEYS FROM `%s` WHERE Key_name = 'PRIMARY'", table.chr)
    )

    return(result.df$Column_name[1])
}


## -- Get a List of All Foreign Keys (if any) ------------------------------------------------------

# This function returns a data frame of all foreign keys in table.chr and the tables and columns
# they are pointing to.
GetFK <- function(db.conn, table.chr) {

    # Get the sql creation information of this new table.
    sql_info.lst <- pool::dbGetQuery(db.conn, sprintf("SHOW CREATE TABLE `%s`", table.chr))

    # If this was successful, grab the "Create Table" segment.
    if (nrow(sql_info.lst) > 0) {
        create.sql <- sql_info.lst$`Create Table`[1]

        # Extract the foreign keys.
        fk.v <- regmatches(
            create.sql
          , gregexpr("FOREIGN KEY \\(.*?\\) REFERENCES .*? \\(.*?\\)", create.sql, perl = TRUE)
        )[[1]]

        # Initialize data frame to hold data.
        fk_rel.df <- data.frame(
            master.fk = character(0)
          , detail.tbl = character(0)
          , detail.col = character(0)
        )

        # Loop over all foreign keys.
        for (fk in fk.v) {

            # Extract information about the foreign key
            matches <- regmatches(
                fk
              , regexec("FOREIGN KEY \\((.*?)\\) REFERENCES (.*?) \\((.*?)\\)", fk, perl = TRUE)
            )[[1]]

            # If the format of the found keys is suitable, add them to the df.
            if (length(matches) == 4) {
                master.fk <-  gsub("`", "", matches[2])  # Remove backticks
                detail.tbl <- gsub("`", "", matches[3])
                detail.col <- gsub("`", "", unlist(strsplit(matches[4], ",")))

                new_row <- data.frame(
                    master.fk = master.fk
                  , detail.tbl = detail.tbl
                  , detail.col = paste(detail.col, collapse = ", ")
                )

                fk_rel.df <- rbind(fk_rel.df, new_row)
            }
        }

        return(fk_rel.df)
    } else {
        return(NULL)
    }
}


## -- Get a List of All Mandatory Columns ----------------------------------------------------------

# This function gives a list of all mandatory columns that aren't primary or foreign keys.
GetMandatories <- function(db.conn, table.chr) {

    # Get the column information for a specific table.
    query.chr <- paste(
                    "SELECT              COLUMN_NAME, IS_NULLABLE",
                    "FROM                INFORMATION_SCHEMA.COLUMNS",
                    "WHERE               TABLE_SCHEMA = 'lucent'",
                    "            AND     TABLE_NAME = ?;"
                 )

    result.df <- pool::dbGetQuery(db.conn, query.chr, params = list(table.chr))

    candidates.v <- result.df$COLUMN_NAME[which(result.df$IS_NULLABLE == "NO")]

    # Remove the primary key.
    candidates.v <- candidates.v[which(candidates.v != GetPK(db.conn, table.chr))]

    # Return the mandatory values.
    return(candidates.v)
}



### --- Compute PK (ID) from Name ----------------------------------------------

# Normally all tables in lucentLIMS have a "name" and an "id" field.
# The name is unique just like the id but isn't used for joins e.g. but
# rather as a readable identifier.
GetIDFromName <- function(db.conn, name.chr, table.chr, name.col = "name") {
    
    # Get the primary key field for "table.chr".
    pk.col <- GetPK(db.conn, table.chr)
    
    # Query the id, fitting to "name.chr".
    name.query <- glue::glue_sql(
        "SELECT {`pk`} FROM {`t`} WHERE {`nc`} = {n}"
      , .con = db.conn
      , pk = pk.col, t = table.chr, nc = name.col, n = name.chr
    )
    
    # Grab the results of the query.
    id.df <- pool::dbGetQuery(db.conn, name.query)
    
    
    return(id.df[[pk.col]])
}
