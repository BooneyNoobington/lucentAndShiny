### ------------------------------------------------------------------------------------------------
### --- Database Related Functions -----------------------------------------------------------------
### ------------------------------------------------------------------------------------------------


## -- Log change -----------------------------------------------------------------------------------
logChange <- function(tab, id_record, change, user_id, verbose = FALSE, ...) {
    
    additional_args = list(...)
    
    # Identify the column name for the primary key of the changed table
    schema.info.df %>%
        dplyr::filter(TABLE_NAME == tab) %>%
            dplyr::pull(COLUMN_NAME) -> pk
    
    # Calculate the checksum of the affected row after the change (update, insert or otherwise)
    DBI::dbGetQuery(
          db.conn
        , glue::glue_sql(.con = db.conn, "SELECT * FROM {`tab`} WHERE {`pk`} = {id_record}")
    ) %>%
        paste(collapse = "") %>%
            digest::digest(algo = "sha256") -> cs
    
    # Form the log statement
    if(!exists("logtab")) logtab <- glue::glue("l_{tab}")  # Assume generic logtable name
    
    log_stmt <- glue::glue_sql(
        .con = db.conn
      , "
        INSERT INTO {`logtab`} ({`pk`}, `change`, `t`, `id_editor`, `note`, `checksum`)
        VALUES({id_record}, {change}, {as.character(Sys.time())}, {user_id}, {additional_args$note}, {cs});
        "
    )
    
    if (verbose) print(log_stmt)
    
    # Log the insert
    tryCatch(
        {
            DBI::dbExecute(conn = db.conn, statement = log_stmt)
        }
      , error = function(e) print(
            glue::glue(
                "
                CIRITCAL: Could not perform logtable entry for table {tab} and inserted id {id_record}
                {e}
                "
            )  
        )
      , warning = function(e) print(
            glue::glue(
                "
                WARNING: Logging insert for table {tab} and id {id_record} did not go smoothly
                {w}
                "
            )
        )
    )    
}


## -- Get ID by list -------------------------------------------------------------------------------
idByList <- function(l, tab) {
    
    # Define the WHERE parameters
    glue::glue_sql_collapse(
        lapply(
              names(l)
            , function(col) {
                glue::glue_sql("{`col`} = {l[[col]]}", .con = db.conn)
            }
        )
      , sep = " AND "
    ) -> where.stmt
    
    # Identify the column name for the primary key of the changed table
    schema.info.df %>%
        dplyr::filter(TABLE_NAME == tab) %>%
            dplyr::pull(COLUMN_NAME) -> pk

    
    DBI::dbGetQuery(
        conn = db.conn
      , statement = glue::glue_sql(
            .con = db.conn
          , "SELECT {`pk`} FROM {`tab`} WHERE {where.stmt}"
        )
    ) %>% dplyr::pull(pk)

}



## -- Insert by List -------------------------------------------------------------------------------
# Packages:
# - tidyverse
# - DBI
# - glue

# Expects:
# - db.conn
# - schema.info.df

# Returns:
# - id of inserted row
insertByList <- function(l, tab, user_id, verbose = FALSE, ...)
{
    # Prepare statement
    stmt <- glue::glue_sql(
        .con = db.conn
      , "INSERT INTO {`tab`} ({`names(l)`*}) VALUES({unlist(l)*});"
    )
    
    if (verbose) print(stmt)
    
    # Try insert
    tryCatch(
        DBI::dbExecute(db.conn, stmt)
      , error = function(e) {
            print(
                glue::glue(
                    "
                    CRITICAL: Error while trying to write data to database.
                    {e}
                    "
                )
            )
            return(-1)  # Return nonsensical value
        }
        
      , warning = function(w) print(
            glue::glue(
                "
                Inserting data did no go smoothly. This warning was produced:
                {w}
                "
            )  
        )
    )
    
    
    # Grab id of newly inserted record
    DBI::dbGetQuery(db.conn, "SELECT LAST_INSERT_ID() AS id") %>% dplyr::pull(id) -> latest_id
    
    
    # Log the insert
    logChange(tab, latest_id, "insert", user_id, ...)

    
    return(latest_id)
}



## -- Update by list -------------------------------------------------------------------------------
# Expectations as above

updateByList <- function(
      updates
    , id_tab
    , tab
    , user_id
    , logtab = glue::glue("l_{tab}")
    , note = "Routine update."
    , verbose = FALSE
)
{

    # Identify the column name for the primary key
    schema.info.df %>%
        dplyr::filter(TABLE_NAME == tab) %>%
            dplyr::pull(COLUMN_NAME) -> pk
    
    # Define the set parameters
    glue::glue_sql_collapse(
        lapply(
              names(updates)
            , function(col) {
                  glue::glue_sql("{`col`} = {updates[[col]]}", .con = db.conn)
              }
        )
      , sep = ","
    ) -> set.stmt
    
    # Prepare the update statement
    glue::glue_sql(
        .con = db.conn
      , "
        UPDATE {`tab`}
        SET {set.stmt}
        WHERE {`pk`} = {id_tab}
        "
    ) -> update.stmt
    
    
    # Execute update
    tryCatch(
        DBI::dbExecute(db.conn, update.stmt)
      , error = function(e) print(
            glue::glue(
                "
                CRITICAL: Update on table {tab} for record {id_tab} could not be performed.
                {e}
                "
            )  
        )
    )
    
    
    # Log the insert
    # Calculate the checksum  of the whole inserted row
    DBI::dbGetQuery(
        db.conn
        , glue::glue_sql(.con = db.conn, "SELECT * FROM {`tab`} WHERE {`pk`} = {id_tab}")
    ) %>%
        paste(collapse = "") %>%
            digest::digest(algo = "sha256") -> cs
    
    log_stmt <- glue::glue_sql(
        .con = db.conn
      , "
        INSERT INTO {`logtab`} ({`pk`}, `change`, `t`, `id_editor`, `note`, `checksum`)
        VALUES({id_tab}, 'update', {as.character(Sys.time())}, {user_id}, {note}, {cs});
        "
    )
    
    tryCatch(
          {
              DBI::dbExecute(conn = db.conn, statement = log_stmt)
          }
        , error = function(e) print(
              glue::glue(
                  "
                  CIRITCAL: Could not perform logtable entry for table {tab} and updated id {id_tab}
                  {e}
                  "
              )  
          )
        , warning = function(e) print(
            glue::glue(
                "
                WARNING: Logging insert for table {tab} and id {id_tab} did not go smoothly
                {w}
                "
            )
          )
    )    
}
