####################################################################################################
### Testing Script #################################################################################
### Test, implement or develop functions ###########################################################
### without a working Shiny UI #####################################################################
####################################################################################################



### Packages #######################################################################################
library(glue)
library(tidyverse)
library(purrr)
library(RMariaDB)
library(digest)
library(rio)
library(readODS)



### Further preparations ###########################################################################
setwd("/home/grindel/Entwicklung/lucentAndShiny")

sys.cnf <- yaml::read_yaml("./conf/las.yaml")  # Load config

source("./db_functions.R")

unix_user.chr <- Sys.getenv("LOGNAME")


## Establish database connection ###################################################################
db.conn <- DBI::dbConnect(
      RMariaDB::MariaDB()
    , dbname = sys.cnf$database$schema
    , unix.sock = sys.cnf$database$socket
    , username = "grindel"
)


## Information about Schema ########################################################################
schema.info.df <- DBI::dbGetQuery(
    conn = db.conn
  , statement = glue::glue_sql(
        "
        SELECT        TABLE_NAME
                    , COLUMN_NAME 
        FROM          information_schema.KEY_COLUMN_USAGE 
        WHERE         TABLE_SCHEMA = 'lucent' 
                  AND CONSTRAINT_NAME = 'PRIMARY' 
        ORDER BY      TABLE_NAME
                    , ORDINAL_POSITION
        ;
        "  
    )
)


## Informatiom about all registered users ##########################################################
users.df <- DBI::dbReadTable(conn = db.conn, name = "person")



### Fill in some demo data #########################################################################
for (table.chr in schema.info.df$TABLE_NAME) {

    tryCatch({
        
        # Write the data frame to the ODS file with the table name as the sheet name
        readODS::write_ods(
            DBI::dbReadTable(db.conn, table.chr)
          , path = glue::glue("./share/demo_data/{table.chr}.ods")
        )
        
        message("Successfully exported table: ", table.chr)
        
    }, error = function(e) {
        message("Error exporting table: ", table.chr, " - ", e$message)
    })
    
}


### Some examples ##################################################################################


## Insert some initial demo data ###################################################################

initial_org <- list(name = "Redhsoft Inc", shorthand = "Redhsoft")

DBI::dbExecute(
    db.conn
  , glue::glue_sql(
        .con = db.conn
      , "
        INSERT INTO organization (name, shorthand)  
        VALUES({initial_org$name}, {initial_org$shorthand});
        "
    )
)


DBI::dbExecute(
    db.conn
  , glue::glue_sql(
        "
        INSERT INTO person (given_name, surname, unix_account, id_organization) 
        VALUES('Booney', 'Noobington', 'grindel', {idByList(initial_org, 'organization')})
        "
    )
)


## Insert a new user ###############################################################################

# This requires that at least one user is already available
insertByList(
    l = list(given_name = "Homer", surname = "Simpson", unix_account = "s7e23")
  , tab = "person"
    # You can grab the current users id dynamically by comparing the unix account to
    # the user table
  , user_id = 23
  , note = "Manual insert for demon purposes."
)


## Insert a new buiseness partner ##################################################################
new_company.list <- list(
    name = "Weyland Yutani"
  , shorthand = "WY"
  , connection = "partner"
)

insertByList(
    l = new_company.list
  , tab = "organization"
  , user_id = 1
)


## Attach the new company to the added user ########################################################
update.list <- list(
    given_name = "John"  # Stays the same
  , surname = "Doe"  # Stays the same
  , id_organization = 2  # To be set
)

updateByList(updates = update.list, tab = "person", id_tab = 12)
