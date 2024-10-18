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
library(openxlsx)
library(yaml)



### Further preparations ###########################################################################
setwd("/home/lucent/Entwicklung/lucentAndShiny")

sys.cnf <- yaml::read_yaml("./conf/las.yaml")  # Load config

source("./db_functions.R")
source("./helpers.R")

unix_user.chr <- Sys.getenv("LOGNAME")


## Establish database connection ###################################################################
db.conn <- DBI::dbConnect(
      RMariaDB::MariaDB()
    , dbname = sys.cnf$database$schema
    , unix.sock = sys.cnf$database$socket
    , username = sys.cnf$database$user
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



### Insert demo data by YAML #######################################################################


## First tables with no references (foreign keys) ##################################################
yaml::yaml.load_file("../lucentBase/demo_data/demo_data_without_references.yaml") %>%
    purrr::walk2(., names(.), function(records, tab) {
        purrr::walk(
            records,
          ~ insertByList(
                db.conn = db.conn,
                l = .x, tab = tab,
                user_id = 1,
                schema_info = schema.info.df
            )
        )
    })


## Then various other tables with fks ##############################################################

# These files contain placeholders that dereference foreign keys.
# Because of that one cannot loop over the source yaml directly.
# The yaml file has to be reevaluated after every insert so that
# inserted records can be found by the placeholder functions.

readr::read_file("../lucentBase/demo_data/demo_data_level_1.yaml") %>%
    glue::glue() %>%
        yaml::yaml.load() -> new_records.dummy.list

# Iterate over the know known table names ...
for (table.chr in names(new_records.dummy.list)) {
    # ... and the keys representing new records
    for (new_record.key.chr in names(new_records.dummy.list[[table.chr]])) {
        # Re-evaluate source yaml
        readr::read_file("../lucentBase/demo_data/demo_data_level_1.yaml") %>%
            # Compute placeholders, this time with non bogus values
            glue::glue() %>%
                yaml::yaml.load() %>%
                    purrr::pluck(table.chr) %>%
                        purrr::pluck(new_record.key.chr) %>%
                            insertByList(db.conn = db.conn, tab = table.chr, user_id = 1, verbose = TRUE)
                            
    }
}

encodeBase10Sub(516, sys.cnf$enumeration$`base ten`)
