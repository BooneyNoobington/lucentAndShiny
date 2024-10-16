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



### Fill in some demo data #########################################################################

# Create an Excel workbook to hold the mostly empty tables.
workbook.fileconn <- openxlsx::createWorkbook()

# Loop through each table and add it as a new sheet
for (table in schema.info.df$TABLE_NAME) {
  # Fetch table data
  data <- DBI::dbReadTable(db.conn, table)
  
  # Add sheet and write data
  openxlsx::addWorksheet(workbook.fileconn, table)
  openxlsx::writeData(workbook.fileconn, sheet = table, data)
}

# Save the workbook
openxlsx::saveWorkbook(
    workbook.fileconn
  , file = "./share/all_tables.xlsx"
  , overwrite = TRUE
)



### Insert demo data by YAML #######################################################################


## First tables with no references (foreign keys) ##################################################
yaml::yaml.load_file("../lucentBase/demo_data/demo_data_without_references.yaml") %>%
    purrr::walk2(., names(.), function(records, tab) {
        purrr::walk(records, ~ insertByList(l = .x, tab = tab, user_id = 1))
    })


## Then various other tables with fks ##############################################################
readr::read_file("../lucentBase/demo_data/demo_data_level_1.yaml") %>%
    glue::glue() %>%
        yaml::yaml.load() %>%
            purrr::walk2(., names(.), function(records, tab) {
                purrr::walk(records, ~ insertByList(l = .x, tab = tab, user_id = 1))
            })


