# --------------------------------------------------------------------------------------------------
# --- Viewing and editing data ---------------------------------------------------------------------
# --- Central part of the interface ----------------------------------------------------------------
# --------------------------------------------------------------------------------------------------



#### --- Load required libraries -------------------------------------------------------------------
library(shiny)          # Generating a user interface.
library(DBI)            # Database related functions.
library(RMariaDB)       # Connecting to a datasbase.
library(DT)             # Display tables in a nicer format.
library(yaml)           # Getting the systems configuration.
library(dplyr)
library(dbplyr)
library(pool)
library(readr)
library(stringr)



#### --- Load the configuration. -------------------------------------------------------------------
sys.cnf <- yaml::read_yaml("/home/grindel/Entwicklung/lucentLIMS/lucentAndShiny/conf/las.yaml")

# Table definitions
table.cnf <- yaml::read_yaml(
    "/home/grindel/Entwicklung/lucentLIMS/lucentBASE/table_definitions.yaml"
)



### --- Load helper functions ----------------------------------------------------------------------
source("db_intop.R")


#### --- Contact the RDBMS -------------------------------------------------------------------------
mariadb.con <- pool::dbPool(
    RMariaDB::MariaDB()
  , dbname = sys.cnf$database$schema
  , unix.sock = sys.cnf$database$socket
  , username = sys.cnf$database$user
)



#### --- Define the user interface. ----------------------------------------------------------------
main.ui <- shiny::navbarPage(
    "lucentLIMS"

  , shiny::tabPanel(
        "Plots"
      , shiny::titlePanel("Plots [Placeholder Page]")
      , shiny::mainPanel(shiny::plotOutput("plot1"))
    )

  , shiny::navbarMenu(
            "Modules"
          , shiny::tabPanel(
                "Organisations"
              , shiny::titlePanel(shiny::textOutput("page_title"))
            )
    )

  , shiny::tabPanel(
        "Log"
      , shiny::titlePanel("What Happened During this Session")
      , shiny::mainPanel(shiny::verbatimTextOutput("log"))
  )
)

main.srv <- function(input, output, session){}




#### --- Generate the shiny app --------------------------------------------------------------------
shiny::shinyApp(ui = main.ui, server = main.srv)
