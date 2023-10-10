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

# Get a list of all unix users.
ua.lst <- system("awk -F: '$3 >= 1000 {print $1}' /etc/passwd", intern = TRUE)
ua.lst <- ua.lst[which(!ua.lst == "nobody")]  # The "nobody" user should not be available.



### --- General Feedback ---------------------------------------------------------------------------
message("Current working directory is: \n", getwd())
unix_user.chr <- Sys.getenv("LOGNAME")
message("lucentLIMS is running for user: ", unix_user.chr)



### --- Load helper functions ----------------------------------------------------------------------
source("db_intop.R")



#### --- Contact the RDBMS -------------------------------------------------------------------------
mariadb.con <- pool::dbPool(
    RMariaDB::MariaDB()
  , dbname = sys.cnf$database$schema
  , unix.sock = sys.cnf$database$socket
  , username = unix_user.chr
)



#### --- Define the user interface. ----------------------------------------------------------------
dw.ui <- shiny::navbarPage(
    "lucentLIMS"
  , id = "main_bar"
  , shiny::navbarMenu(
        "Modules"

      , shiny::tabPanel(
            "Samples"
          , shiny::titlePanel("Sample")
          , shiny::mainPanel(DT::DTOutput("samples.table"))
          , shiny::sidebarPanel(
                shiny::actionButton("samples.add_record", "Add Sample", icon = icon("plus"))
            )
          , icon = shiny::icon("vials")
        )

      , shiny::tabPanel(
            "Organisations"
          , shiny::mainPanel(DT::DTOutput("organisations.table"))
          , shiny::sidebarPanel(
                shiny::h4("General")
              , shiny::actionButton("organisations.add_record", "Add Record", icon = icon("plus"))
              , shiny::actionButton("organisations.del_record", "Delete Selected", icon = icon("minus"))
              , shiny::h4("SQL-Information")
              , shiny::tags$style(
                  shiny::HTML(
                  "
                      .monospace-textarea {
                          font-family: 'Hack', monospace;
                      }
                  ")
                )
              , shiny::div(
                    class = "monospace-textarea"
                  , shiny::textAreaInput("organisations.sql_info", NULL, rows = 7)
                )
            )
          , icon = shiny::icon("building")
        )

      , shiny::tabPanel(
            "Contacts"
          , shiny::mainPanel(DT::DTOutput("contacts.table"))
          , shiny::sidebarPanel(
                shiny::h4("General")
              , shiny::actionButton("contacts.add_record", "Add Record", icon = icon("plus"))
              , shiny::actionButton("contacts.del_record", "Delete Selected", icon = icon("minus"))
            )
          , icon = shiny::icon("address-book")
        )

    )

  , shiny::tabPanel(
        "Plots"
      , shiny::titlePanel("Plots [Placeholder Page]")
      , shiny::mainPanel(shiny::plotOutput("plot1"))
    )

  , shiny::tabPanel(
        "Log"
      , shiny::titlePanel("What Happened During this Session")
      , shiny::mainPanel(shiny::verbatimTextOutput("log"))
  )

  , footer = shiny::tags$div(
        id = "status_bar"
      , style = "position: fixed; bottom: 0; left: 0; right: 0; background-color: #333; color: #fff; padding: 10px;"
    )
)




#### --- Define the server logic -------------------------------------------------------------------
dw.srv <- function(input, output, session) {

    # The central element of the data view is this table.
    reactive.values <- reactiveValues(
        df = NULL
      , query = NULL
      , statement = NULL
      , active_module = NULL
      , log = NULL
      , status_message = paste("User:", unix_user.chr, "Database connected")
    )


    # Observe the users choice of a module.
    shiny::observe({

            if (shiny::req(input$main_bar) == "Organisations") {

                reactive.values$active_module <- "organisations"

                reactive.values$log <- paste0("Current tab is: ", reactive.values$active_module)

                # Get the data from the database.
                query.path <- paste0(
                    sys.cnf$shiny[["query dir"]], "/modules/", reactive.values$active_module
                  , "/"
                  , sys.cnf$modules[[reactive.values$active_module]]$sql
                )

                query.chr <- paste(readLines(query.path), collapse = "\n")

                reactive.values$df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
                reactive.values$log <- paste0("Getting query from: ", query.path)

                shiny::updateTextInput(
                    session, "organisations.sql_info", value = paste(readLines(query.path), collapse = "\n")
                )

                # Take not of the original column names.
                reactive.values$names <- names(reactive.values$dt)
            }

            if (shiny::req(input$main_bar) == "Contacts") {

                reactive.values$active_module <- "contacts"

                reactive.values$log <- paste0("Current tab is: ", reactive.values$active_module)

                # Get the data from the database.
                query.path <- paste0(
                    sys.cnf$shiny[["query dir"]], "/modules/", reactive.values$active_module
                  , "/"
                  , sys.cnf$modules[[reactive.values$active_module]]$sql
                )

                query.chr <- paste(readLines(query.path), collapse = "\n")

                reactive.values$df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
                reactive.values$log <- paste0("Getting query from: ", query.path)
            }
    })


    output$organisations.table <- DT::renderDT({
        # Transform to data table, which is more interactive.
        table.dt <- DT::datatable(
            reactive.values$df
          , editable = TRUE
          , colnames = unlist(
              sys.cnf$modules[[reactive.values$active_module]]$table[["friendly names"]][
                  which(
                      sys.cnf$modules[[reactive.values$active_module]]$table[["friendly names"]] %in% names(reactive.values$df)
                  )
              ]
            )
          , rownames = FALSE
        )
    })

    output$contacts.table <- DT::renderDT({
        # Transform to data table, which is more interactive.
        table.dt <- DT::datatable(
            reactive.values$df
          , editable = TRUE
          , colnames = unlist(
              sys.cnf$modules[[reactive.values$active_module]]$table[["friendly names"]][
                  which(
                      sys.cnf$modules[[reactive.values$active_module]]$table[["friendly names"]] %in% names(reactive.values$df)
                  )
              ]
            )
          , rownames = FALSE
        )
    })


    # Example code: Plotting:
    output$plot1 <- renderPlot({
        plot.df <- data.frame("X Achse" = 1:10, "Y Achse" = sqrt(1:10))
        plot(plot.df)
    })

    # Keep track of Changes in the Database.
    shiny::observeEvent(
        input$organisations.table_cell_edit
      , {
            edits.lst <- input$organisations.table_cell_edit
            pk_col.chr <- "id_organisation"
            changed_col.chr <- names(reactive.values$df)[edits.lst$col + 1]
            changed_row.pk.num <- reactive.values$df[[pk_col.chr]][edits.lst$row]

            print(paste0("Changed table ...............", "organisation"))
            print(paste0("Name of changed column: .... ", changed_col.chr))
            print(paste0("Changed row no ............. ", edits.lst$row))
            print(paste0("Primary Key: ............... ", pk_col.chr, " = ", changed_row.pk.num))

            update.statement <- sprintf(
                "UPDATE `organisation` SET `%s` = '%s' WHERE `%s` = %i;"
              , changed_col.chr, edits.lst$value, pk_col.chr, changed_row.pk.num
            )


            shiny::updateTextInput(
                session, "organisations.sql_info", value = update.statement
            )

            #RMariaDB::dbExecute(mariadb.con, update.statement)
            reactive.values$statement <- update.statement
        }
    )

    output$log <- shiny::renderPrint({print(paste(input$log, reactive.values$log, sep = "\n"))})


    # Contacts: Add record.
    shiny::observeEvent(
        input$contacts.add_record
      , {
            shiny::showModal(source("./snippets/contacts/add_record.R", local = TRUE))
        }
    )

    shiny::observeEvent(
        input$contacts.add_record.add
      , {
            reactive.values$statement <- sprintf(
                "INSERT INTO `person` (`given_name`, `surname`, `unix_account`) VALUES('%s', '%s', '%s')"
              , input$contacts.add_record.given_name
              , input$contacts.add_record.surname
              , input$contacts.add_record.unix_account
            )

            shiny::removeModal()
        }
    )

    shiny::observeEvent(
        reactive.values$statement
      , {
          log_line.chr <- paste(
              format(Sys.time(), format = "%Y.%m.%d %T")
            , unix_user.chr
            , "The following statement was executed"
            , paste(reactive.values$statement, collapse = "\n")
            , sep = " · "
          )

          log.lst <-

          fileConn <- file("/home/grindel/Entwicklung/lucentLIMS/lucentAndShiny/log/lucentLIMS.log")
          writeLines(log_line.chr, fileConn)
          close(fileConn)

          tryCatch(
              {RMariaDB::dbExecute(mariadb.con, reactive.values$statement)}
            , error = function(e) {
                  message("SQL error: ", e)
                  reactive.values$status_message <- e$message
                  shiny::showModal(source("./snippets/sql_warning.R", local = TRUE))
              }
            , warning = function(w) {
                  message("SQL warning: ", w$message)
                  reactive.values$status_message <- w$message
              }
          )

        }
    )


    # Update the footer.
    output$status_bar <- renderText({
        reactive.values$status_message
    })

    # Close the pool when quitting the server.
    session$onSessionEnded(function() {
        observe({
            pool::poolClose(mariadb.pool)
        })
    })

}



#### --- Generate the shiny app --------------------------------------------------------------------
shiny::shinyApp(
    ui = dw.ui
  , server = dw.srv
  , options = list(
        host = "0.0.0.0"  # Listen on all network interfaces
      , port = 4001       # Specify the port you want to use (e.g., 3838)
    )
)
