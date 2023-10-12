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
ua.v <- system("awk -F: '$3 >= 1000 {print $1}' /etc/passwd", intern = TRUE)
ua.v <- ua.v[which(!ua.v == "nobody")]  # The "nobody" user should not be available.



### --- General Feedback ---------------------------------------------------------------------------
message("Current working directory is: \n", getwd())
unix_user.chr <- Sys.getenv("LOGNAME")
message("lucentLIMS is running for user: ", unix_user.chr)



### --- Modules and Helpers ------------------------------------------------------------------------
source("db_intop.R")
source("navbar_menu.R")
source("module_selection.R")
source("dialogs.R")
source("server_modules.R")



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
  , module_selection.ui()

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
    globals <- reactiveValues(
        df = NULL
      , query = NULL
      , statement = NULL
      , active_module = NULL
      , log = NULL
      , status_message = paste("User:", unix_user.chr, "Database connected")
    )

    df <- reactiveVal(NULL)


    # Observe the users choice of a module.
    shiny::observe({

            if (shiny::req(input$main_bar) == "Organisations") {

                globals$active_module <- "organisations"

                globals$log <- paste0("Current tab is: ", globals$active_module)

                # Get the data from the database.
                query.path <- paste0(
                    sys.cnf$shiny[["query dir"]], "/modules/", globals$active_module
                  , "/"
                  , sys.cnf$modules[[globals$active_module]]$sql
                )

                query.chr <- paste(readLines(query.path), collapse = "\n")

                globals$df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
                globals$log <- paste0("Getting query from: ", query.path)

                shiny::updateTextInput(
                    session, "organisations.sql_info", value = paste(readLines(query.path), collapse = "\n")
                )

                # Take not of the original column names.
                globals$names <- names(globals$dt)
            }

            if (shiny::req(input$main_bar) == "Contacts") {

                globals$active_module <- "contacts"

                globals$log <- paste0("Current tab is: ", globals$active_module)

                # Get the data from the database.
                query.path <- paste0(
                    sys.cnf$shiny[["query dir"]], "/modules/", globals$active_module
                  , "/"
                  , sys.cnf$modules[[globals$active_module]]$sql
                )

                query.chr <- paste(readLines(query.path), collapse = "\n")

                globals$df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
                globals$log <- paste0("Getting query from: ", query.path)
            }
    })


    output$organisations.table <- DT::renderDT({
        # Transform to data table, which is more interactive.
        table.dt <- DT::datatable(
            globals$df
          , editable = TRUE
          , colnames = unlist(
              sys.cnf$modules[[globals$active_module]]$table[["friendly names"]][
                  which(
                      sys.cnf$modules[[globals$active_module]]$table[["friendly names"]] %in% names(globals$df)
                  )
              ]
            )
          , rownames = FALSE
        )
    })

    output$contacts.table <- DT::renderDT({
        # Transform to data table, which is more interactive.
        table.dt <- DT::datatable(
            globals$df
          , editable = TRUE
          , colnames = unlist(
              sys.cnf$modules[[globals$active_module]]$table[["friendly names"]][
                  which(
                      sys.cnf$modules[[globals$active_module]]$table[["friendly names"]] %in% names(globals$df)
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

    # Add an organisation.
    shiny::observeEvent(
        input$organisations.add_record
      , {
            shiny::showModal(
              organisations.AddOrga("register_orga", c("contractor", "partner", "client"))
            )
        }
    )

    shiny::observeEvent(
        input[["register_orga-add_record"]]
      , {
            globals$statement <- registerOrga("register_orga")
            shiny::removeModal()
        }

    )




    # Keep track of Changes in the Database.
    shiny::observeEvent(
        input$organisations.table_cell_edit
      , {
            edits.lst <- input$organisations.table_cell_edit
            pk_col.chr <- "id_organisation"
            changed_col.chr <- names(globals$df)[edits.lst$col + 1]
            changed_row.pk.num <- globals$df[[pk_col.chr]][edits.lst$row]

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
            globals$statement <- update.statement
        }
    )

    output$log <- shiny::renderPrint({print(paste(input$log, globals$log, sep = "\n"))})


    # Contacts: Add record.
    shiny::observeEvent(
        input$contacts.add_record
      , { shiny::showModal(contacts.AddContact(ua.v)) }
    )


    shiny::observeEvent(
        input$contacts.add_record.add
      , {

            if(input$contacts.add_record.unix_account == "n.a.") {
                globals$statement <- sprintf(
                    "INSERT INTO `person` (`given_name`, `surname`) VALUES('%s', '%s')"
                  , input$contacts.add_record.given_name
                  , input$contacts.add_record.surname
                )
            } else {
                globals$statement <- sprintf(
                    "INSERT INTO `person` (`given_name`, `surname`, `unix_account`) VALUES('%s', '%s', '%s')"
                  , input$contacts.add_record.given_name
                  , input$contacts.add_record.surname
                  , input$contacts.add_record.unix_account
                )
            }

            shiny::removeModal()
        }
    )

    shiny::observeEvent(
        input$contacts.del_record
      , {
            sel.v <- input$contacts.table_rows_selected
            print(
                globals$df[sel.v, "id_person"]
            )
        }
    )


    shiny::observeEvent(
        globals$statement
      , {
          log_line.v <- data.frame(
              date = format(Sys.time(), format = "%Y.%m.%d %T")
            , user = unix_user.chr
            , event = "The following statement was executed"
            , description = paste(globals$statement, collapse = "\n")
          )

          write.table(
              log_line.v
            , file = "/home/grindel/Entwicklung/lucentLIMS/lucentAndShiny/log/lucentLIMS.log"
            , sep = " · "
            , append = TRUE
            , quote = FALSE
            , row.names = FALSE
            , col.names = FALSE
          )

          tryCatch(
              {RMariaDB::dbExecute(mariadb.con, globals$statement)}
            , error = function(e) {
                  message("SQL error: ", e)
                  globals$status_message <- e$message
                  shiny::showModal(WarnAboutBadSQL(globals))
              }
            , warning = function(w) {
                  message("SQL warning: ", w$message)
                  globals$status_message <- w$message
              }
          )

        }
    )


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
