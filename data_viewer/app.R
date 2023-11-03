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
library(pool)           # Manage datbase connections,
library(stringr)        # Advanced string manipulation.
library(dplyr)          # Advanced data frame manipulation.



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
        "Admin Info"
      , shiny::verbatimTextOutput("admin_info.sql_info")
    )

  , footer = shiny::tags$div(
        id = "status_bar"
      , style = "position: fixed; bottom: 0; left: 0; right: 0; background-color: #333; color: #fff; padding: 10px;"
    )
)




#### --- Define the server logic -------------------------------------------------------------------
dw.srv <- function(input, output, session) {

    # Values that are rarely set (usually once per session).
    globals <- reactiveValues(
        statement = NULL
      , status_message = paste("User:", unix_user.chr, "Database connected")
    )

    # The currently used query.
    current_query.chr <- reactiveVal(NULL)

    # The currently used DETAIL query.
    current_detail_query.chr <- reactiveVal(NULL)

    # The currenty executed statement.
    statement.chr <- reactiveVal(NULL)

    # Make the currently selected id or id's globally available.
    sel_id.int_v <- reactiveVal(NULL)

    # Global data frame. Holds whatever the current query retuns.
    df <- reactiveVal(NULL)

    # Another global data frame. Only used for detail data.
    df_detail <- reactiveVal(NULL)


    ## -- Global Reactivity ------------------------------------------------------------------------

    # Observe the users choice of a module.
    shiny::observeEvent(
        input$main_bar
      , {
            tryCatch(
                {
                    # Load the default query for the selected module.
                    current_query.chr(
                        LoadDefaultQuery(sys.cnf$shiny[["query dir"]], shiny::req(input$main_bar))
                    )
                }
              , error = function(e) {shiny::showModal(ReportGeneralError(e))}
            )
        }
    )

    # Observer when the detail query has been changed.
    shiny::observe(
        current_detail_query.chr
      , {
            df_detail(RMariaDB::dbGetQuery(mariadb.con, current_detail_query()))
        }
    )


    ## -- Organisations Module ---------------------------------------------------------------------

    # Render the table.
    output$organisations.table <- DT::renderDT({

        # Get the currently active modules.
        am.chr <- shiny::req(input$main_bar)

        # Transform to data table, which is more interactive.
        table.dt <- DT::datatable(
            # Remove id columns. They usually shouldn't be displayed.
            df()  # <-- Reactive value that gets the output changed whenever changed itself.
          , editable = TRUE  # Users generally can edit the table. TODO: Function that sets this valu accourding to UPDATE and INSERT privileges of current user?
          # Don't use database names for the column headers but prettier names.
          , colnames = unlist(
              sys.cnf$modules[[am.chr]]$table[["friendly names"]][
                  which(sys.cnf$modules[[am.chr]]$table[["friendly names"]] %in% names(df()))
              ]
            )
          , rownames = FALSE  # No running numers.
          # Hide id columns. TODO: Dumb logic that just searches for columns starting with "id_".
          , options = list(
                columnDefs = list(
                    list(targets = names(df())[which(grepl("^id_", names(df())))], visible = FALSE)
                )
            )
        )

    })

    # Show a modal dialog with info about the current query.
    shiny::observeEvent(
        input$organisations.sql_info
      , {shiny::showModal(DisplayQuery("query_info", current_query.chr()))}
    )

    shiny::observeEvent(
        input[["query_info-requery"]]
      , {
            current_query.chr(input[["query_info-ace"]])
            shiny::removeModal()
        }
    )


    ## -- Contacts Module --------------------------------------------------------------------------

    output$contacts.table <- DT::renderDT({

        # Get the currently active modules.
        am.chr <- shiny::req(input$main_bar)

        # Transform to data table, which is more interactive.
        table.dt <- DT::datatable(
            # Remove id columns. They usually shouldn't be displayed.
            df()  # <-- Reactive value that gets the output changed whenever changed itself.
          , editable = TRUE  # Users generally can edit the table. TODO: Function that sets this valu accourding to UPDATE and INSERT privileges of current user?
          # Don't use database names for the column headers but prettier names.
          , colnames = unlist(
              sys.cnf$modules[[am.chr]]$table[["friendly names"]][
                  which(sys.cnf$modules[[am.chr]]$table[["friendly names"]] %in% names(df()))
              ]
            )
          , rownames = FALSE  # No running numers.
          # Hide id columns. TODO: Dumb logic that just searches for columns starting with "id_".
          , options = list(
                columnDefs = list(
                    list(
                      targets = names(df())[
                          which(grepl("^id_", names(df())) | names(df()) == "recycle_bin")
                      ]
                    , visible = FALSE)
                )
            )
          # Allow only single selections.
          , selection = list(mode = "single", target = "row"))


    })

    # Observe the selection.
    shiny::observeEvent(
        input$contacts.table_rows_selected  # Fire everytime a new line is selected.
      , {
            selected_id.int <- df()[input$contacts.table_rows_selected, "id_person"]
            print(selected_id.int)
            current_detail_query.chr(
                stringr::str_replace(
                    string =readLines(paste0(sys.cnf$shiny[["query dir"]], "/modules/Contacts/GET_ADDRESSES.SQL"))
                  , pattern = "selected_id"
                  , replacement = as.character(selected_id.int)
                )
            )
        }
    )


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
            # Requery.
            current_query.chr(paste(current_query.chr(), "-- requery after insert", sep = "\n"))
        }

    )




    # Keep track of Changes in the Database.
    shiny::observeEvent(
        input$organisations.table_cell_edit
      , {
            edits.lst <- input$organisations.table_cell_edit
            pk_col.chr <- "id_organisation"
            changed_col.chr <- names(df())[edits.lst$col + 1]
            changed_row.pk.num <- df()[[pk_col.chr]][edits.lst$row]

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



    # React when the current query is changed.
    shiny::observeEvent(
        current_query.chr()
      , {
            tryCatch(
                {df(RMariaDB::dbGetQuery(mariadb.con, current_query.chr()))}
              , error = function(e) {WarnAboutBadSQL(current_query.chr(), e)}
            )
            output$admin_info.sql_info <-  shiny::renderPrint(current_query.chr())
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
