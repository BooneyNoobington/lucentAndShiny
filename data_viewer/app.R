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



#### --- Load the configuration. -------------------------------------------------------------------
sys.cnf <- yaml::read_yaml("/home/grindel/Entwicklung/lucentLIMS/lucentAndShiny/conf/las.yaml")

# Table definitions
table.cnf <- yaml::read_yaml(
    "/home/grindel/Entwicklung/lucentLIMS/lucentBASE/table_definitions.yaml"
)



#### --- Contact the RDBMS -------------------------------------------------------------------------
mariadb.con <- pool::dbPool(
    RMariaDB::MariaDB()
  , dbname = sys.cnf$database$schema
  , unix.sock = sys.cnf$database$socket
  , username = sys.cnf$database$user
)



#### --- Define the user interface. ----------------------------------------------------------------
dw.ui <- shiny::navbarPage(
    "lucentLIMS"
  , shiny::tabPanel(
        "Tables"
      , shiny::titlePanel(shiny::textOutput("page_title"))
      , shiny::mainPanel(DT::DTOutput("table"))
      , shiny::sidebarPanel(
            shiny::selectInput("module_select", "Modules", names(sys.cnf$modules))
        )
    )

  , shiny::tabPanel(
        "Plots"
      , shiny::titlePanel("Plots [Placeholder Page]")
      , shiny::mainPanel(shiny::plotOutput("plot1"))
      , shiny::conditionalPanel(
            checkboxGroupInput(
                "show_vars"
              , "Plot Settings:"
              , c("mean", "sd", "covar")
              , selected = "mean"
            )
        ),
    )

  , shiny::tabPanel(
        "Log"
      , shiny::titlePanel("What Happened During this Session")
      , shiny::mainPanel(shiny::verbatimTextOutput("log"))
  )
)




#### --- Define the server logic -------------------------------------------------------------------
dw.srv <- function(input, output, session) {

    # The central element of the data view is this table.
    reactive.values <- reactiveValues(data = NULL, names = NULL, df = NULL)

    # Observe the users choice of a module.
    shiny::observeEvent(
        input$module_select
      , {
            # Change the title according to what the configuration says.
            output$page_title <- renderText({sys.cnf$modules[[input$module_select]]$title})
            print(paste("Changing title panel to:", sys.cnf$modules[[input$module_select]]$title))
            # Populate the table.
            query.path <- paste0(
                sys.cnf$shiny["query dir"], "/modules/"
              , input$module_select, "/"
              , sys.cnf$modules[[input$module_select]]$sql
            )
            print(paste("Loading query from:", query.path))

            # Form the query.
            query.chr <- paste(readLines(query.path), collapse = "\n")

            # Get a data frame from this query.
            table.df <- RMariaDB::dbGetQuery(mariadb.con, query.chr)
            reactive.values$df <- table.df

            # Take not of the original column names.
            reactive.values$names <- names(table.df)

            # Transform to data table, which is more interactive.
            table.dt <- DT::datatable(
                table.df
              , editable = TRUE
              , colnames = unlist(
                  sys.cnf$modules[[input$module_select]]$table[["friendly names"]][
                    which(
                      sys.cnf$modules[[input$module_select]]$table[["friendly names"]] %in% names(table.df)
                    )
                  ]
                )
              , rownames = FALSE
            )

            reactive.values$data <- table.dt

        }
    )

    output$table <- DT::renderDT({
        reactive.values$data
    })


    # Example code: Plotting:
    output$plot1 <- renderPlot({
        plot.df <- data.frame("X Achse" = 1:10, "Y Achse" = sqrt(1:10))
        plot(plot.df)
    })

    # Keep track of Changes in the Database.
    shiny::observeEvent(
        input$table_cell_edit
      , {
            edits.lst <- input$table_cell_edit

            pk_query.raw.chr <- paste(
                readLines(paste0(sys.cnf$shiny["query dir"], "/", "GET_PK.SQL")), collapse = "\n"
            )

            table_name.chr <- sys.cnf$modules[[input$module_select]]$table$name

            column_info.lst <- table.cnf$tables[[table_name.chr]]$columns

            pk_query.chr <- sprintf(pk_query.raw.chr, table_name.chr)

            pk_col.chr <- RMariaDB::dbGetQuery(mariadb.con, pk_query.chr)$COLUMN_NAME

            changed_col.chr <- reactive.values$names[edits.lst$col + 1]

            changed_row.pk.num <- reactive.values$df[[pk_col.chr]][edits.lst$row]

            print(paste0("Changed table ...............", table_name.chr))
            print(paste0("Changed column no .......... ", edits.lst$col + 1))
            print(paste0("Name of changed column: .... ", changed_col.chr))
            print(paste0("Changed row no ............. ", edits.lst$row))
            print(paste0("Primary Key: ............... ", pk_col.chr, " = ", changed_row.pk.num))

            update.statement <- sprintf(
                "UPDATE `%s` SET `%s` = '%s' WHERE `%s` = %i;"
              , table_name.chr, changed_col.chr, edits.lst$value, pk_col.chr, changed_row.pk.num
            )

            print(update.statement)

            if (!"no editing" %in% column_info.lst[[changed_col.chr]]$attributes) {
                RMariaDB::dbExecute(mariadb.con, update.statement)
            } else {
                output$log <- shiny::renderPrint({
                    sprintf("Editing for column'%s' is not allowed.", changed_col.chr)
                })
            }

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
shiny::shinyApp(ui = dw.ui, server = dw.srv)
