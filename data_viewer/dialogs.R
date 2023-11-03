### General Info Dialogs ---------------------------------------------------------------------------
# Add a new organisation.
DisplayQuery <- function(id, query.chr) {

      library("shinyAce")

      ns <- shiny::NS(id)
      shiny::modalDialog(
          title = "Query Information"

        , shinyAce::aceEditor(
              outputId = ns("ace")
            , selectionId = ns("selection")
            , value = query.chr
            , placeholder = "Show a placeholder when the editor is empty ..."
            , mode = "sql"
          )

        , easyClose = TRUE
        , fade = TRUE

        , footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            , shiny::actionButton(ns("requery"), "Requery")
          )
      )
}



### --- General Warning and Error Dialogs ----------------------------------------------------------

WarnAboutBadSQL <- function(stmt.chr, w) {
    shiny::modalDialog(
          title = "SQL Error"
        , footer = NULL  # Remove the footer buttons
        , paste("SQL statement was unsuccessful.", w$message)
        , shiny::h4("Offending Statement")
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
            , stmt.chr
          )

        , easyClose = TRUE  # Allow closing the dialog by clicking outside
        , fade = TRUE
    )
}


ReportGeneralError <- function(e) {
    shiny::modalDialog(
          title = "General Error"
        , footer = NULL  # Remove the footer buttons
        , paste("Something went wrong…")
        , shiny::h5("System Error Message:")
        , shiny::verbatimTextOutput(e$message)
        , easyClose = TRUE  # Allow closing the dialog by clicking outside
        , fade = TRUE
    )
}



### --- Module Specific Dialogs --------------------------------------------------------------------


## -- Organisations --------------------------------------------------------------------------------


## -- Contacts -------------------------------------------------------------------------------------
# Add a new organisation.
organisations.AddOrga <- function(id, possible.v) {
      ns <- shiny::NS(id)
      shiny::modalDialog(
          title = "Register Organisation"

        , shiny::textInput(ns("identifier"), NULL, placeholder = "Company Name")
        , shiny::textInput(ns("short_identifier"), NULL, placeholder = "Shorthand (optional)")
        , shiny::selectInput(ns("connection"), "Connection (Optional)", possible.v)

        , footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            , shiny::actionButton(ns("add_record"), "Add Organisation")
          )

        , easyClose = TRUE
        , fade = TRUE
      )
}


# Add a new contact.
contacts.AddContact <- function(ua.v) {
    shiny::modalDialog(
          shiny::h4("Personal Information")
        , shiny::textInput("contacts.add_record.given_name", NULL, placeholder = "Given Name")
        , shiny::textInput("contacts.add_record.surname", NULL, placeholder = "Surname")
        , shiny::selectInput("contacts.add_record.unix_account", "Unix Account", c(ua.v, "n.a."))
        , title = "Add Contact"
        , footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            , shiny::actionButton("contacts.add_record.add", "Add Contact")
          )
        , easyClose = TRUE
        , fade = TRUE
    )
}
