### --- General Warning and Error Dialogs ----------------------------------------------------------

WarnAboutBadSQL <- function(reactive.values) {
    shiny::modalDialog(
          title = "SQL Error"
        , footer = NULL  # Remove the footer buttons
        , paste("SQL statement was unsuccessful.", reactive.values$status_message)
        , shiny::h4("Offending Statement:")
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
            , reactive.values$statement
          )

        , easyClose = TRUE  # Allow closing the dialog by clicking outside
        , fade = TRUE
    )
}



### --- Module Specific Dialogs --------------------------------------------------------------------


## -- Contacts -------------------------------------------------------------------------------------

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
