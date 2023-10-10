# Define the modal dialog
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
