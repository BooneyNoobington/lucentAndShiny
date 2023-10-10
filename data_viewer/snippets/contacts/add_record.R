shiny::modalDialog(
    shiny::h4("Personal Information")
  , shiny::textInput("contacts.add_record.given_name", NULL, placeholder = "Given Name")
  , shiny::textInput("contacts.add_record.surname", NULL, placeholder = "Surname")
  , shiny::selectInput("contacts.add_record.unix_account", "Unix Account", ua.lst)
  , title = "Add Contact"
  , footer = shiny::tagList(
        shiny::modalButton("Dismiss")
      , shiny::actionButton("contacts.add_record.add", "Add Contact")
    )
  , easyClose = TRUE
  , fade = TRUE
)
