print("Opening dialog for adding a record in table \'organisation\'")

add_record.dialog <- shiny::modalDialog(
     title = "Adding a New Organisation to Database"
   , shiny::textInput(
        "company_name"
      , NULL
      , placeholder = "Company Name"
    )

  , shiny::textInput(
        "shorthand"
      , NULL
      , placeholder = "Shorthand (optional)"
    )

  , shiny::textInput(
        "connection"
      , NULL
      , placeholder = "Connection"
    )

  , footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_add_record", "Add Record")
    )
)

reactive.values$test = add_record.dialog$company_name

#reactive.values$next_sql = "INSERT INTO `organisation` (`identifier`, `short_identifier`, `connection`)"
