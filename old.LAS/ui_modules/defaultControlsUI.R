### ------------------------------------------------------------------------------------------------
### --- UI for Default Operations ------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------



DrawDefaultControls <- function(id) {

    # Namespacing.
    ns <- shiny::NS(id)

    # Wrap the buttons in a container div and assign a CSS class to it.
    div_wrapper <- shiny::div(
        # Add a record.
        shiny::actionButton(ns("add_record"), NULL, shiny::icon("plus"))
        # Delete a record.
      , shiny::actionButton(ns("del_record"), NULL, shiny::icon("trash"))
        # Show SQL information.
      , shiny::actionButton(ns("sql_info"), NULL, shiny::icon("database"))
        # Set the class for the container div.
      , class = "button-container"
    )

    # Define the CSS to style the buttons in the same row.
    button_css <- "
        .button-container {
            display: flex;
            flex-direction: row;
            gap: 10px; /* Adjust the gap as needed */
        }
    "

    # Add the CSS to the page.
    shiny::tags$style(HTML(button_css))

    shiny::tagList(
        div_wrapper
    )
}
