### ------------------------------------------------------------------------------------------------
### --- Modules to be Use in the Main Server Function ----------------------------------------------
### ------------------------------------------------------------------------------------------------



### --- Organisations Module -----------------------------------------------------------------------

## -- Register a New Organisation ------------------------------------------------------------------

# Compute input when a new organisation is added.
registerOrga <- function(id) {
    shiny::moduleServer(
        id
      , function(input, output, session) {

            # Create a list of columns and their respective values to be inserted into.
            new_orga.lst <- list(
                identifier = input$identifier
              , short_identifier = input$short_identifier
              , connection = input$connection
            )

            # Form the statement using a dedicated function.
            return(InsertByList("organisation", new_orga.lst))

        }
    )
}
