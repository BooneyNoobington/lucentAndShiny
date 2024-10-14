### ------------------------------------------------------------------------------------------------
### --- Edit SQL Queries and Statements ------------------------------------------------------------
### ------------------------------------------------------------------------------------------------



# Display a modal dialog with an editor.
aceDialog <- function(ns, query.chr) {

    shiny::showModal(
        shiny::modalDialog(
            title = "Query Information"

          , shinyAce::aceEditor(
                outputId = ns("sql")
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
    )
}
