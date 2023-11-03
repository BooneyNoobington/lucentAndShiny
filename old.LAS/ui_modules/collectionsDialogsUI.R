### ------------------------------------------------------------------------------------------------
### --- Dialogs for the Collections Module ---------------------------------------------------------
### ------------------------------------------------------------------------------------------------


# Add a new collection.
newCollectionUI <- function(
    id
  , next.name.chr
  , objects.v
  , matrices.v
  , col_classes.v
  , spots.v
  , sites.v
)
{

    # Namespacing.
    ns <- shiny::NS(id)

    # Create a variable to store the selected radio button value
    relate_to.chr <- shiny::reactiveVal("Spot")

    # Functions returns a modal dalog.
    shiny::modalDialog(

          shinyjs::useShinyjs()

        , title = "Add Collection"


          # Pick a name for the sample.
        , shinyjs::disabled(
              shiny::textInput("collection.name", "Next Identifier", next.name.chr)
          )

        , shinyWidgets::pickerInput(
              inputId = "matrix.name.pick"
            , label = "Select a Matrix"
            , choices = matrices.v
            , options = list("live-search" = TRUE)  # Enable live search/autocomplete
          )

        , shiny::textInput("matrix.name.set", NULL, placeholder = "Create a new matrix")

          # Pick a collection class.
        , shinyWidgets::pickerInput(
              inputId = "collection_class.name"
            , label = "Select a Collection Class"
            , choices = col_classes.v
            , options = list("live-search" = TRUE)  # Enable live search/autocomplete
          )

          # Choose wether spot or site.
        , shinyWidgets::radioGroupButtons(
              inputId = "relate_to.rbtn"
            , label = NULL
            , choices = c("Spot", "Site", "Compound")
            , justified = TRUE
            , selected = relate_to.chr()
          )

          # Conditional rendering of the "spot.name" or "site.name" menu
        , shiny::conditionalPanel(
              condition = 'input["relate_to.rbtn"] == "Spot"',
              shinyWidgets::pickerInput(
                  inputId = "spot.name"
                , label = "Select a Spot"
                , choices = spots.v
                , options = list("live-search" = TRUE)
              )
          )

        , shiny::conditionalPanel(
              condition = 'input["relate_to.rbtn"] == "Site"',
              shinyWidgets::pickerInput(
                  inputId = "site.name"
                , label = "Select a Site"
                , choices = sites.v
                , options = list("live-search" = TRUE)
              )
          )



        , footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            , shiny::actionButton("collections.ctrl.add_record.add", NULL, shiny::icon("plus"))
          )

        , size = "s"
        , easyClose = TRUE
        , fade = TRUE
    )
}



# Add a new object.
DrawAddObject <- function(ns) {

    # Functions returns a modal dalog.
    shiny::modalDialog(

          title = "Add Object"

          # A name for the object.
        , shiny::textInput(ns("add_object.dialog.name"), NULL, placeholder = "Object")

          # Add a small description.
        , shiny::textInput(ns("add_object.dialog.description"), NULL, placeholder = "Description")

        , footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            , shiny::actionButton(ns("add_object.dialog.add_record"), NULL, shiny::icon("plus"))
          )

        , size = "s"
        , easyClose = TRUE
        , fade = TRUE
    )

}
