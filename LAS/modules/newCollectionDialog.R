### ------------------------------------------------------------------------------------------------
### --- Dialogs for the Collections Module ---------------------------------------------------------
### ------------------------------------------------------------------------------------------------


# Add a new collection.
newCollectionDialog <- function(
    ns
  , next.name.chr
  , objects.v
  , matrices.v
  , col_classes.v
  , spots.v
  , sites.v
)
{

    # Functions returns a modal dalog.
    shiny::modalDialog(

        shinyjs::useShinyjs()

      , title = "Add Collection"


        # Pick a name for the sample.
      , shinyjs::disabled(
            shiny::textInput(ns("collection.name"), "Next Identifier", next.name.chr)
        )

      , shinyWidgets::pickerInput(
            inputId = ns("matrix.name.pick")
          , label = "Select a Matrix"
          , choices = matrices.v
          , options = list("live-search" = TRUE)  # Enable live search/autocomplete
        )

      , shiny::textInput(ns("matrix.name.set"), NULL, placeholder = "or create a new matrix")

        # Pick a collection class.
      , shinyWidgets::pickerInput(
            inputId = ns("collection_class.name.pick")
          , label = "Select a Collection Class"
          , choices = col_classes.v
          , options = list("live-search" = TRUE)  # Enable live search/autocomplete
        )

      , shiny::textInput(
            ns("collection_class.name.set")
          , NULL
          , placeholder = "or create a new collection class"
        )

        # Choose wether spot or site.
      , shinyWidgets::radioGroupButtons(
            inputId = ns("relate_to_choice")
          , label = "Relate Collection to"
          , choices = c("Spot", "Site", "Compound")
          , justified = TRUE
        )

        # Conditional rendering of the "spot.name" or "site.name" menu
      , shiny::conditionalPanel(
            condition = 'input["relate_to_choice"] == "Spot"'
          , ns = ns
          , shinyWidgets::pickerInput(
                inputId = ns("spot.name.pick")
              , label = "Select a Spot"
              , choices = spots.v
              , options = list("live-search" = TRUE)
            )
        )

      , shiny::conditionalPanel(
            condition = 'input["relate_to_choice"] == "Site"'
          , ns = ns
          , shinyWidgets::pickerInput(
                inputId = ns("site.name.pick")
              , label = "Select a Site"
              , choices = sites.v
              , options = list("live-search" = TRUE)
            )
        )

      , shiny::conditionalPanel(
            condition = 'input["relate_to_choice"] == "Compound"'
          , ns = ns
          , shinyWidgets::pickerInput(
                inputId = ns("compound.name.pick")
              , label = "Select a Compound"
              , choices = LETTERS  # TODO: Remove placeholder as soon as table is established.
              , options = list("live-search" = TRUE)
            )
        )



      , footer = shiny::tagList(
            shiny::modalButton("Dismiss")
          , shiny::actionButton(ns("add_collection"), NULL, shiny::icon("plus"))
        )

      , size = "s"
      , easyClose = TRUE
      , fade = TRUE
  )

}
