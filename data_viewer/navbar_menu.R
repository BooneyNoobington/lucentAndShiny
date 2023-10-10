# Module UI function
navbar_menu.ui <- function(id) {
    # `NS(id)` returns a namespace function, which was save as `ns` and will
    # invoke later.
    ns <- shiny::NS(id)

    shiny::navbarMenu(
        id

      , shiny::tabPanel(
            ns("Samples")
          , shiny::titlePanel(ns("Sample"))
          , shiny::mainPanel(DT::DTOutput(ns("samples.table")))
          , shiny::sidebarPanel(
                shiny::actionButton(ns("samples.add_record"), "Add Sample", icon = icon("plus"))
            )
          , icon = shiny::icon("vials")
        )

      , shiny::tabPanel(
            ns("Organisations")
          , shiny::mainPanel(DT::DTOutput(ns("organisations.table")))
          , shiny::sidebarPanel(
                shiny::h4("General")
              , shiny::actionButton(ns("organisations.add_record"), "Add Record", icon = icon("plus"))
              , shiny::actionButton(ns("organisations.del_record"), "Delete Selected", icon = icon("minus"))
            )
          , icon = shiny::icon("building")
        )

      , shiny::tabPanel(
            ns("Contacts")
          , shiny::mainPanel(DT::DTOutput(ns("contacts.table")))
          , shiny::sidebarPanel(
                shiny::h4("General")
              , shiny::actionButton(ns("contacts.add_record"), "Add Record", icon = icon("plus"))
              , shiny::actionButton(ns("contacts.del_record"), "Delete Selected", icon = icon("minus"))
            )
          , icon = shiny::icon("address-book")
        )

    )


}
