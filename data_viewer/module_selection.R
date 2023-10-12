module_selection.ui <- function() {
    shiny::navbarMenu(
          "Modules"
        , shiny::tabPanel(
              "Samples"
            , shiny::titlePanel("Sample")
            , shiny::mainPanel(DT::DTOutput("samples.table"))
            , shiny::sidebarPanel(
                shiny::actionButton("samples.add_record", "Add Sample", icon = icon("plus"))
            )
            , icon = shiny::icon("vials")
        )

        , shiny::tabPanel(
              "Organisations"
            , shiny::mainPanel(DT::DTOutput("organisations.table"))
            , shiny::sidebarPanel(
                    shiny::h4("General")
                , shiny::actionButton("organisations.add_record", "Add Record", icon = icon("plus"))
                , shiny::actionButton("organisations.del_record", "Delete Selected", icon = icon("minus"))
                , shiny::actionButton("organisations.link_address", "Link Address", icon = icon("link"))

              )
            , icon = shiny::icon("building")
        )

        , shiny::tabPanel(
              "Contacts"
            , shiny::mainPanel(DT::DTOutput("contacts.table"))
            , shiny::sidebarPanel(
                shiny::h4("General")
                , shiny::actionButton("contacts.add_record", "Add Record", icon = icon("plus"))
                , shiny::actionButton("contacts.del_record", "Delete Selected", icon = icon("minus"))
              )
            , icon = shiny::icon("address-book")
        )
    )
}
