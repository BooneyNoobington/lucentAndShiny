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
                , shiny::actionButton("organisations.sql_info", NULL, icon = shiny::icon("database"))

              )
            , icon = shiny::icon("building")
          )

        , shiny::tabPanel(
              "Contacts"
            , shiny::mainPanel(
                  shiny::h3("People")
                , DT::DTOutput("contacts.table")  # Master Table.

                  # There is an n to m relation between people, organisations and addresses.
                , shiny::h3("Asscociations")
                , shiny::tabsetPanel(
                      shiny::tabPanel("Addresses", DTOutput("contacts.addresses"))
                    , shiny::tabPanel("Organisations", DTOutput("contacts.organisations"))
                  )

              )
            , shiny::sidebarPanel(
                shiny::h4("General")
                , shiny::actionButton("contacts.add_record", "Add Record", icon = icon("plus"))
                , shiny::actionButton("contacts.del_record", "Delete Selected", icon = icon("minus"))
              )
            , icon = shiny::icon("address-book")
          )

    )
}
