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
                , shiny::h4("SQL-Information")
                , shiny::tags$style(
                    shiny::HTML(
                    "
                        .monospace-textarea {
                            font-family: 'Hack', monospace;
                        }
                    ")
                    )
                , shiny::div(
                    class = "monospace-textarea"
                    , shiny::textAreaInput("organisations.sql_info", NULL, rows = 7)
                )
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
