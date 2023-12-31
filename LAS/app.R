### ------------------------------------------------------------------------------------------------
### --- "LucentAndShiny" ---------------------------------------------------------------------------
### --- A Shiny based interface for lucentLIMS -----------------------------------------------------
### ------------------------------------------------------------------------------------------------




### --- Requirements -------------------------------------------------------------------------------
library(shiny)                      # Basic user interface components.
library(shinydashboard)             # Building the user interface.
library(DT)                         # Advanced table output.
library(ini)                        # Load system configuration.
library(leaflet)                    # Dispaly spatial data.
library(shinyAce)                   # Show a small editor.

# Source UI specifications.
source("./modules/sidebarUI.R")
source("./modules/collectionsUI.R")
source("./modules/locationsUI.R")

# Source server logic.
source("./modules/locationsServer.R")
source("./modules/collectionsServer.R")

# Load system configuration.
sys.cnf <- ini::read.ini("./conf/las.ini")

# Translate the "general" section into options.
if (!is.null(sys.cnf$general)) options(sys.cnf$general)

coltrans.cnf <- ini::read.ini("./conf/coltrans.ini")

# Determine current user.
unix_user.chr <- Sys.getenv("LOGNAME")



### --- Translations / Internationalizations -------------------------------------------------------
library(shiny.i18n)                 # Translation package.
i18n <- shiny.i18n::Translator$new(translation_csvs_path = "./translations")
i18n$set_translation_language("de")



### --- Contact the Database -----------------------------------------------------------------------
mariadb.con <- pool::dbPool(
    RMariaDB::MariaDB()
  , dbname = sys.cnf$database$schema
  , unix.sock = sys.cnf$database$socket
  , username = unix_user.chr
)


### --- User interface -----------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(

    # Display the lucentLIMS logo in the upper left.
    shinydashboard::dashboardHeader(title = "lucentLIMS")

    # Display the sidebar. Its contents are defined via modules.
  , shinydashboard::dashboardSidebar(DrawSidebarMenu(i18n))


    # Draw the bodies depending on the selected item in the sidebar.
  , shinydashboard::dashboardBody(
        shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "collections", collectionsUI("collections"))
          , shinydashboard::tabItem(tabName = "locations", locationsUI("locations"))
        )
    )

)



### --- Server logic -------------------------------------------------------------------------------
server <- function(input, output, session) {
      locationsServer("locations", mariadb.con, coltrans.cnf$locations)
      collectionsServer("collections", mariadb.con, coltrans.cnf$collections, sys.cnf)
}



### Initialize the app -----------------------------------------------------------------------------
shiny::shinyApp(ui, server, options = list(port = 4001, host = "0.0.0.0"))
