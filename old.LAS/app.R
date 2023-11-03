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

# Source UI specifications.
source("./ui_modules/sidebarModuleUI.R")
source("./ui_modules/collectionsModuleUI.R")
source("./ui_modules/spatialBodyUI.R")
source("./ui_modules/mainBodyModuleUI.R")
source("./ui_modules/classesModuleUI.R")

# Source server logic.
source("./server_modules/spatialModuleServer.R")
source("./server_modules/collectionsModuleServer.R")

# Load system configuration.
sys.cnf <- ini::read.ini("./conf/las.ini")
coltrans.cnf <- ini::read.ini("./conf/coltrans.ini")

# Determine current user.
unix_user.chr <- Sys.getenv("LOGNAME")



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
  , shinydashboard::dashboardSidebar(DrawSidebarMenu())


    # Draw the bodies depending on the selected item in the sidebar.
  , shinydashboard::dashboardBody(
        shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "collections", DrawCollectionsBody("collections"))
          , shinydashboard::tabItem(tabName = "locations", DrawSpatialBody("spatial"))
          , shinydashboard::tabItem(tabName = "classes", DrawClassesBody("classes"))
        )
    )

)



### --- Server logic -------------------------------------------------------------------------------
server <- function(input, output, session) {
      spatialServer("spatial", mariadb.con, coltrans.cnf$spatial)
      collectionsServer("collections", mariadb.con, coltrans.cnf$collections, sys.cnf)
}



### Initialize the app -----------------------------------------------------------------------------
shiny::shinyApp(ui, server, options = list(port = 4001, host = "0.0.0.0"))
