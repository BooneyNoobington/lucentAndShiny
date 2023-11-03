### ------------------------------------------------------------------------------------------------
### --- Server Module for Interacting With Spatial Data --------------------------------------------
### ------------------------------------------------------------------------------------------------


## -- Import Functionality -------------------------------------------------------------------------
library(leaflet)         # Use leaflet to display maps.
library(leaflet.extras)  # Address search bar.
library(pool)            # Handle interaction with database.
library(shinyWidgets)
source("./dbInterop.R")  # Handle queries and statements.

# Module server function
spatialServer <- function(id, db.conn, coltrans.lst) {
    shiny::moduleServer(
        id
      , function(input, output, session) {

            # Reactive values.
            master.df <- shiny::reactiveVal(NULL)
            master.query <- shiny::reactiveVal(
                paste(readLines("./SQL/QUERY_SPOTS.SQL"), collapse = "\n")
            )
            requery.bol <- shiny::reactiveVal(FALSE)
            master.stmt <- shiny::reactiveVal(NULL)

            sites.query <- shiny::reactiveVal(
                paste(readLines("./SQL/QUERY_SITES.SQL"), collapse = "\n")
            )
            sites.df <- shiny::reactiveVal(NULL)
            shiny::observeEvent(
                sites.query()
              , {sites.df(pool::dbGetQuery(db.conn, sites.query()))}
            )

            output$sites.tbl <- DT::renderDT({
                # Draw the table via the DT package.
                DT::datatable(
                    sites.df()
                  , editable = TRUE  # Users generally can edit the table. TODO: Function that sets this valu accourding to UPDATE and INSERT privileges of current user?
                  , rownames = FALSE  # No running numers.
                    # Only one row should be selectable at a time.
                  , selection = list(mode = "single", target = "row")
                  , colnames = unlist(
                        coltrans.lst[which(coltrans.lst %in% names(sites.df()))]
                    )
                )
            })


            # Rebuild the master data frame every time the master query changes.
            shiny::observeEvent(
                master.query()
              , {
                  master.df(pool::dbGetQuery(db.conn, master.query()))
                }
            )

            # If a statment was provided by some server logic, try to execute it
            # against the database.
            shiny::observeEvent(
                master.stmt()
              , {
                    if(!is.null(master.stmt())) pool::dbExecute(db.conn, master.stmt())
                    master.query(paste(master.query(), "\n -- requery after insert."))
                }
            )

            # Draw the master table according to the master data frame.
            output$master.tbl <- DT::renderDT({
                # Draw the table via the DT package.
                DT::datatable(
                    master.df()
                  , editable = TRUE  # Users generally can edit the table. TODO: Function that sets this valu accourding to UPDATE and INSERT privileges of current user?
                  , rownames = FALSE  # No running numers.
                    # Only one row should be selectable at a time.
                  , selection = list(mode = "single", target = "row")
                  , colnames = unlist(
                        coltrans.lst[which(coltrans.lst %in% names(master.df()))]
                    )
                )
            })


            # Initial display of the map.
            output$master.map <- leaflet::renderLeaflet({
                leaflet::leaflet() %>%
                  leaflet::addTiles() %>%
                    leaflet::setView(lng = 8.403653, lat = 49.006890, zoom = 13) %>%
                      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>%
                        leaflet.extras::addDrawToolbar(
                            polylineOptions = FALSE
                          , circleMarkerOptions = FALSE
                          , rectangleOptions = FALSE
                          , circleOptions = FALSE
                        )

            })


            # Follow the users selection of the row.
            shiny::observeEvent(
                    input$master.tbl_rows_selected
                  , {

                        i <- input$master.tbl_rows_selected

                        xcoord <- master.df()[i, "s.x"]
                        ycoord <- master.df()[i, "s.y"]

                        # Render the map inside this observeEvent to ensure a reactive context.
                        output$master.map <- leaflet::renderLeaflet({
                            leaflet::leaflet() %>%
                              leaflet::addTiles() %>%
                                leaflet::setView(lng = ycoord, lat = xcoord, zoom = 30) %>%
                                  leaflet::addPopups(
                                      lng = ycoord
                                    , lat = xcoord
                                    , master.df()[i, "spot.name"]
                                  ) %>%
                                    leaflet.extras::addSearchOSM(
                                       options = searchOptions(collapsed = TRUE)
                                    ) %>%
                                      leaflet.extras::addDrawToolbar(
                                          polylineOptions = FALSE
                                        , circleMarkerOptions = FALSE
                                        , rectangleOptions = FALSE
                                        , circleOptions = FALSE
                                      )
                        })

                    }
            )

            # Clicking on the + button should open the dialogbox.
            shiny::observeEvent(
                input[["master.ctrl-add_record"]]
              , {shiny::showModal(DrawAddSpot("add.spot"))}
            )

            # A user can draw markers and polygons on the map.
            # Decide by the coordinates which one it is and open respective modal dialogs.
            shiny::observe({
                click <- shiny::req(input$master.map_draw_all_features)

                    # Capture the drawing.
                    new.form <- input$master.map_draw_all_features$features[[1]]

                    xy.v <- new.form$geometry$coordinates

                    if (new.form$geometry$type == "Point") {
                        latitude.dbl <- xy.v[[2]]       # y-Coordinate. ~ 48 in Karlsruhe.
                        longitude.dbl <- xy.v[[1]]      # x-Coordinate. ~ 8 in Karlsruhe.

                        # Pass the sessions namespace to the modal dialog.
                        # Otherwise its inputs aren't available.
                        shiny::showModal(DrawAddSpot(session$ns, latitude.dbl, longitude.dbl))

                        # Re-render the map. Clears all drawings.
                        # TODO: Very long and repetetive. Proxy or function.
                        output$master.map <- leaflet::renderLeaflet({
                        leaflet::leaflet() %>%
                            leaflet::addTiles() %>%
                            leaflet::setView(lng = xy.v[[1]], lat = xy.v[[2]], zoom = 13) %>%
                                leaflet.extras::addSearchOSM(
                                    options = searchOptions(collapsed = TRUE)
                                ) %>%
                                leaflet.extras::addDrawToolbar(
                                      polylineOptions = FALSE
                                    , circleMarkerOptions = FALSE
                                    , rectangleOptions = FALSE
                                    , circleOptions = FALSE
                                ) %>%
                                    leaflet::addPopups(
                                        lng = xy.v[[1]]
                                      , lat = xy.v[[2]]
                                      , input$identifier
                                    )
                        })

                    } else if (new.form$geometry$type == "Polygon") {
                        # Flatten the list and convert it into a matrix
                        xy.v.flat <- do.call(rbind, unlist(xy.v, recursive = FALSE))

                        # Convert the matrix into the desired format
                        xy.v.chr <- paste(xy.v.flat[, 1], xy.v.flat[, 2], collapse = ", ")

                        crs.df <- pool::dbGetQuery(db.conn, "SELECT * FROM `crs`")

                        shiny::showModal(
                            DrawAddSite(ns = session$ns, borders = xy.v.chr, crs.df = crs.df)
                        )



                    }

            })

            # Ad a new spot.
            shiny::observeEvent(
                input$add_record
              , {
                    # Sanitize the user input.
                    if(input$identifier == "") {ident <- NULL} else {ident <- input$identifier}
                    if(input$description == "") {desc <- NULL} else {desc <- input$description}

                    master.stmt(
                        CreateStatementByList(
                            table.chr = "spot"
                          , kav.lst = list(
                                name = ident
                              , description = desc
                              , x = input$x
                              , y = input$y
                              , id_crs = 1  # OpenStreetMap uses WGS 84 (id = 1)
                            )
                        )
                    )

                    shiny::removeModal()
                }
            )



        }
    )
}
