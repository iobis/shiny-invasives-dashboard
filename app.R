library(shiny)
library(DT)
library(leaflet)
library(robis)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(shinyjs)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$script(src="https://unpkg.com/leaflet.vectorgrid@latest/dist/Leaflet.VectorGrid.bundled.js")),
  fluidRow(
    style = "margin-top: 20px;",
    column(12, leafletOutput("mymap"))
  ),
  fluidRow(
    style = "margin-top: 20px; margin-bottom: 0px;",
    column(12, p("This dashboard displays a list of WRiMS species observed in the South Pacific based on OBIS data. Species that have been observed in Fiji are flagged as such. Click the column row to display all occurrence of a species on the map."))
  ),
  fluidRow(
    style = "margin-top: 20px; margin-bottom: 10px;",
    column(12, dataTableOutput("mytable"))
  )
)

server <- function(input, output, session) {
  
  cl_fiji <- checklist(wrims = TRUE, areaid = 68) %>%
    select(scientificName, taxonID, class, order, records)
  
  cl <- checklist(wrims = TRUE, geometry = "POLYGON ((-210 13, -210 -60, -100 -60, -100 13, -210 13))") %>%
    select(scientificName, taxonID, class, order, records) %>%
    mutate(fiji = taxonID %in% cl_fiji$taxonID)

  observe({
    selected <- input$mytable_rows_selected
    if (!is.null(selected)) {
      taxonID = cl$taxonID[selected]
      runjs(paste0("
        const map = document.getElementById('mymap')._leaflet_map;
        map.eachLayer(function(layer) {
          if (layer.vectorgrid) map.removeLayer(layer);
        });
        const styling = {
          grid: {
            fill: true,
            weight: 1,
            fillColor: '#ff0066',
            color: '#ff0066',
            fillOpacity: 0.2,
            opacity: 0.4,
          }
        }
        const layer = L.vectorGrid.protobuf('https://api.obis.org/occurrence/tile/{x}/{y}/{z}.mvt?cellspertile=30&grid=geotile&taxonid=", taxonID, "', {
            vectorTileLayerStyles: styling
        });
        layer.type = 'test';
        layer.addTo(map);
      "))
    }
  })
    
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(140, -10, zoom = 3) %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = FALSE)) %>%
      onRender("
        function(el, x) {
          L.VectorGrid.Protobuf.include({
            vectorgrid: true
          });
          el._leaflet_map = this;
        }
      ")
  })
  output$mytable <- renderDataTable(
    datatable(cl, selection = "single") %>% formatStyle(
      "fiji",
      target = "cell",
      backgroundColor = styleEqual(c(1, 0), c("#ffe6cc", NA))
    )
  )
  
}

shinyApp(ui, server)