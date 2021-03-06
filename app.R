library(shiny)
library(DT)
library(leaflet)
library(robis)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(shinyjs)
library(stringi)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$script(src="https://unpkg.com/leaflet.vectorgrid@latest/dist/Leaflet.VectorGrid.bundled.js")),
  fluidRow(
    style = "margin-top: 0px;",
    column(12,
      h2("South Pacific invasives dashboard"),
      p(HTML("This dashboard displays a list of WRiMS species observed in the South Pacific based on OBIS data. Species that have been observed in Fiji are flagged as such. Click the row to display all occurrence of a species on the map, or click the scientific name to open the WoRMS page. The references and remarks columns are taken from the priority species lists at <a href=\"https://github.com/iobis/pacman/tree/main/SpeciesLists\" target=\"_blank\">https://github.com/iobis/pacman/tree/main/SpeciesLists</a>."))
    )
  ),
  fluidRow(
    style = "margin-top: 20px;",
    column(12, leafletOutput("mymap"))
  ),
  fluidRow(
    style = "margin-top: 20px; margin-bottom: 10px;",
    column(12, dataTableOutput("mytable"))
  )
)

server <- function(input, output, session) {
  
  withProgress(message = "Fetching data", value = 0, {
    
  cl_fiji <- checklist(wrims = TRUE, areaid = 68) %>%
    select(scientificName, taxonID, class, order, records)
  incProgress(1/4, detail = "Fetching Fiji checklist")
              
  cl_expert <- read.csv("https://raw.githubusercontent.com/iobis/pacman/main/SpeciesLists/SpeciesList.tsv?token=AADXUOMJQ24AIINJURWKUYDAFLISW", sep = "\t") %>%
    select(taxonID = AphiaID_accepted, references, remarks = taxonRemarks, scientificName_accepted) %>%
    mutate(taxonID = as.numeric(taxonID)) %>%
    filter(!is.na(taxonID)) %>%
    group_by(taxonID, scientificName_accepted) %>%
    summarize(references = paste0(stri_remove_empty_na(references), collapse = "; "), remarks = paste0(stri_remove_empty_na(remarks), collapse = "; "))
  incProgress(1/4, detail = "Fetching priority lists")
  
  cl <- checklist(wrims = TRUE, geometry = "POLYGON ((-218 -62, -218 14, -86 14, -67 -23, -74 -62, -218 -62))") %>%
    select(scientificName, taxonID, class, order, records) %>%
    full_join(cl_expert, by = c("taxonID")) %>%
    mutate(scientificName = ifelse(!is.na(scientificName), scientificName, scientificName_accepted)) %>%
    mutate(fiji = taxonID %in% cl_fiji$taxonID) %>%
    mutate(south_pacific = (!is.na(records) & records > 0)) %>%
    mutate(scientificName = paste0("<a href=\"http://www.marinespecies.org/aphia.php?p=taxdetails&id=", taxonID, "#distributions\" target=\"_blank\">", scientificName, "</a>")) %>%
    select(-scientificName_accepted)
  incProgress(2/4, detail = "Fetching South Pacific checklist")
  
  })
  
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
      setView(170, -20, zoom = 3) %>%
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
    datatable(cl, selection = "single", escape = FALSE) %>%
      formatStyle(
        "south_pacific",
        target = "cell",
        backgroundColor = styleEqual(c(0, 1), c("#ffe6cc", NA))
      ) %>%
      formatStyle(
        "fiji",
        target = "cell",
        backgroundColor = styleEqual(c(1, 0), c("#e6f2ff", NA))
      )
  )
  
}

shinyApp(ui, server)