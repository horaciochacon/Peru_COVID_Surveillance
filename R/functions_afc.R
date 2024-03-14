# Helper Functions --------------------------------------------------------


# Filtros -----------------------------------------------------------------

filtro_picker <- function(value, label, variable) {
  pickerInput(
    value,
    h5(label),
    sort(unique(variable)),
    options = list(`actions-box` = TRUE),
    multiple = TRUE,
    selected = unique(variable)
  )
}

filtro_rango_fechas <- function(value, label) {
  dateRangeInput(
    value,
    label = h5(label),
    start = ymd("2020-08-09"),
    end = ymd("2020-08-18"),
    min = ymd("2020-08-09"),
    max = ymd("2020-08-18"),
    format = "dd-mm-yyyy",
    separator = "hasta el",
    language = "es"
  )
}



# Mapa Base ---------------------------------------------------------------

leaflet_base <- function(data) {
  leaflet(data = data, options = leafletOptions(minZoom = 15)) %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     options = tileOptions(minZoom = 15, maxZoom = 17)) %>%
    setMaxBounds(
      lng1 = -73.602914,
      lat1 = -4.522127,
      lng2 = -73.558633,
      lat2 = -4.487898
    )
}


# Mapa Cluster ------------------------------------------------------------

leaflet_cluster <- function(data) {
  icons <- makeAwesomeIcon(icon = "home")
  
  leaflet_base(data) %>%
    addAwesomeMarkers(
      lng = ~ lon,
      lat = ~ lat,
      popup = paste(
        sep = "<br/>",
        paste("<b>Nombre:</b>", data$nombre_vivieda, sep = " "),
        paste("<b>Dirección:</b>", data$direccion_vivienda, sep = " "),
        paste("<b>Habitantes Totales:</b>", data$habitantes_total, sep = " "),
        paste("<b>Habitantes 18+:</b>", data$habitantes_elegibles, sep = " ")
      ),
      clusterOptions = markerClusterOptions(
        spiderfyOnMaxZoom = FALSE,
        disableClusteringAtZoom = 17
      ),
      icon = icons,
    )
}

# Mapa Heatmap ------------------------------------------------------------

leaflet_heatmap <- function(data) {
  leaflet_base(data) %>%
    addHeatmap(lng = ~ lon,
               lat = ~ lat,
               radius = 10)
}

# Mapa Habitantes ------------------------------------------------------------

leaflet_habitantes <- function(data) {
  icons <- makeAwesomeIcon(icon = "home")
  
  leaflet_base(data) %>%
    addAwesomeMarkers(
      lng = ~ lon,
      lat = ~ lat,
      popup = paste(
        sep = "<br/>",
        paste("<b>Nombre:</b>", data$Nombre, sep = " "),
        paste("<b>Sexo:</b>", data$Sexo, sep = " "),
        paste("<b>Edad:</b>", data$Edad, sep = " "),
        paste("<b>Saturación:</b>", data$`Sat O2`, "%", sep = " ")
      ),
      clusterOptions = markerClusterOptions(
        spiderfyOnMaxZoom = FALSE,
        disableClusteringAtZoom = 17
      ),
      icon = icons,
    )
}

# Data Table --------------------------------------------------------------

column_def_habitantes <- list(
  list(targets = c(2, 8), width = "250px"),
  list(targets = c(1, 10), width = "120px"),
  list(targets = c(3), width = "250px"),
  list(
    targets = c(18),
    width = "120px",
    className = "text-left"
  ),
  list(targets = c(23), width = "300px"),
  list(targets = c(27), width = "350px"),
  list(
    targets = c(4:7, 9:17, 19:22, 24:26),
    width = "80px",
    className = "text-center"
  )
)

column_def_factibilidad <- list(
  list(
    targets = c(1),
    width = "150px",
    className = "text-left"
  ),
  list(
    targets = c(2:11),
    width = "100px",
    className = "text-center"
  )
)

data_table_other <- function(data) {
  datatable(
    data = data,
    extensions = list(
      "FixedColumns" = NULL,
      "Buttons" = c('copy', 'csv', 'excel', 'pdf', 'print')),
    escape = FALSE,
    options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      language = list(
        url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
        ),
      lengthMenu = list(c(10, 25, 50, 100,-1),
                        c("10", "25", "50", "100", "Todos")),
      dom = '<"top"Bf>t<"bottom"lp><"clear">',
      scrollY = "65vh",
      columnDefs = column_def_habitantes
    )
  )
}

data_table <- function(data) {
  datatable(
    data = data,
    extensions = list(
      "Buttons" =  NULL
      ),
    escape = FALSE,
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
}
