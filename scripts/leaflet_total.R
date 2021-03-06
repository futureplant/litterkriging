# function that makes a visualization of intensity of total litter amounts and trashcan locations ----

visualize_total <- function(){
    
  # Read in data -----
    tot <- raster('output/litter_total_indicatorkriging.tif')
    roads <- getPoints("data/prc02_osm_road_reclassified.shp")
    
    # preprocessing ----
    roads <- st_transform(roads, "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m +no_def")
    roads <- st_transform(roads, '+proj=longlat +datum=WGS84')
    trashcans <- getPoints('data/prb02_trashcan_6jun2019.shp')
    trashcans <- st_transform(trashcans, "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m +no_def")
    trashcans <- st_transform(trashcans, '+proj=longlat +datum=WGS84')
    
    # construct color palettes for visualization ----
    qpal <- colorNumeric(palette = "YlOrRd", domain = NULL, na.color = NA, reverse = F)
    legendpal <- c("#ffffb2","#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
    
    # visualize in leaflet ----
    m <- leaflet() %>% setView(lng = 6.643071, lat = 52.030869, zoom = 12)
    m %>% addTiles()
    leaflet() %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addRasterImage(tot, colors = qpal, opacity = 0.8) %>%
      addPolylines(data = roads, opacity = 0, label=~road_type)%>%
      addCircleMarkers(data = trashcans, group = "trashcans", radius = 5) %>%
      
      # Add functionality to toggle layers
      addLayersControl(
        overlayGroups = c("trashcans"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      
      # Add legend
      addLegend(
        position = 'bottomright',
        colors = legendpal,
        labels = c("0 pieces", "1 or 2 pieces", "3 or 4 pieces", "5 or 6 pieces", ">6 pieces"), opacity = 1,
        title = 'Expected amount \n of litter'
      )
}
