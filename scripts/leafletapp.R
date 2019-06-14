# load libraries ----
library(leaflet)
library(raster)

# Read in data ----
tot <- raster('output/litter_total_indicatorkriging.tif')
plastics <- raster('output/plastics_indicatorkriging.tif')
paper <- raster('output/paper_indicatorkriging.tif')
organic <- raster('output/organic_indicatorkriging.tif')
other <- raster('output/other_indicatorkriging.tif')
cigarette <- raster('output/cigarette_indicatorkriging.tif')

# construct color palettes for visualization ----
qpal <- colorNumeric(palette = "YlOrRd", domain = NULL, na.color = NA, reverse = F)
legendpal <- c("#ffffb2","#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")


# visualize in leaflet ----
m <- leaflet() %>% setView(lng = 6.643071, lat = 52.030869, zoom = 12)
m %>% addTiles()
leaflet() %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  # Base groups
  addRasterImage(plastics, colors = qpal, opacity = 0.8, group = "plastics") %>%
  addRasterImage(paper, colors = qpal, opacity = 0.8, group = "paper") %>%
  addRasterImage(organic, colors = qpal, opacity = 0.8, group = "organic") %>%
  addRasterImage(other, colors = qpal, opacity = 0.8, group = "other") %>%
  addRasterImage(cigarette, colors = qpal, opacity = 0.8, group = "cigarette") %>%
  
  # add functionality to toggle layers
  addLayersControl(
   baseGroups = c("plastics", "paper", "organic", "other", "cigarette"),
   options = layersControlOptions(collapsed = FALSE)) %>%
  
  # add legend
  addLegend(
    position = 'bottomright',
    colors = legendpal,
    labels = c("0 pieces", "1 piece", "2 pieces", "3 pieces", ">3 pieces"), opacity = 1,
    title = 'Expected amount \n of litter'
  )

  
