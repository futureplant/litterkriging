library(leaflet)
library(raster)

tot <- raster('output/litter_total_indicatorkriging.tif')
plastics <- raster('output/plastics_indicatorkriging.tif')
paper <- raster('output/paper_indicatorkriging.tif')
organic <- raster('output/organic_indicatorkriging.tif')
other <- raster('output/other_indicatorkriging.tif')
cigarette <- raster('output/cigarette_indicatorkriging.tif')

plastics <- as.factor(plastics)

# pal <- colorNumeric(c("#D0FF92", "#F10F50"), values(tot),
#                    na.color = "transparent")



qpal <- colorNumeric(palette = "YlOrRd", domain = NULL, na.color = NA, reverse = F, n=5)
legendpal <- c("#ffffb2","#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")


m <- leaflet() %>% setView(lng = 6.643071, lat = 52.030869, zoom = 12)

m %>% addTiles()
leaflet() %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  #Base groups
  addRasterImage(plastics, colors = qpal, opacity = 0.8, group = "plastics") %>%
  addRasterImage(paper, colors = pal, opacity = 0.8, group = "paper") %>%
  addRasterImage(organic, colors = pal, opacity = 0.8, group = "organic") %>%
  addRasterImage(other, colors = pal, opacity = 0.8, group = "other") %>%
  addRasterImage(cigarette, colors = pal, opacity = 0.8, group = "cigarette") %>%
  addLayersControl(
   baseGroups = c("plastics", "paper", "organic", "other", "cigarette"),
   options = layersControlOptions(collapsed = FALSE)) %>%

  addLegend(
    position = 'bottomright',
    colors = legendpal,
    labels = c("0 pieces", "1 piece", "2 pieces", "3 pieces", ">3 pieces"), opacity = 1,
    title = 'Expected amount \n of litter'
  )

  
