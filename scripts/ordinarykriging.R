
# Ordinary Kriging #################

# Make semivariogram
glitter <- gstat(formula = total ~ 1, data = sampledata)

vglitter <- variogram(glitter, boundaries = c(70, 125, 175, 300, 400, 500, 600, 800, 1000, 1500))
vglitter
plot(vglitter, plot.numbers = T)

vgmlitter <- vgm(nugget = 2, psill = 12, range = 350, model = 'Exp')
vgmlitter <- fit.variogram(vglitter, vgmlitter)

plot(vglitter, vgmlitter)
attr(vgmlitter, 'SSErr')

# Export semivariogram to png


# Perform Kriging (euclidian distance)
# cross-validation
litter_cv <- krige.cv(formula = total ~ 1, locations = sampledata, vgmlitter)
litter_cv$residual

# IDW: litter_cv <- krige.cv(formula = total ~ 1, locations = sampledata)

plot(litter_cv$residual)

# litter_cv_sp <- as(litter_cv, 'Spatial')
# bubble(litter_cv_sp_na, zcol = 'residual')

mean(litter_cv$zscore)
sd(litter_cv$zscore)


# get roadnetwork with buffer 
# roadnetwork <- raster('data/c02_osm_road_raster.tif')
# roadnetwork[roadnetwork == -9999] <- NA
# roadnetwork <- as(roadnetwork, 'SpatialGridDataFrame')
roadnetwork <- readOGR(dsn = 'data', layer = 'c03_osm_roads_buffer_Dissolve')

# roadnetwork <- spTransform(roadnetwork, CRS(proj4string(sampledata)))

# "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"



# ordinary kriging
litter_krig = krige(total ~ 1, locations = sampledata, newdata = area_raster, model = vgmlitter, nmax = 15)

spplot(litter_krig['var1.pred'])
spplot(litter_krig['var1.var'], sp.layout = list('sp.points', sampledata, col = 'black'))

# Clip Raster on buffered roads

litter_krig_rast <- raster(litter_krig)
final <- mask(litter_krig_rast, roadnetwork)

# Visualize 
spplot(final, zcol = 'var1.pred')

# Export to geotiff
writeRaster(final, 'output/litter_ordinarykriging', format = 'GTiff')