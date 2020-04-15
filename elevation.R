library(raster)
library(tmap)

cont <- stack("Data/continents-stack.envi")
r_temp <- sum(cont, na.rm = TRUE)

el <- raster("Data/ETOPO1_Bed_c_geotiff.tif")
crs(el) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
el <- projectRaster(el, r_temp, method = "bilinear")

el[r_temp == 0] <- NA
el[el < 0] <- 0
plot(el)

writeRaster(el, "elevation.tif")

r <- el
th <- 2000
r[r >= th] <- th
r[r < th] <- 1
r[r != 1] <- 0

plot(r, col = rev(wesanderson::wes_palette("Royal1", 2, type = "discrete")),
         axes = FALSE, box = FALSE, main = "Elevation range")

r <- rasterToPolygons(r, fun = function(x) {x == 0})
r <- buffer(r, 1)

m <- tm_shape(el) +
  tm_raster(palette = viridis::viridis(100), title = "Elevation (m)") +
  tm_shape(r) +
  tm_borders(col = "red") +
  tm_layout(legend.position = c(0.1, -0.10), frame = FALSE)

tmap_save(m, "Figures/elevation.png", width = 10, height = 6)
