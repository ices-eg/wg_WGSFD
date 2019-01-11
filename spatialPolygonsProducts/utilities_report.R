

makeRaster <- function(data, value, resolution = 0.05, by = NULL, fun = sum) {
  # return NULL if data is empty
  if (nrow(data) == 0) return(NULL)

  # make a raster of the data
  loc <- as.matrix(data[c("Lon", "Lat")])
  colnames(loc) <- c('X', 'Y')

  # set up an 'empty' raster, here via an extent object derived from your data
  r <- raster::raster(raster::extent(loc) + resolution,
                      resolution = resolution,
                      crs = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  if (!is.null(by)) {
    byvalue <- unique(data[[by]])
    x <- lapply(byvalue, function(byval) {
           raster::rasterize(loc, r, data[data[[by]] == byval, value], fun = fun)
         })
    Reduce("+", x) / length(x)
  } else
  {
    raster::rasterize(loc, r, data[[value]], fun = fun)
  }
}


plotRaster <- function(x, region, land, palette, breaks, main = "") {
  # defaults:
  if (missing(palette)) palette <- RColorBrewer::brewer.pal(9, "YlOrRd")

  # derived quantities
  col <- colorRampPalette(palette)(length(breaks)-1)
  if (length(col) == 0) col <- 1

  # define bounding box based on ecoregion
  bbox <- as(raster::extent(region), "SpatialPolygons")
  sp::proj4string(bbox) <- sp::proj4string(region)

  # make base plot of land and ecoregion outline
  sp::plot(region, col = "#FF000001", lwd = .5, axes = FALSE)
  sp::degAxis(1, mgp = c(2, 0.5, 0))
  sp::degAxis(2, mgp = c(2, 0.75, 0), las = 1)
  box(bty = "o")
  sp::plot(rgeos::gIntersection(land, bbox), col = "#c9c0b6FF", border = NA, add = TRUE)
  
  # plot raster
  if (!is.null(x))
    raster::plot(x, col = col, breaks = breaks, add = TRUE, legend = FALSE)

  # add title
  mtext(main, outer = F, cex = 1, line = 0.5)
}


plotLegend <- function(breaks, maxvalue = max(breaks), title = "", palette, digits) {

  # defaults:
  if (missing(palette)) palette <- RColorBrewer::brewer.pal(9, "YlOrRd")
  if (missing(digits)) {
    # how small is the smallest number (in magnitude)
    decimals <- -1 * min(log(abs(breaks)[breaks != 0], 10))
    # set digits to be the 1st sig fig of the smallest if smallest < 1, otherwise 0
    digits <- pmax(0, decimals)
  }

  # derived quantities
  col <- colorRampPalette(palette)(length(breaks)-1)

  # set up breaks for legend
  breaks <- c(breaks[breaks < maxvalue], maxvalue)
  col <- col[1:(length(breaks)-1)]

  ncol <- length(breaks) - 1
  fbreaks <- formatC(breaks, format = "f", digits = digits)
  labels <- paste0(fbreaks[(ncol):1], " - ", fbreaks[(ncol + 1):2])
  labels[length(labels)] <- "missing"

  par(fig = c(0, 1, 0, 1), oma = c(0.2, 0.2, 0.2, 0.2), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')

  legend("topright", y = NULL,
         legend = labels,
         title = title,
         col = col[ncol:1], pch = 15, pt.cex = 2.5,
         bg = "white", inset = c(0, 0), xpd = TRUE)
}




# Ecosystem overview plots ---------


plotEOPages <- function(data, ivalues, years, breaks, palette, res, main, area_list, digits, save = TRUE) {

  # default settings:
  if (missing(palette)) palette <- RColorBrewer::brewer.pal(9, "YlOrRd")
  if (missing(res)) res <- 0.05
  if (missing(digits)) {
    # how small is the smallest number (in magnitude)
    decimals <- -1 * min(log(abs(breaks)[breaks != 0], 10))
    # set digits to be the 1st sig fig of the smallest if smallest < 1, otherwise 0
    digits <- pmax(0, decimals)
  }

  # derived quantities
  col <- colorRampPalette(palette)(length(breaks)-1)

  # do pair of plots
    par(mfrow = c(1,2), oma = c(1,1,1,7.5), mar = c(2,2,2,2))

    pdata <- data[data$Year %in% years &
                  data$Ecoregion == iarea, ]
    maxvalue <- 0
    for (i in 1:2) {
      rast <- makeRaster(pdata, value = ivalues[i], resolution = res)
      plotRaster(rast,
                 ecoregion = ices_eco[ices_eco$Ecoregion == iarea,], land = coast,
                 main = legendtitle[i],
                 col = col, breaks = breaks)
      maxvalue <- max(maxvalue, max(rast[], na.rm = TRUE))
    }

    # set up breaks for legend
    ibreaks <- c(breaks[breaks < maxvalue], maxvalue)
    icol <- col[1:(length(ibreaks)-1)]

    # do legend
    plotLegend(ibreaks, icol, "", digits)

    if (save) dev.off()
}



makesf <- function(c_square, values, resolution = 0.05) {
  values %>%
    cbind.data.frame(., vmstools::CSquare2LonLat(c_square,  0.05)) %>%
    rename(
      Longitude = SI_LONG,
      Latitude = SI_LATI
    ) %>%
    mutate(
      wkt = paste0('POLYGON((',
                   Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude + 0.025, '))')
    ) %>%
    st_as_sf(crs = 4326, wkt = "wkt")
}
