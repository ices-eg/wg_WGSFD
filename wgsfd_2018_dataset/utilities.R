
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
