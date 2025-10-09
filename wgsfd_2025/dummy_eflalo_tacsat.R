# ============================================================
# Function: create_dummy_eflalo_tacsat()
# ============================================================
# Creates realistic linked dummy EFLALO (logbook) and TACSAT (VMS) datasets.
# Parameters:
#   n_vessels          - number of vessels
#   n_trips_per_vessel - trips per vessel
#   n_positions_per_trip - VMS points per trip
#   n_gears            - number of different gears to simulate
#   n_rectangles       - number of ICES rectangles to use
#   date_start         - start date for generating trips
# ============================================================


n_vessels = 1 
n_trips_per_vessel = 3 
n_positions_per_trip = 6 
n_gears = 1 
n_rectangles = 1 
date_start = as.Date("2024-06-01")

create_dummy_eflalo_tacsat = function(
    n_vessels = 1,
    n_trips_per_vessel = 3,
    n_positions_per_trip = 6,
    n_gears = 1,
    n_rectangles = 1,
    date_start = as.Date("2024-06-01")
) {
  # Dependencies
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
  library(dplyr)
  library(lubridate)
  
  set.seed(123)
  
  # ------------------------------------------------------------
  # Gear and ICES rectangles pools
  # ------------------------------------------------------------
  gear_type = c("OTB",  "TBB", "DRB")
  gear  = sample ( x =  gear_type , n_gears  ) 
  rects = c("31F2", "31F1", "32F3", "30E8", "33F2", "33F1")[1:n_rectangles]
  
  # ------------------------------------------------------------
  # Generate EFLALO
  # ------------------------------------------------------------
  eflalo = expand.grid(
    VE_REF = paste0("VESSEL_", seq_len(n_vessels)),
    TRIP_NO = seq_len(n_trips_per_vessel)
  ) %>%
    mutate(
      LE_ID = paste(VE_REF, TRIP_NO, sep = "_"),
      LE_CDAT = date_start + sample(0:60, n(), replace = TRUE),
      LE_GEAR = sample(gear_types, n(), replace = TRUE),
      LE_MET = paste0(LE_GEAR, "_DEF"),
      LE_RECT = sample(rects, n(), replace = TRUE),
      LE_LAT = runif(n(), 50, 56),
      LE_LON = runif(n(), -5, 3),
      LE_EFF = round(runif(n(), 5, 15), 1),
      LE_TON = round(runif(n(), 50, 500), 1),
      LE_SP = sample(c("COD", "HKE", "PLE", "NEP", "SOL"), n(), replace = TRUE),
      LE_KG = round(runif(n(), 20, 400), 1),
      LE_VALUE = round(LE_KG * runif(n(), 1.5, 3.5), 1)
    ) %>%
    arrange(VE_REF, LE_CDAT)
  
  # ------------------------------------------------------------
  # Generate TACSAT with consistent linkage
  # ------------------------------------------------------------
  tacsat = eflalo %>%
    group_by(VE_REF, LE_ID, LE_LAT, LE_LON, LE_CDAT) %>%
    do({
      n = n_positions_per_trip
      start_time = as.POSIXct(paste(.$LE_CDAT, sprintf("%02d:00:00", sample(0:23, 1))), tz = "UTC")
      
      tibble(
        VE_REF = .$VE_REF,
        LE_ID = .$LE_ID,
        SI_DATIM = start_time + minutes(seq(0, by = 120, length.out = n)),
        SI_LATI = .$LE_LAT + cumsum(rnorm(n, 0, 0.05)),
        SI_LONG = .$LE_LON + cumsum(rnorm(n, 0, 0.05)),
        SI_SPD = round(runif(n, 0, 12), 2),
        SI_HE = sample(seq(0, 350, 10), n, replace = TRUE)
      )
    }) %>%
    ungroup() %>%
    mutate(
      PING_DATE = as.Date(SI_DATIM),
      PING_TIME = format(SI_DATIM, "%H:%M:%S")
    )
  
  # ------------------------------------------------------------
  # Output
  # ------------------------------------------------------------
  return(list(EFLALO = eflalo, TACSAT = tacsat))
}



create_dummy_eflalo_tacsat(
  n_vessels = 1,
  n_trips_per_vessel = 3,
  n_positions_per_trip = 2,
  n_gears = 1,
  n_rectangles = 1,
  date_start = as.Date("2024-06-01")
)


