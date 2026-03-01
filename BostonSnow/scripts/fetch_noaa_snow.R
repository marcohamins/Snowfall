#!/usr/bin/env Rscript
# Fetch GHCN daily data for multiple US snowy cities and write one multi-city CSV
# for the Shiny app. Run weekly (e.g. via GitHub Actions). Uses direct .dly download/parse.

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

NOAA_BASE <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/all"

# City list: display name (for dropdown), GHCN station ID, full name, lat, lon, elev (m)
CITIES <- tibble(
  city = c(
    "Boston, MA",
    "Buffalo, NY",
    "Baltimore, MD",
    "Burlington, VT",
    "Chicago, IL",
    "Cleveland, OH",
    "Denver, CO",
    "Detroit, MI",
    "Milwaukee, WI",
    "Minneapolis-St Paul, MN",
    "New York, NY",
    "Philadelphia, PA",
    "Pittsburgh, PA",
    "Raleigh, NC",
    "Salt Lake City, UT",
    "Seattle, WA",
    "Washington, D.C.",
    "Jackson, WY",
    "Aspen, CO",
    "Boise, ID"
  ),
  station_id = c(
    "USW00014739",
    "USW00014733",
    "USW00093721",
    "USW00014742",
    "USW00094846",
    "USW00014820",
    "USC00050848",
    "USW00094847",
    "USW00014839",
    "USW00014922",
    "USW00094728",
    "USW00013739",
    "USW00094823",
    "USW00013722",
    "USW00024127",
    "USW00024233",
    "USW00093738",
    "USC00484910",
    "USC00050372",
    "USW00024131"
  ),
  name = c(
    "BOSTON LOGAN INTERNATIONAL AIRPORT, MA US",
    "BUFFALO NIAGARA INTERNATIONAL AIRPORT, NY US",
    "BALTIMORE WASHINGTON INTL AP, MD US",
    "BURLINGTON INTL AIRPORT, VT US",
    "CHICAGO O'HARE INTERNATIONAL AIRPORT, IL US",
    "CLEVELAND HOPKINS INTERNATIONAL AIRPORT, OH US",
    "BOULDER 2 SW CO US",
    "DETROIT METROPOLITAN WAYNE COUNTY AIRPORT, MI US",
    "MILWAUKEE MITCHELL INTERNATIONAL AIRPORT, WI US",
    "MINNEAPOLIS ST PAUL INTERNATIONAL AIRPORT, MN US",
    "CENTRAL PARK OBSERVATORY, NY US",
    "PHILADELPHIA INTERNATIONAL AIRPORT, PA US",
    "PITTSBURGH INTERNATIONAL AIRPORT, PA US",
    "RALEIGH DURHAM INTERNATIONAL AIRPORT, NC US",
    "SALT LAKE CITY INTERNATIONAL AIRPORT, UT US",
    "SEATTLE TACOMA INTERNATIONAL AIRPORT, WA US",
    "WASHINGTON DULLES INTERNATIONAL AIRPORT, VA US",
    "JACKSON 2 E, WY US",
    "ASPEN 1 SW CO US",
    "BOISE AIR TERMINAL, ID US"
  ),
  latitude = c(42.36057, 42.9405, 39.17329, 44.46825, 41.9786, 41.4050, 40.0167, 42.2125, 42.95489, 44.8825, 40.7794, 39.8729, 40.48459, 35.8776, 40.77069, 47.44467, 38.93485, 43.4864, 39.19, 43.5667),
  longitude = c(-71.00975, -78.7322, -76.68408, -73.1499, -87.9047, -82.6556, -105.2667, -83.3534, -87.9045, -93.2218, -73.9632, -75.2414, -80.21448, -78.7875, -111.96503, -122.31361, -77.45581, -110.7617, -106.82, -116.2406),
  elevation = c(3.2, 219, 42, 101.1, 191, 241, 1673, 190, 203, 256, 40, 9, 341, 134, 1288, 112, 90, 1893, 2455, 860.5)
)

# Output path: first arg or current directory
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) > 0) args[1] else getwd()
out_file <- file.path(out_dir, "BostonSnow/snow_multicity.csv")

parse_dly_line <- function(line) {
  id <- trimws(substr(line, 1, 11))
  yr <- as.integer(substr(line, 12, 15))
  mo <- as.integer(substr(line, 16, 17))
  elem <- trimws(substr(line, 18, 21))
  out <- data.frame(
    id = id,
    year = yr,
    month = mo,
    element = elem,
    day = 1:31,
    value = NA_integer_
  )
  for (d in 1:31) {
    start <- 21L + (d - 1) * 8 + 1
    end <- start + 4
    v <- substr(line, start, end)
    out$value[d] <- suppressWarnings(as.integer(trimws(v)))
  }
  out
}

fetch_one_station <- function(station_id, station_name, lat, lon, elev, city_label) {
  dly_url <- paste0(NOAA_BASE, "/", station_id, ".dly")
  tmp <- tempfile(fileext = ".dly")
  ok <- suppressWarnings(
    tryCatch(
      download.file(dly_url, tmp, quiet = TRUE, mode = "wb"),
      error = function(e) NA
    )
  )
  if (!identical(ok, 0L)) {
    warning("Failed to download ", dly_url, " for ", city_label)
    return(NULL)
  }
  lines <- readLines(tmp, warn = FALSE)
  unlink(tmp)
  if (length(lines) == 0) return(NULL)

  raw_list <- lapply(lines, function(l) {
    tryCatch(parse_dly_line(l), error = function(e) NULL)
  })
  raw <- bind_rows(raw_list[!sapply(raw_list, is.null)])
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  elements <- raw %>%
    filter(element %in% c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")) %>%
    filter(!is.na(value), value > -9999, value < 9999) %>%
    mutate(date = make_date(year, month, day)) %>%
    filter(!is.na(date)) %>%
    select(id, date, element, value) %>%
    pivot_wider(names_from = element, values_from = value, values_fill = NA)

  date_range <- seq(min(elements$date, na.rm = TRUE), max(elements$date, na.rm = TRUE), by = "day")
  wide <- data.frame(DATE = date_range) %>%
    left_join(elements %>% rename(DATE = date) %>% select(-id), by = "DATE")
  # Some stations lack SNOW or other elements; ensure all required columns exist
  for (col in c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  wide <- wide %>%
    mutate(
      STATION = station_id,
      LATITUDE = lat,
      LONGITUDE = lon,
      ELEVATION = elev,
      NAME = station_name,
      CITY = city_label,
      PRCP = ifelse(is.na(PRCP), 0, PRCP),
      SNOW = ifelse(is.na(SNOW), 0, SNOW),
      SNWD = ifelse(is.na(SNWD), 0, SNWD)
    ) %>%
    select(STATION, DATE, LATITUDE, LONGITUDE, ELEVATION, NAME, CITY, PRCP, SNOW, SNWD, TMAX, TMIN)
  wide
}

# Loop over cities, fetch and bind
all_data <- list()
for (i in seq_len(nrow(CITIES))) {
  row <- CITIES[i, ]
  message("Fetching ", row$city, " (", row$station_id, ") ...")
  one <- fetch_one_station(
    row$station_id,
    row$name,
    row$latitude,
    row$longitude,
    row$elevation,
    row$city
  )
  if (!is.null(one)) {
    all_data[[length(all_data) + 1]] <- one
  }
}

if (length(all_data) == 0) stop("No station data could be fetched.")

combined <- bind_rows(all_data)
write_csv(combined, out_file, na = "")
message("Wrote ", nrow(combined), " rows (", length(all_data), " cities) to ", out_file)
