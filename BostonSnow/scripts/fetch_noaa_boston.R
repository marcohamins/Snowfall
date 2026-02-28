#!/usr/bin/env Rscript
# Fetch Boston Logan (USW00014739) GHCN daily data from NOAA and write CSV
# for use by the Shiny app. Run daily or weekly (e.g. via GitHub Actions).
# Uses direct download and .dly parsing (no rnoaa dependency).

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

STATION_ID <- "USW00014739"
STATION_NAME <- "BOSTON LOGAN INTERNATIONAL AIRPORT, MA US"
LATITUDE <- 42.36057
LONGITUDE <- -71.00975
ELEVATION <- 3.2
NOAA_BASE <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/all"

# Output path: pass as first arg, else current working directory (run from BostonSnow/ or set in GHA)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) > 0) args[1] else getwd()
out_file <- file.path(out_dir, "USW00014739_2_24_25.csv")

# Download .dly file (fixed-width: ID 1-11, YEAR 12-15, MONTH 16-17, ELEMENT 18-21, then 31 x (VALUE 5 chars + 3 flags))
dly_url <- paste0(NOAA_BASE, "/", STATION_ID, ".dly")
tmp <- tempfile(fileext = ".dly")
message("Fetching ", dly_url, " ...")
ok <- suppressWarnings(
  tryCatch(
    download.file(dly_url, tmp, quiet = TRUE, mode = "wb"),
    error = function(e) NA
  )
)
if (!identical(ok, 0L)) {
  stop("Failed to download ", dly_url)
}

lines <- readLines(tmp, warn = FALSE)
unlink(tmp)
if (length(lines) == 0) stop("Empty file received from NOAA.")

# Parse fixed-width: each line = one month of one element; VALUE1 at 22-26, VALUE2 at 30-34, ... VALUE31 at 262-266
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

raw_list <- lapply(lines, function(l) {
  tryCatch(parse_dly_line(l), error = function(e) NULL)
})
raw <- bind_rows(raw_list[!sapply(raw_list, is.null)])
if (nrow(raw) == 0) stop("No valid records parsed from .dly file.")

# Keep only needed elements; treat missing/sentinel values as NA
elements <- raw %>%
  filter(element %in% c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")) %>%
  filter(!is.na(value), value > -9999, value < 9999) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(!is.na(date)) %>%
  select(id, date, element, value) %>%
  pivot_wider(names_from = element, values_from = value, values_fill = NA)

# Full date range and wide table for app (one row per date)
date_range <- seq(min(elements$date, na.rm = TRUE), max(elements$date, na.rm = TRUE), by = "day")
wide <- data.frame(DATE = date_range) %>%
  left_join(elements %>% rename(DATE = date) %>% select(-id), by = "DATE") %>%
  mutate(
    STATION = STATION_ID,
    LATITUDE = LATITUDE,
    LONGITUDE = LONGITUDE,
    ELEVATION = ELEVATION,
    NAME = STATION_NAME,
    PRCP = ifelse(is.na(PRCP), 0, PRCP),
    SNOW = ifelse(is.na(SNOW), 0, SNOW),
    SNWD = ifelse(is.na(SNWD), 0, SNWD)
  ) %>%
  select(STATION, DATE, LATITUDE, LONGITUDE, ELEVATION, NAME, PRCP, SNOW, SNWD, TMAX, TMIN)

write_csv(wide, out_file, na = "")
message("Wrote ", nrow(wide), " rows to ", out_file)
