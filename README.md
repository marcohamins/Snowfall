# Snowfall by Season

Interactive Shiny app for **cumulative snowfall by winter season** (Oct–May) for multiple major US snowy cities, with historical comparison and optional statistical overlays. Choose a city from the dropdown to view that station’s data.

## Run locally

1. Install R dependencies (once):

   ```r
   source("BostonSnow/install.R")
   ```

2. (Optional) Refresh the data from NOAA (multi-city):

   ```bash
   cd BostonSnow && Rscript scripts/fetch_noaa_snow.R
   ```

   This writes `snow_multicity.csv` with Boston, Buffalo, Chicago, Cleveland, Denver, Detroit, Milwaukee, Minneapolis–St Paul, New York, Philadelphia, Pittsburgh, Raleigh, Salt Lake City, Seattle, and Washington D.C. For Boston-only data you can still run `scripts/fetch_noaa_boston.R` to get `USW00014739_2_24_25.csv`.

3. Start the app:

   ```bash
   Rscript BostonSnow/startApp.R
   ```

   Or from R: `shiny::runApp("BostonSnow")`. Open `http://localhost:80` (or the port shown).

## Free hosting (ShinyApps.io)

1. Create a free account at [shinyapps.io](https://www.shinyapps.io/).
2. Install the `rsconnect` package and authorize your account:

   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name = "YOUR_ACCOUNT", token = "YOUR_TOKEN", secret = "YOUR_SECRET")
   ```

3. Push this repo to GitHub (including the CSV and the workflow below).
4. Deploy the app and point it at the multi-city CSV in this repo so it stays up to date:
   - In the ShinyApps.io dashboard, open your app → **Settings** → **Environment variables**.
   - Add: `SNOW_DATA_URL` = `https://raw.githubusercontent.com/marcohamins/Snowfall/main/BostonSnow/snow_multicity.csv`
5. Deploy from R (from the repo root):

   ```r
   rsconnect::deployApp("BostonSnow", appName = "boston-snowfall")
   ```

The app will read the CSV from that URL. When the GitHub Action updates the file (see below), the next app load will show the new data.

## Updating the data (daily/weekly)

Data is refreshed automatically by GitHub Actions:

- **Schedule:** every **Sunday at 12:00 UTC** (see `.github/workflows/update-snow-data.yml`).
- **Manual run:** Actions tab → “Update snow data” → “Run workflow”.
- The workflow runs `BostonSnow/scripts/fetch_noaa_snow.R`, which pulls GHCN daily data for multiple US cities from NOAA and overwrites `BostonSnow/snow_multicity.csv`. If the file changed, it is committed and pushed.

No redeploy of the Shiny app is needed: the app uses `SNOW_DATA_URL`, so updated data is used on next load.

## Data source

- [NOAA GHCN Daily](https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily) — stations for Boston, Buffalo, Chicago, Cleveland, Denver, Detroit, Milwaukee, Minneapolis–St Paul, New York (Central Park), Philadelphia, Pittsburgh, Raleigh, Salt Lake City, Seattle, and Washington D.C. (Dulles).
