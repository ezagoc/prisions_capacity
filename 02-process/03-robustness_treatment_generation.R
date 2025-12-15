###################################################
## Data Processing: capacity treatment generation
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, sf)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Paths:
# In
pf <- '../../data/00-map/'

# Out
pfo <- '../../data/03-analysis/'
# Datasets used across all years

# Municipalities shapefile
set.seed(200)
mp <- read_sf('../../data/00-map/shapefiles2/Muni_2012gw.shp') |>
  select(-c(OID_1:cov_id))

random_points <- map(mp$geometry, ~ st_sample(.x, 
                                              size = 1, 
                                              type = "random")[[1]])

mp$mun_random <- st_sfc(random_points, crs = st_crs(mp))

mp <- mp |> mutate(
  center_long = st_coordinates(st_centroid(mp$geometry))[, 1],
  center_lat = st_coordinates(st_centroid(mp$geometry))[, 2],
  rand_long = st_coordinates(mun_random)[, 1], 
  rand_lat = st_coordinates(mun_random)[, 2]
)

mp_p <- mp |> st_set_geometry(NULL) |> 
  select(CVE_ENT:NOM_MUN, center_long:center_lat)

months_seq <- tibble(
  month = c(1:12)
)

mp_point <- mp_p |> crossing(months_seq) |> 
  mutate(bimonth = case_when(month %in% c(1, 2) ~ 2, 
                             month %in% c(3, 4) ~ 4, 
                             month %in% c(5, 6) ~ 6, 
                             month %in% c(7, 8) ~ 8, 
                             month %in% c(9, 10) ~ 10,
                             month %in% c(11, 12) ~ 12)) |> 
  mutate(code_inegi = as.numeric(CVE_ENT)*1000 + as.numeric(CVE_MUN))

# Federal prisons and state regions

fed <- readxl::read_excel(paste0(pf, 'prisiones_federales.xlsx')) |> 
  select(name, date_opening, latitude, longitude, capacity, private, closed) |>
  mutate(date_opening = as.Date(date_opening))

reg <- readxl::read_excel(paste0(pf, 'catalogo_entidades.xlsx'))

fed <- fed |> st_as_sf(coords = c("longitude", "latitude"), 
                       crs = st_crs(mp))

fed <-  st_join(fed, mp)

fed <- fed |> left_join(reg) |> arrange(date_opening)

fed_final <- fed |> 
  filter(between(date_opening, as.Date(paste0('31-12-1999'), format = "%d-%m-%Y"),
                 as.Date(paste0('31-12-2014'), format = "%d-%m-%Y"))) |> 
  arrange(date_opening) 

fed2 <- fed_final |> st_set_geometry(NULL)

# Prisons 

prisd <- read_parquet(paste0(pfo, 
                             'individual_prisons_municipalities.parquet.gzip'))

prisp <- read_parquet(paste0(pfo, 
                             'panel_capacity.parquet.gzip'))

# Generate the full panel

years_seq <- tibble(
  year = c(2000:2012)
)

mp_point_full <- mp_point |> crossing(years_seq) |> arrange(year, month) |>
  mutate(date_panel = as.Date(paste0(year, '-', month, '-27'))) |>
  st_as_sf(coords = c("center_long", "center_lat"), crs = st_crs(mp))

# Now for the full panel generate the minimum distance to a federal prison center

fed <- st_transform(fed, 6372)
fed_final <- st_transform(fed_final, 6372)
mp_point_full <- st_transform(mp_point_full, 6372)

fed <- fed |> filter(name != 'CEFERESO 1 ALTIPLANO') # Only send gang members
results <- list()
dates <- unique(fed_final$date_opening)
dates <- c(as.Date('1999-09-01'), dates, as.Date('2013-01-01'))

for (i in 1:14) {
  initial_date <- dates[i]
  final_date <- dates[i+1]
  
  fed_filter <- fed |> filter(date_opening < final_date)
  state_filter <- mp_point_full |> filter(date_panel >= initial_date & 
                                            date_panel < final_date)
  
  # Compute distance matrix
  dist_mat <- st_distance(state_filter, fed_filter)
  
  # Get index of minimum distance and the corresponding value
  min_index <- apply(dist_mat, 1, which.min)
  min_dist <- apply(dist_mat, 1, min)
  
  # Get names of the closest federal prisons
  closest_fed_names <- fed_filter$name[min_index]  # replace 'prison_name' with the actual variable
  
  # Store results
  state_filter$min_dist_to_fed <- min_dist
  state_filter$closest_fed_prison <- closest_fed_names
  
  results[[i]] <- state_filter
}

mp_point_full <- bind_rows(results) |> 
  mutate(min_dist_to_fed_km = min_dist_to_fed / 1000)

### Now without Occidente and Altiplano: 

results <- list()
dates <- unique(fed_final$date_opening)
dates <- c(as.Date('1999-09-01'), dates, as.Date('2013-01-01'))

for (i in 1:14) {
  initial_date <- dates[i]
  final_date <- dates[i+1]
  
  fed_filter <- fed_final |> filter(date_opening < final_date)
  state_filter <- mp_point_full |> filter(date_panel >= initial_date & 
                                            date_panel < final_date)
  
  if (nrow(state_filter) == 0) next
  
  if (nrow(fed_filter) == 0) {
    state_filter$min_dist_to_fed2 <- NA_real_
    state_filter$closest_fed_prison2 <- NA_character_
  } else {
    dist_mat <- st_distance(state_filter, fed_filter)
    min_index <- apply(dist_mat, 1, which.min)
    min_dist <- apply(dist_mat, 1, min)
    
    state_filter$min_dist_to_fed2 <- min_dist
    state_filter$closest_fed_prison2 <- fed_filter$name[min_index]  # replace as needed
  }
  
  results[[i]] <- state_filter
}

mp_point_full <- bind_rows(results) |> 
  mutate(min_dist_to_fed_km2 = min_dist_to_fed2 / 1000)

cutoffs <- seq(250, 350, by = 1)

mpfinal <- mp_point_full %>%
  mutate(across(
    .cols = min_dist_to_fed_km2,
    .fns = list(!!!setNames(
      lapply(cutoffs, function(c) {
        function(x) ifelse(x < c & is.na(x) == F, 1, 0)
      }),
      paste0("btreat_", cutoffs, "KM2")
    )),
    .names = "{fn}"
  ))

mpfinal <- mpfinal |> arrange(code_inegi, year, month)

mpfinal <- mpfinal |> select(code_inegi, year, month, min_dist_to_fed_km2, 
                             starts_with('btreat_'))

mpfinal <- mpfinal |> st_set_geometry(NULL)

write_parquet(mpfinal, paste0(pfo, 'treatment_judicial_robustness.parquet.gzip'), 
              compression = 'gzip')
