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

mp_point_full <- mp_point_full |> 
  mutate(btreat_100KM2 = ifelse(min_dist_to_fed_km2<100 &
                                  is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_200KM2 = ifelse(min_dist_to_fed_km2<200 & 
                                  is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_250KM2 = ifelse(min_dist_to_fed_km2<250 & 
                                  is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_300KM2 = ifelse(min_dist_to_fed_km2<300 & 
                                  is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_400KM2 = ifelse(min_dist_to_fed_km2<400 & 
                                  is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_500KM2 = ifelse(min_dist_to_fed_km2<500 & 
                                  is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_750KM2 = ifelse(min_dist_to_fed_km2<750 & 
                                  is.na(min_dist_to_fed_km2) == F, 1, 0),
         btreat_100KM = ifelse(min_dist_to_fed_km2<100 & 
                                 is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_200KM = ifelse(min_dist_to_fed_km<200 & 
                                 is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_300KM = ifelse(min_dist_to_fed_km<300 & 
                                 is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_400KM = ifelse(min_dist_to_fed_km<400 & 
                                 is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_500KM = ifelse(min_dist_to_fed_km<500 & 
                                 is.na(min_dist_to_fed_km2) == F, 1, 0), 
         btreat_750KM = ifelse(min_dist_to_fed_km<750 & 
                                 is.na(min_dist_to_fed_km2) == F, 1, 0),
         ctreat_200KM = ifelse(min_dist_to_fed_km<200, 
                               min_dist_to_fed_km, 0), 
         ctreat_250KM = ifelse(min_dist_to_fed_km<250, 
                               min_dist_to_fed_km, 0), 
         ctreat_300KM = ifelse(min_dist_to_fed_km<300, 
                               min_dist_to_fed_km, 0), 
         ctreat_400KM = ifelse(min_dist_to_fed_km<400, 
                               min_dist_to_fed_km, 0), 
         ctreat_500KM = ifelse(min_dist_to_fed_km<500, 
                               min_dist_to_fed_km, 0), 
         ctreat_750KM = ifelse(min_dist_to_fed_km<750, 
                               min_dist_to_fed_km, 0),
         ctreat_200KM2 = ifelse(min_dist_to_fed_km2<200 & is.na(min_dist_to_fed_km2) == F, 
                               min_dist_to_fed_km2, 0), 
         ctreat_250KM2 = ifelse(min_dist_to_fed_km2<250 & is.na(min_dist_to_fed_km2) == F, 
                                min_dist_to_fed_km2, 0), 
         ctreat_300KM2 = ifelse(min_dist_to_fed_km2<300 & is.na(min_dist_to_fed_km2) == F, 
                               min_dist_to_fed_km2, 0), 
         ctreat_400KM2= ifelse(min_dist_to_fed_km2<400 & is.na(min_dist_to_fed_km2) == F, 
                               min_dist_to_fed_km2, 0), 
         ctreat_500KM2 = ifelse(min_dist_to_fed_km2<500 & is.na(min_dist_to_fed_km2) == F, 
                               min_dist_to_fed_km2, 0), 
         ctreat_750KM2 = ifelse(min_dist_to_fed_km2<750 & is.na(min_dist_to_fed_km2) == F, 
                               min_dist_to_fed_km2, 0))

# Now, using the capacity panel, lets merge with all the prison information available

prisd <- prisd |> 
  mutate(femenil =  ifelse(grepl("femenil", center_name, 
                                 ignore.case = TRUE), 1, 0))

prisp <- prisp |> mutate(year_month = paste0(year, '-', month))

# First only removing female centers
prisp_no_fem <- prisp |> left_join(prisd |> select(prison_id, femenil, 
                                                   lat_manual, long_manual)) |>
  filter(femenil == 0)

# Now build the loop to get the treatment
mp_p <- mp_p |> st_as_sf(coords = c("center_long", "center_lat"), 
                         crs = st_crs(mp))

mp_p <- st_transform(mp_p, 6372)

panel_final <- tibble()
for (y in 2000:2012){
  panel_int <- tibble()
  print(y)
  for (m in seq(2, 12, by = 2)){
    pristem <- prisp_no_fem |> filter(year == y & month == m) |>
      select(prison_id, lat_manual, long_manual, center_name_clean, 
             total_clean:perc_federal, min_dist_to_fed:ctreat_750KM, 
             treat_zone:federal_25p)
    
    pristem_geo <- pristem |> st_as_sf(coords = c("long_manual", "lat_manual"), 
                                   crs = st_crs(mp)) |>
      filter(is.na(capacity_clean) == F & is.na(total_clean) == F)
    pristem_geo <- st_transform(pristem_geo, 6372)
    
    mp_final <- mp_p |> st_set_geometry(NULL)
    
    # Compute distance matrix
    dist_mat <- st_distance(mp_p, pristem_geo)
    
    # Get index of minimum distance and the corresponding value
    min_index <- apply(dist_mat, 1, which.min)
    min_dist <- apply(dist_mat, 1, min)
    
    # Get names of the closest federal prisons
    closest_state_names <- pristem_geo$prison_id[min_index]  # replace 'prison_name' with the actual variable
    
    # Store results
    mp_final$min_dist_to_state <- min_dist
    mp_final$prison_id <- closest_state_names
    
    mp_final <- mp_final |> mutate(year = y, month = m) |> 
      left_join(pristem, by = c('prison_id'))
    
    panel_int <- rbind(panel_int, mp_final)
    
  }
  
  panel_final <- rbind(panel_final, panel_int)
}

panel_final <- panel_final |> 
  mutate(code_inegi = as.numeric(CVE_ENT)*1000 + as.numeric(CVE_MUN), 
         min_dist_to_state_km = min_dist_to_state / 1000) |>
  select(-c(CVE_ENT:NOM_MUN))

colnames(panel_final)[7:46] <- paste0(colnames(panel_final)[7:46], '_state')

final <- mp_point_full |> st_set_geometry(NULL) |> 
  left_join(panel_final, by = c('code_inegi', 'year', 'bimonth' = 'month'))

write_parquet(final, paste0(pfo, 'treatment_judicial.parquet.gzip'), 
              compression = 'gzip')
