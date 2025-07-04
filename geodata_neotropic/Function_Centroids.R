

library(sf)
library(dplyr)

# Function to convert a dataframe into a spatial object with group centroids
process_centroid_data <- function(data, coord_cols) {
  # Convert input data.frame to sf object using specified coordinate columns
  sf_data <- st_as_sf(data, coords = coord_cols, crs = 4326)
  
  # Transform to metric projection (Web Mercator) for accurate spatial operations
  sf_proj <- st_transform(sf_data, crs = 3857)
  
  # Group by new.ID, combine geometries, and calculate the centroid
  centroids <- sf_proj %>%
    group_by(new.ID) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_centroid()
  
  # Transform centroids back to latitude/longitude
  centroids_latlon <- st_transform(centroids, crs = 4326)
  
  # Extract coordinates and convert back to a regular dataframe
  coords <- st_coordinates(centroids_latlon)
  centroid_df <- centroids_latlon %>%
    st_drop_geometry() %>%
    mutate(
      centroid_long = coords[, "X"],
      centroid_lat = coords[, "Y"]
    )
  
  # Merge with original data, keeping only one row per group (new.ID)
  result <- data %>%
    left_join(centroid_df, by = "new.ID") %>%
    arrange(new.ID) %>%
    group_by(new.ID) %>%
    slice_head(n = 1)
  
  return(result)
}


#### ejemplo de uso
#### 
#### 
new_atlasmxb <- read_csv("Data/new.atlasmxb.csv", 
                         locale = locale(encoding = "Latin1"))  # o prueba "UTF-8"!

new_atlasmxb_centroid2 <- process_centroid_data(new_atlasmxb, c("Longitud", "Latitud"))
new_dryf_centroid <- process_centroid_data(new_dryf, c("x", "y"))
new_long_centroid <- process_centroid_data(new_long, c("Longitude", "Latitude"))
