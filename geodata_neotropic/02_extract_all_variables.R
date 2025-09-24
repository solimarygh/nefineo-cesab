# 02_extract_all_variables.R
# One-stop extraction: Morrone + WWF + Climenv + Holdridge.
# Leaflet diagnostic maps are commented by default.


#### PackageS ####
#### 
#install.packages("devtools")      # o remotes: install.packages("remotes")
#install.packages("SPEI", dependencies = TRUE)


suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(stringr)
  library(purrr)
  library(tibble)
  library(SPEI)      # Thornthwaite PET
  library(leaflet) # only if you re-enable maps
  library(RColorBrewer) # only if you re-enable maps
  
  # optional:  
  # install.packages("devtools")   
  # devtools::install_github("jamestsakalos/climenv", upgrade = "never", dependencies = TRUE)
  # install.packages("SPEI", dependencies = TRUE)
  # library(devtools) 
  
  library(climenv) 
  library (SPEI)
})



                 # o library(remotes)


library(climenv)
####  Paths #####
# all relative to this project!
# 

path_in_csv  <- "data/WP2_dataset.csv"
path_nc_rds  <- "geodata_neotropic/morrone_shapefile_improved_sf.rds"   # created by 01_build_morrone_sf.R
path_wwf_shp <- "geodata_neotropic/data_WWF/wwf_terr_ecos.shp"

cache_morrone <- "geodata_neotropic/tmp_WP2_dataset_morrone.rds"
cache_wwf     <- "geodata_neotropic/tmp_WP2_dataset_wwf.rds"
cache_clim    <- "geodata_neotropic/tmp_WP2_dataset_climenv.rds"

out_csv <- "geodata_neotropic/WP2_dataset_allvars.csv"

#### Helpers ####
check_key <- function(df, key = "new.ID_sample") {
  cat("\n-- KEY CHECK:", deparse(substitute(df)), "--\n")
  cat("rows:", nrow(df), " distinct(", key, "): ", dplyr::n_distinct(df[[key]]), "\n", sep="")
  dups <- df %>% count(.data[[key]]) %>% filter(n > 1)
  if (nrow(dups) > 0) { 
    cat("⚠️ dups:", nrow(dups), "\n"); print(head(dups, 10)) 
  } else cat("✅ unique\n")
}

ensure_distinct_by <- function(df, key = "new.ID_sample") {
  df %>% distinct(across(all_of(key)), .keep_all = TRUE)
}

month_levels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
to_num <- function(x) as.numeric(if (is.list(x)) unlist(x) else x)


##### Load base table and ensure new.ID_sample ####

stopifnot(file.exists(path_in_csv))
df0 <- read_csv(path_in_csv, show_col_types = FALSE)

## creando um identificar por cada linea para no confundir los datos!
df0 <- df0 %>%
  group_by(new.ID) %>%
  mutate(fila = row_number()) %>%
  ungroup() %>%
  mutate(new.ID_sample = paste0(new.ID, "_", fila))
#Así, cada réplica de new.ID obtiene un sufijo incremental (_1, _2, _3), y nunca se confunde con los números de fila globales.

check_key(df0, "new.ID_sample")

stopifnot(all(c("longitude","latitude") %in% names(df0)))


#Vamos a eliminar el  NEF_AtlasMXB_B89_2025_1 que es el punto de mexico fuera del neotropico!!
df0 <- df0 %>% filter(new.ID_sample != "NEF_AtlasMXB_B89_2025_1")
df0 %>% filter(new.ID_sample == "NEF_AtlasMXB_B89_2025_1") %>% nrow() > 0



#### 1) MORRONE (point-in-polygon) + manual fixes (by new.ID) ####

stopifnot(file.exists(path_nc_rds))
nc_small <- readRDS(path_nc_rds)

# Intersections (planar)
pts_sf <- st_as_sf(df0, coords = c("longitude","latitude"), crs = 4326)
nc_pl  <- st_transform(nc_small, 2163) # Its non-deprecated replacement EPSG:9311 will be used instead. 
pts_pl <- st_transform(pts_sf,    2163) # Its non-deprecated replacement EPSG:9311 will be used instead. 

if (!file.exists(cache_morrone)) {
  message("• Morrone intersections...")
  idx_list <- lapply(seq_len(nrow(pts_pl)), function(i) which(st_intersects(pts_pl[i,], nc_pl, sparse = FALSE)))
  morrone_df <- purrr::imap_dfr(idx_list, function(ix, i) {
    if (length(ix) > 0) {
      cbind(data.frame(new.ID_sample = df0$new.ID_sample[i]), st_drop_geometry(nc_pl[ix,]))
    } else {
      empty_row <- as.list(rep(NA, ncol(nc_pl))); names(empty_row) <- colnames(nc_pl)
      data.frame(new.ID_sample = df0$new.ID_sample[i], as.data.frame(empty_row))
    }
  })
  saveRDS(morrone_df, cache_morrone)
} else {
  morrone_df <- readRDS(cache_morrone)
}

# Keep only relevant columns and prefix as morrone_biogeoregions_
morro_tmp <- morrone_df %>%
  select(new.ID_sample, IDProv, Region, Subregion, Provincias, Dominio, SubDominio) %>%
  rename_with(~ paste0("morrone_biogeoregions2_", .x), -new.ID_sample)

out1 <- df0 %>% left_join(ensure_distinct_by(morro_tmp), by = "new.ID_sample")

out1_clean <- out1 %>%
  select(-starts_with("morrone_biogeoregions_")) %>%
  rename_with(~ str_replace(.x, "^morrone_biogeoregions2_", "morrone_biogeoregions_"),
              starts_with("morrone_biogeoregions2_"))

# Lookup from official shape for expanding IDProv → (stable)
morrone_cols <- c(
  "morrone_biogeoregions_IDProv",
  "morrone_biogeoregions_Region",
  "morrone_biogeoregions_Subregion",
  "morrone_biogeoregions_Provincias",
  "morrone_biogeoregions_Dominio",
  "morrone_biogeoregions_SubDominio"
)

morrone_lookup <- nc_small %>%
  st_drop_geometry() %>%
  transmute(
    morrone_biogeoregions_IDProv    = as.integer(IDProv),
    morrone_biogeoregions_Region    = Region,
    morrone_biogeoregions_Subregion = Subregion,
    morrone_biogeoregions_Provincias= Provincias,
    morrone_biogeoregions_Dominio   = Dominio,
    morrone_biogeoregions_SubDominio= SubDominio
  ) %>%
  distinct()




#####  (Optional) Morrone leaflet diagnostic map 
#  
# # Uncomment to visualize points that still lack Morrone province
# faltantes_morrone <- out1_clean  %>% filter(is.na(morrone_biogeoregions_IDProv))
# library(leaflet); library(RColorBrewer)
# id_levels <- sort(unique(nc_small$IDProv))
# pal_morr  <- colorFactor(palette = colorRampPalette(brewer.pal(12,"Set3"))(length(id_levels)),
#                          domain = id_levels)
# nc_labels <- nc_small %>% st_make_valid() %>% st_point_on_surface() %>%
#   mutate(IDProv_chr = as.character(IDProv))
# leaflet(options = leafletOptions(minZoom = 2)) %>%
#   addMapPane("labels", zIndex = 430) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = nc_small, fillColor = ~pal_morr(IDProv), fillOpacity = 0.6,
#               color = "grey35", weight = 1,
#               popup = ~paste0("<b>SubDominio:</b> ", SubDominio,
#                               "<br><b>Dominio:</b> ", Dominio,
#                               "<br><b>IDProv:</b> ", IDProv)) %>%
#   addLabelOnlyMarkers(data = nc_labels, label = ~IDProv_chr,
#                       labelOptions = labelOptions(noHide = TRUE, direction = "center",
#                                                   textOnly = TRUE, textsize = "12px",
#                                                   style = list("font-weight"="bold",
#                                                                "color"="black",
#                                                                "text-shadow"="0 0 3px #FFFFFF, 0 0 6px #FFFFFF")),
#                       options = pathOptions(pane = "labels")) %>%
#   addCircleMarkers(data = faltantes_morrone, lng = ~longitude, lat = ~latitude,
#                    radius = 4, color = "black", fillColor = "white", fillOpacity = 0.95,
#                    stroke = TRUE, weight = 1,
#                    popup = ~paste0("<b>ID:</b> ", new.ID, "<br><b>Project.Source:</b> ", Project.Source)) %>%
#   addLegend("bottomright", pal = pal_morr, values = nc_small$IDProv,
#             title = "Morrone IDProv", opacity = 0.7)



manual_fix_morrone <- tibble::tribble(
  ~new.ID_sample,                        ~morrone_biogeoregions_IDProv,
  "NEF_GloFung-its2_76_2022_1",                      46,
  "NEF_GloFung-its2_77_2022_1",                      46,
  "NEF_GloFung-its2_89_2022_1",                      25,
  "NEF_GloFung-its2_89_2022_2",                      25,
  "NEF_GloFung-its2_710_2016_1",                     12,
  "NEF_GloFung-its1_183_2021_1",                     23,
  "NEF_GloFung-its1_183_2021_2",                     23,
  "NEF_GloFung-its1_200_2021_1",                     19,
  "NEF_GloFung-its1_237_2021_1",                     11,
  "NEF_GloFung-its1_327_2021_1",                     17,
  "NEF_GloFung-its1_388_2021_1",                     11,
  "NEF_GloFung-its1_389_2021_1",                     11,
  "NEF_GloFung-its1_394_2021_1",                     12,
  "NEF_GloFung-its1_396_2021_1",                     12,
  "NEF_GloFung-its1_426_2021_1",                      8,
  "NEF_GloFung-its1_432_2021_1",                      8,
  "NEF_GloFung-its1_443_2021_1",                     22,
  "NEF_GloFung-its1_482_2021_1",                     22,
  "NEF_diam_MRA_2025_1",                             30,
  "NEF_SPUNBR_SPUN77_2025_1",                        46,
  "NEF_SPUNBR_SPUN78_2025_1",                        46,
  "NEF_SPUNBR_SPUN79_2025_1",                        46,
  "NEF_SPUNBR_SPUN84_2025_1",                        46,
  "NEF_SPUNBR_SPUN14_2025_1",                        46,
  "NEF_SPUNBR_SPUN61_2025_1",                        46
)




manual_full_morrone <- manual_fix_morrone %>%
  left_join(morrone_lookup, by = "morrone_biogeoregions_IDProv")


# Corrigiendo errores causados por el punto de Mexico
# # ¿Qué IDProv están repetidos en el lookup?
# morrone_lookup %>%
#   count(morrone_biogeoregions_IDProv) %>%
#   filter(n > 1) %>%
#   arrange(desc(n))
# 
# # ¿Qué new.ID_sample se multiplicaron tras el join?
# manual_full_morrone %>%
#   count(new.ID_sample) %>%
#   filter(n > 1) %>%
#   arrange(desc(n))
# 
# id_fuera <- "NEF_AtlasMXB_B89_2025_1"
# 
# manual_fix_morrone <- manual_fix_morrone %>%
#   filter(new.ID_sample != id_fuera )


out1_complete <- out1_clean %>%
  left_join(manual_full_morrone, by = "new.ID_sample", suffix = c("", "_manual"))

summary_counts <- tibble(
  dataset = c("out1_complete", "out1_clean", "manual_full_morrone"),
  n_rows = c(nrow(out1_complete), nrow(out1_clean),  nrow(manual_full_morrone)),
  n_unique_ids = c(n_distinct(out1_complete$new.ID_sample),
                   n_distinct(out1_clean$new.ID_sample),
                   n_distinct(manual_full_morrone$new.ID_sample))
)

print(summary_counts) #Todo ok, mismo número de líneas!
# A tibble: 3 × 3
# dataset          n_rows n_unique_ids
# <chr>             <int>        <int>
# 1 out1_complete      3925         3925
# 2 out1_clean         3925         3925
# 3 manual_full_mor…     25           25



# Coalesce only where original is NA and manual has value
for (cn in morrone_cols) {
  man <- paste0(cn, "_manual")
  if (man %in% names(out1_complete)) {
    out1_complete[[cn]] <- dplyr::coalesce(out1_complete[[cn]], out1_complete[[man]])
  }
}


out1_complete <- out1_complete %>% select(-matches("_manual$"))


#### 2) WWF ecoregions + manual fixes (your final list) #####

stopifnot(file.exists(path_wwf_shp))
wwf <- st_read(path_wwf_shp, quiet = TRUE)
sf_use_s2(FALSE)

###  Eliminar as columnas existentes
out1_complete <-out1_complete %>%
  select(-matches("^wwf_ecoregions_"))



# Crop to Morrone extent (optional, using original Neotropic extents)
nc_orig <- read_sf("geodata_neotropic/NeotropicMap_Geo.shp")
ext_long <- range(st_coordinates(nc_orig)[,"X"])
ext_lat  <- range(st_coordinates(nc_orig)[,"Y"])
wwfSA <- st_crop(wwf, xmin = ext_long[1], ymin = ext_lat[1], xmax = ext_long[2], ymax = ext_lat[2]) # although coordinates are longitude/latitude, st_intersection assumes that they are planar
# Warning message:
#   attribute variables are assumed to be spatially constant throughout all geometries 

# Intersections (planar)
wwfSA_pl <- st_transform(wwfSA, 2163)
o1_sf    <- st_as_sf(out1_complete, coords = c("longitude","latitude"), crs = 4326)
o1_pl    <- st_transform(o1_sf, 2163)

if (!file.exists(cache_wwf)) {
  message("WWF intersections...")
  idx_list <- lapply(seq_len(nrow(o1_pl)), function(i) which(st_intersects(o1_pl[i,], wwfSA_pl, sparse = FALSE)))
  wwf_df <- purrr::imap_dfr(idx_list, function(ix, i) {
    if (length(ix) > 0) {
      cbind(data.frame(new.ID_sample = out1_complete$new.ID_sample[i]), st_drop_geometry(wwfSA_pl[ix,]))
    } else {
      empty_row <- as.list(rep(NA, ncol(wwfSA_pl))); names(empty_row) <- colnames(wwfSA_pl)
      data.frame(new.ID_sample = out1_complete$new.ID_sample[i], as.data.frame(empty_row))
    }
  })
  saveRDS(wwf_df, cache_wwf)
} else {
  wwf_df <- readRDS(cache_wwf)
}

wwf_tmp <- wwf_df %>% st_drop_geometry() %>%
  rename_with(~ paste0("wwf_ecoregions_", .x), -new.ID_sample)


summary_counts <- tibble(
  dataset = c("out1_complete", "wwf_df"),
  n_rows = c(nrow(out1_complete), nrow( wwf_df)),
  n_unique_ids = c(n_distinct(out1_complete$new.ID_sample),
                   n_distinct( wwf_df$new.ID_sample))
)

print(summary_counts)




out2 <- out1_complete %>% left_join(ensure_distinct_by(wwf_tmp), by = "new.ID_sample")
colnames (wwf_tmp)


### (Optional) WWF leaflet diagnostic map 
# # Uncomment to visualize points with missing WWF assignment
# 
# 

# out2 %>% filter (new.ID_sample == "NEF_GloFung-its2_89_2022_1") %>%select ("wwf_ecoregions_ECO_NAME" )
#  faltantes_wwf <- out2 %>% filter(is.na(wwf_ecoregions_ECO_NAME))
#  
#  sum(is.na(out2$wwf_ecoregions_ECO_NAME))
# library(leaflet); library(RColorBrewer)
# wwf_wgs <- st_transform(wwfSA, 4326)
# eco_levels <- sort(unique(wwf_wgs$ECO_NAME))
# pal_wwf <- colorFactor(palette = brewer.pal(max(8, min(length(eco_levels), 12)), "Set3"),
#                        domain = eco_levels)
# faltantes_wwf_sf <- st_as_sf(faltantes_wwf, coords = c("longitude","latitude"), crs = 4326)
# leaflet(options = leafletOptions(minZoom = 2)) %>%
#   addMapPane("labels", zIndex = 430) %>%
#   addProviderTiles("CartoDB.Positron", group = "Light") %>%
#   addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
#   addPolygons(data = wwf_wgs, fillColor = ~pal_wwf(ECO_NAME), fillOpacity = 0.6,
#               color = "grey35", weight = 0.7, smoothFactor = 0.5,
#               popup = ~paste0("<b>Ecoregion:</b> ", ECO_NAME,
#                               "<br><b>Biome:</b> ", ifelse(is.na(BIOME), "-", BIOME),
#                               "<br><b>Realm:</b> ", ifelse(is.na(REALM), "-", REALM),
#                               "<br><b>ECO_ID:</b> ", ifelse(is.na(ECO_ID), "-", ECO_ID))) %>%
#   addCircleMarkers(data = faltantes_wwf_sf, radius = 4, color = "black",
#                    fillColor = "white", fillOpacity = 0.95, stroke = TRUE, weight = 1,
#                    popup = ~paste0("<b>ID:</b> ", new.ID, "<br><b>Project.Source:</b> ", Project.Source)) %>%
#   addLegend("bottomright", pal = pal_wwf, values = wwf_wgs$ECO_NAME,
#             title = "WWF Ecoregions", opacity = 0.7)

#### Manual WWF fixes (first check the map!  this is the final list) ## AREA, PERIMETER creates several lines by ECO_ID - not use it here.#### WWF lookup 1:1 by ECO_ID (ignore AREA/PERIMETER which vary by multipart pieces) ####
wwf_lookup <- wwfSA %>%
  st_drop_geometry() %>%
  group_by(ECO_ID, ECO_NAME, BIOME, REALM, ECO_NUM, ECO_SYM, GBL_STAT) %>%
  summarise(.groups = "drop")

# Sanity check: one row per ECO_ID
stopifnot(n_distinct(wwf_lookup$ECO_ID) == nrow(wwf_lookup))

# (Optional) Inspect which ECO_IDs are multipart in the original shapefile
dup_ids <- wwfSA %>%
  st_drop_geometry() %>%
  count(ECO_ID, sort = TRUE) %>%
  filter(n > 1)
# print(dup_ids)

#### Manual fixes list (as provided) ####
manual_fix_wwf <- tibble::tribble(
  ~new.ID_sample,                  ~ECO_ID,
  "NEF_GloFung-its2_89_2022_1",    61407,
  "NEF_GloFung-its2_89_2022_2",    61407,
  "NEF_GloFung-its2_710_2016_1",   60226,
  "NEF_GloFung-its1_183_2021_1",   60129,
  "NEF_GloFung-its1_183_2021_2",   60129,
  "NEF_GloFung-its1_200_2021_1",   61405,
  "NEF_GloFung-its1_237_2021_1",   61305,
  "NEF_GloFung-its1_327_2021_1",   60181,
  "NEF_GloFung-its1_330_2021_1",   60181,
  "NEF_GloFung-its1_388_2021_1",   61305,
  "NEF_GloFung-its1_389_2021_1",   60179,
  "NEF_GloFung-its1_394_2021_1",   61305,
  "NEF_GloFung-its1_396_2021_1",   61305,
  "NEF_GloFung-its1_402_2021_1",   61402,
  "NEF_GloFung-its1_426_2021_1",   60213,
  "NEF_GloFung-its1_432_2021_1",   61402,
  "NEF_GloFung-its1_443_2021_1",   61308,
  "NEF_GloFung-its1_482_2021_1",   61308,
  "NEF_diam_MRA_2025_1",           61401,
  "NEF_SPUNBR_SPUN78_2025_1",      60160,
  "NEF_SPUNBR_SPUN79_2025_1",      60160
)

# Warn if any ECO_ID in the manual list is missing from the lookup
missing_ids <- dplyr::anti_join(manual_fix_wwf, wwf_lookup, by = "ECO_ID")
if (nrow(missing_ids) > 0) {
  warning("Manual ECO_ID not found in lookup: ",
          paste(unique(missing_ids$ECO_ID), collapse = ", "))
}

#### Expand WWF metadata for manual samples and prefix column names ####
manual_full_wwf <- manual_fix_wwf %>%
  left_join(wwf_lookup, by = "ECO_ID") %>%
  rename_with(~ paste0("wwf_ecoregions_", .x), -new.ID_sample)

#### Join to main dataset and coalesce fields (prefer manual when original is NA) ####
out2 <- out2 %>%
  left_join(manual_full_wwf, by = "new.ID_sample", suffix = c("", "_manual"))

# Identify WWF base columns (without the "_manual" suffix)
wwf_base <- grep("^wwf_ecoregions_", names(out2), value = TRUE)
wwf_base <- wwf_base[!grepl("_manual$", wwf_base)]

# Harmonize types (coalesce requires same type)
out2 <- out2 %>%
  dplyr::mutate(across(all_of(wwf_base), ~ if (is.factor(.)) as.character(.) else .)) %>%
  dplyr::mutate(across(ends_with("_manual"), ~ if (is.factor(.)) as.character(.) else .))

# Coalesce column-by-column
for (cn in wwf_base) {
  man <- paste0(cn, "_manual")
  if (man %in% names(out2)) {
    if (!identical(class(out2[[cn]]), class(out2[[man]]))) {
      # last-resort alignment: cast both to character
      out2[[cn]] <- as.character(out2[[cn]])
      out2[[man]] <- as.character(out2[[man]])
    }
    out2[[cn]] <- dplyr::coalesce(out2[[cn]], out2[[man]])
  }
}

# Drop helper columns
out2 <- out2 %>% select(-dplyr::ends_with("_manual"))

#### Final checks ####
# 1) one row per sample
stopifnot(nrow(out2) == dplyr::n_distinct(out2$new.ID_sample)) ### rows == uniqueIDs ==  3925!! :) ok!!


# 2) manual samples should now have non-NA ECO_ID
stopifnot(!any(is.na(out2$wwf_ecoregions_ECO_ID[out2$new.ID_sample %in% manual_fix_wwf$new.ID_sample])))





#### 3) CLIMENV extraction (optional) + Holdridge (Thornthwaite)####



# Remove any old climenv columns
climenv_old <- grep("^climenv", names(out2), value = TRUE)
out2_noclim <- out2 %>% select(-all_of(climenv_old))

# 2) Tu dataset -> sf de PUNTOS en WGS84 (EPSG:4326)
#    Ajusta los nombres de columnas si difieren


#### CLIMENV extraction ####
# Extract only if package is installed
# 
# Output dir for climenv artifacts
out_dir <- "geodata_neotropic/data_climenv"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Cache file for the list returned by ce_extract()
# (use your existing variable if already defined)
# cache_clim <- "geodata_neotropic/cache/WP2_clim.rds"


  
  # --- Build sf points (EPSG:4326) ---
  stopifnot(all(c("longitude","latitude","new.ID_sample") %in% names(out2_noclim)))
  out2_sf <- st_as_sf(out2_noclim, coords = c("longitude","latitude"), crs = 4326)
  stopifnot(all(st_geometry_type(out2_sf) %in% c("POINT","MULTIPOINT")))
  
  # --- Download grids once (if folder is empty) ---
  if (length(list.files(out_dir, all.files = TRUE, no.. = TRUE)) == 0) {
    message("• Downloading climenv grids into: ", out_dir)
    climenv::ce_download(path = out_dir, location = out2_sf)
  }
  
  # --- Extract (or load from cache) ---
  if (!file.exists(cache_clim)) {
    message("• Extracting climenv...")
    WP2_clim <- climenv::ce_extract(
      path       = out_dir,
      location   = out2_sf,
      location_g = "new.ID_sample" # group id
    )
    saveRDS(WP2_clim, cache_clim)
  } else {
    WP2_clim <- readRDS(cache_clim)
  }
 
  # --- Monthly tables (exclude *_sd, elev, lat, Readme) ---
  keep_monthly <- names(WP2_clim)[!grepl("elev|lat|Readme|_sd", names(WP2_clim))]
  tmp1 <- map(keep_monthly, function(nm) {
    out <- as.data.frame(WP2_clim[[nm]])
    colnames(out) <- paste("climenv_monthavg", nm, colnames(out), sep = "_")
    out %>% rownames_to_column("new.ID_sample")
  }) %>% reduce(full_join, by = "new.ID_sample")
  
  # --- Annual means (exclude *_sd and Readme) ---
  keep_annual <- names(WP2_clim)[!grepl("Readme|_sd", names(WP2_clim))]
  tmp2 <- map(keep_annual, function(nm) {
    mat <- as.matrix(WP2_clim[[nm]])
    # rowMeans on monthly columns; keep as numeric
    out <- as.data.frame(rowMeans(mat, na.rm = TRUE))
    colnames(out) <- nm
    out %>% rownames_to_column("new.ID_sample")
  }) %>% reduce(full_join, by = "new.ID_sample")
  
  # Rename annual columns: elev/lat -> "climenv_*", others -> "climenv_annavg_*"
  ann_cols <- setdiff(colnames(tmp2), "new.ID_sample")
  colnames(tmp2)[match(ann_cols, colnames(tmp2))] <-
    ifelse(grepl("^(elev|lat)$", ann_cols),
           paste0("climenv_", ann_cols),
           paste0("climenv_annavg_", ann_cols))
  
  # --- Merge back (many-to-one by new.ID_sample) ---
  out3 <- out2_noclim %>%
    left_join(ensure_distinct_by(tmp2), by = "new.ID_sample") %>%
    left_join(ensure_distinct_by(tmp1), by = "new.ID_sample")
  
  # Final sanity checks
  stopifnot(nrow(out3) == n_distinct(out3$new.ID_sample))
  


colnames ( out3 )

#  Holdridge using SPEI::thornthwaite ### different from Lucy!
#  
to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
month_levels <- month.abb  # "Jan","Feb",...,"Dec"

order_month_cols <- function(cols) {
  ord <- match(str_extract(cols, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"), month_levels)
  cols[order(ord)]
}

# ensure_distinct_by <- function(df, key = "new.ID_sample") {
#   df %>% distinct(.data[[key]], .keep_all = TRUE)
# }

# Build Holdridge class from PER (PET/TAP) and TAP (annual precip)
holdridge_class <- function(PER, TAP) {
  evapo <- list(c(NA, NA, "Wet tundra", rep("Wet forest", 4), "Rain forest"),
                c(NA, "Moist tundra", rep("Moist forest", 4), "Wet forest"), 
                c("Dry tundra", "Dry bush", "Steppe", rep("Dry forest",2), "Moist forest"),
                c("Desert", "Desert scrub", "Thorn steppe", "Very dry forest", "Dry forest"),
                c("Desert", "Desert scrub", "Thorn woodland", "Very dry forest"),
                c("Desert", "Desert scrub", "Thorn woodland"),
                c("Desert", "Desert scrub"),
                "Desert")
  names(evapo) <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16)
  
  precb <- list(c(rep("Desert", 5), "Dry tundra", NA, NA), 
                c(rep("Desert scrub", 4), "Dry bush", "Moist tundra"),
                c(rep("Thorn woodland", 2), "Thorn steppe", "Steppe", "Moist forest", "Wet tundra"),
                c(rep("Very dry forest", 2), "Dry forest", "Moist forest", "Wet forest"),
                c(rep("Dry forest", 2), "Moist forest", "Wet forest"),
                c(rep("Moist forest", 2), "Wet forest"), 
                rep("Wet forest", 2),
                "Rain forest")
  names(precb) <- c(62.5, 125, 250, 500, 1000, 2000, 4000, 8000)
  
  idx_per <- findInterval(PER, as.numeric(names(evapo))); idx_per[idx_per == 0] <- 1
  idx_tap <- findInterval(TAP, as.numeric(names(precb)));  idx_tap[idx_tap == 0] <- 1
  
  evapo_c <- evapo[idx_per]; prec_c <- precb[idx_tap]
  vapply(seq_along(evapo_c),
         function(i) paste(intersect(evapo_c[[i]], prec_c[[i]]), collapse = "|"),
         character(1))
}

# --- Column sets (from your out3) -----------------------------------------

tavg_cols <- grep("^climenv_monthavg_tavg_m_", names(out3), value = TRUE)
prec_cols <- grep("^climenv_monthavg_prec_m_", names(out3), value = TRUE)
tmin_cols <- grep("^climenv_monthavg_tmin_m_", names(out3), value = TRUE)
tmax_cols <- grep("^climenv_monthavg_tmax_m_", names(out3), value = TRUE)

stopifnot(length(tavg_cols) == 12, length(prec_cols) == 12)  # needed for both methods
tavg_cols <- order_month_cols(tavg_cols)
prec_cols <- order_month_cols(prec_cols)

# --- Matrices monthly (numeric) -------------------------------------------

tavg_mat <- as.matrix(as.data.frame(lapply(out3[tavg_cols], to_num)))
prec_mat <- as.matrix(as.data.frame(lapply(out3[prec_cols], to_num)))
lat_vec  <- as.numeric(out3$latitude)

# --- Method A: PET via SPEI::thornthwaite --------------------------------
pet_th_mat <- matrix(NA_real_, nrow(tavg_mat), 12)
for (i in seq_len(nrow(tavg_mat))) {
  ti <- tavg_mat[i, ]; la <- lat_vec[i]
  if (!all(is.na(ti)) && !is.na(la)) pet_th_mat[i, ] <- SPEI::thornthwaite(ti, lat = la)
}
tap_A        <- rowSums(prec_mat, na.rm = TRUE)
petA_annual  <- rowSums(pet_th_mat, na.rm = TRUE)
per_A        <- petA_annual / tap_A
class_A      <- holdridge_class(per_A, tap_A)

holdridge_A <- tibble(
  new.ID_sample = out3$new.ID_sample,
  method        = "thornthwaite",
  PET_annual    = petA_annual,
  TAP_annual    = tap_A,
  PER           = per_A,
  Holdridge     = class_A
)

out4<-holdridge_A
##### Save final output #####
write_csv(out4, out_csv)
message("✅ Done. Saved: ", out_csv)
