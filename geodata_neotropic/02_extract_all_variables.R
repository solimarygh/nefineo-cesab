# 02_extract_all_variables.R
# One-stop extraction: Morrone + WWF + Climenv + Holdridge.
# Leaflet diagnostic maps are commented by default.


#### Packages ####
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(stringr)
  library(purrr)
  library(tibble)
  library(SPEI)      # Thornthwaite PET
  library(leaflet)   # only if you re-enable maps
  library(RColorBrewer) # only if you re-enable maps
  library(climenv) 
  library(SPEI)
  library(missMDA)   # para PCA e imputación
})



#### Paths #####
# all relative to this project!


path_morrone_sf  <- "geodata_neotropic/morrone_shapefile_improved_sf.rds"   # created by 01_build_morrone_sf.R

path_wwf_shp <- "geodata_neotropic/data_WWF/wwf_terr_ecos.shp"


# si no tenemos estos datos, se crean
# cache_morrone:"geodata_neotropic/tmp_WP2_dataset_morrone.rds"
# cache_wwf: "geodata_neotropic/tmp_WP2_dataset_wwf.rds"
# "geodata_neotropic/tmp_WP2_dataset_climenv.rds" : "geodata_neotropic/tmp_WP2_dataset_climenv.rds"


ensure_distinct_by <- function(df, key = "new.ID") {
  df %>% distinct(across(all_of(key)), .keep_all = TRUE)
}

month_levels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
to_num <- function(x) as.numeric(if (is.list(x)) unlist(x) else x)


##### Load base table ####

# 1) Ruta del archivo

WP2data_in_csv_novo <- "geodata_neotropic/soil_pca_div_siteswp2.csv"
df_novo  <- read_csv(WP2data_in_csv_novo,  show_col_types = FALSE)
colnames(df_novo)

df0 <- df_novo


#### 1) MORRONE (point-in-polygon) + nearest-polygon fixes ####

stopifnot(file.exists(path_morrone_sf))
nc_small <- readRDS(path_morrone_sf)

# Intersections (planar)
pontos_sf <- st_as_sf(df0, coords = c("Longitude","Latitude"), crs = 4326)
morrone_plano  <- st_transform(nc_small, 2163) # Its non-deprecated replacement EPSG:9311 will be used instead. 
pontos_plano <- st_transform(pontos_sf, 2163)  # Its non-deprecated replacement EPSG:9311 will be used instead. 


# descomentar si no quieren extraer nuevamente
# if (!file.exists("geodata_neotropic#/tmp_WP2_dataset_morrone.rds")) {
message("• Morrone intersections...")
idx_list <- lapply(seq_len(nrow(pontos_plano)), function(i) which(st_intersects(pontos_plano[i,], morrone_plano, sparse = FALSE)))
morrone_df <- purrr::imap_dfr(idx_list, function(ix, i) {
  if (length(ix) > 0) {
    cbind(data.frame(new.ID = df0$new.ID[i]), st_drop_geometry(morrone_plano[ix,]))
  } else {
    empty_row <- as.list(rep(NA, ncol(morrone_plano))); names(empty_row) <- colnames(morrone_plano)
    data.frame(new.ID = df0$new.ID[i], as.data.frame(empty_row))
  }
})
saveRDS(morrone_df, "geodata_neotropic/tmp_WP2_dataset_morrone.rds")
morrone_df <- readRDS("geodata_neotropic/tmp_WP2_dataset_morrone.rds")
# } else {
#   morrone_df <- readRDS("geodata_neotropic/tmp_WP2_dataset_morrone.rds")
# }




# Keep only relevant columns and prefix as morrone_biogeoregions_
morro_tmp <- morrone_df %>%
  select(new.ID, IDProv, Region, Subregion, Provincias, Dominio, SubDominio) %>%
  rename_with(~ paste0("morrone_biogeoregions2_", .x), -new.ID)

out1 <- df0 %>% left_join(ensure_distinct_by(morro_tmp), by = "new.ID")

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

# Uncomment to visualize points that still lack Morrone province
faltantes_morrone <- out1_clean  %>% filter(is.na(morrone_biogeoregions_IDProv))

id_levels <- sort(unique(nc_small$IDProv))
pal_morr  <- colorFactor(palette = colorRampPalette(brewer.pal(12,"Set3"))(length(id_levels)),
                         domain = id_levels)
nc_labels <- nc_small %>% st_make_valid() %>% st_point_on_surface() %>%
  mutate(IDProv_chr = as.character(IDProv))

leaflet(options = leafletOptions(minZoom = 2)) %>%
  addMapPane("labels", zIndex = 430) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = nc_small, fillColor = ~pal_morr(IDProv), fillOpacity = 0.6,
              color = "grey35", weight = 1,
              popup = ~paste0("<b>SubDominio:</b> ", SubDominio,
                              "<br><b>Dominio:</b> ", Dominio,
                              "<br><b>IDProv:</b> ", IDProv)) %>%
  addLabelOnlyMarkers(data = nc_labels, label = ~IDProv_chr,
                      labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                                  textOnly = TRUE, textsize = "12px",
                                                  style = list("font-weight"="bold",
                                                               "color"="black",
                                                               "text-shadow"="0 0 3px #FFFFFF, 0 0 6px #FFFFFF")),
                      options = pathOptions(pane = "labels")) %>%
  addCircleMarkers(data = faltantes_morrone, lng = ~Longitude, lat = ~Latitude,
                   radius = 4, color = "black", fillColor = "white", fillOpacity = 0.95,
                   stroke = TRUE, weight = 1,
                   popup = ~paste0("<b>ID:</b> ", new.ID)) %>%
  addLegend("bottomright", pal = pal_morr, values = nc_small$IDProv,
            title = "Morrone IDProv", opacity = 0.7)






#### Imputación automática: polígono Morrone más cercano ####

if (nrow(faltantes_morrone) > 0) {
  
  # Puntos + polígonos en WGS84 (con coordenadas preservadas)
  out1_sf_wgs <- st_as_sf(
    out1_clean,
    coords  = c("Longitude","Latitude"),
    crs     = 4326,
    remove  = FALSE   # mantiene columnas Longitude/Latitude en el data.frame
  )
  
  morrone_wgs <- st_transform(nc_small, 4326)
  
  # Índices de los faltantes dentro de out1_clean
  idx_miss <- which(out1_clean$new.ID %in% faltantes_morrone$new.ID)
  
  # Para cada punto faltante → polígono Morrone más cercano
  nearest_ids <- st_nearest_feature(
    out1_sf_wgs[idx_miss, ],
    morrone_wgs
  )
  
  # Atributos del polígono más cercano
  morrone_nearest <- morrone_wgs[nearest_ids, ] %>%
    st_drop_geometry() %>%
    transmute(
      new.ID = out1_clean$new.ID[idx_miss],
      morrone_biogeoregions_IDProv    = as.integer(IDProv),
      morrone_biogeoregions_Region    = Region,
      morrone_biogeoregions_Subregion = Subregion,
      morrone_biogeoregions_Provincias= Provincias,
      morrone_biogeoregions_Dominio   = Dominio,
      morrone_biogeoregions_SubDominio= SubDominio
    )
  
  # Unir y coalescer: mantiene el valor original si ya existía,
  # y rellena con el "nearest" si era NA.
  out1_complete <- out1_clean %>%
    left_join(morrone_nearest, by = "new.ID", suffix = c("", "_nearest"))
  
  for (cn in morrone_cols) {
    cn_near <- paste0(cn, "_nearest")
    if (cn_near %in% names(out1_complete)) {
      out1_complete[[cn]] <- dplyr::coalesce(out1_complete[[cn]],
                                             out1_complete[[cn_near]])
    }
  }
  
  out1_complete <- out1_complete %>%
    select(-ends_with("_nearest"))
  
} else {
  # Si no hay faltantes, seguimos igual
  out1_complete <- out1_clean
}

# Chequeo rápido: tamaño y NAs
summary_counts <- tibble(
  dataset = c("out1_clean", "out1_complete"),
  n_rows = c(nrow(out1_clean), nrow(out1_complete)),
  n_unique_ids = c(n_distinct(out1_clean$new.ID),
                   n_distinct(out1_complete$new.ID))
)

print(summary_counts)

cat("N de puntos todavía sin Morrone después de la imputación: ",
    sum(is.na(out1_complete$morrone_biogeoregions_IDProv)), "\n")



## Chequeo: 



# IDs que antes entraban en la tabla manual_fix_morrone
ids_teste <- c(
  "NEF_GloFung.its2_76_2022",
  "NEF_GloFung.its2_77_2022",
  "NEF_GloFung.its2_89_2022",
  "NEF_GloFung.its2_710_2016",
  "NEF_GloFung.its1_183_2021",
  "NEF_GloFung.its1_200_2021",
  "NEF_GloFung.its1_237_2021",
  "NEF_GloFung.its1_327_2021",
  "NEF_GloFung.its1_388_2021",
  "NEF_GloFung.its1_389_2021",
  "NEF_GloFung.its1_394_2021",
  "NEF_GloFung.its1_396_2021",
  "NEF_GloFung.its1_426_2021",
  "NEF_GloFung.its1_432_2021",
  "NEF_GloFung.its1_443_2021",
  "NEF_GloFung.its1_482_2021",
  "NEF_diam_MRA_2025",
  "NEF_SPUNBR_SPUN77_2025",
  "NEF_SPUNBR_SPUN78_2025",
  "NEF_SPUNBR_SPUN79_2025",
  "NEF_SPUNBR_SPUN84_2025",
  "NEF_SPUNBR_SPUN14_2025",
  "NEF_SPUNBR_SPUN61_2025"
)

# Tabla con el IDProv que usabas en la corrección manual (para conferir)
morrone_expected <- tribble(
  ~new.ID,                      ~IDProv_expected,
  "NEF_GloFung.its2_76_2022",   46,
  "NEF_GloFung.its2_77_2022",   46,
  "NEF_GloFung.its2_89_2022",   25,
  "NEF_GloFung.its2_710_2016",  12,
  "NEF_GloFung.its1_183_2021",  23,
  "NEF_GloFung.its1_200_2021",  19,
  "NEF_GloFung.its1_237_2021",  11,
  "NEF_GloFung.its1_327_2021",  17,
  "NEF_GloFung.its1_388_2021",  11,
  "NEF_GloFung.its1_389_2021",  11,
  "NEF_GloFung.its1_394_2021",  12,
  "NEF_GloFung.its1_396_2021",  12,
  "NEF_GloFung.its1_426_2021",   8,
  "NEF_GloFung.its1_432_2021",   8,
  "NEF_GloFung.its1_443_2021",  22,
  "NEF_GloFung.its1_482_2021",  22,
  "NEF_diam_MRA_2025",          30,
  "NEF_SPUNBR_SPUN77_2025",     46,
  "NEF_SPUNBR_SPUN78_2025",     46,
  "NEF_SPUNBR_SPUN79_2025",     46,
  "NEF_SPUNBR_SPUN84_2025",     46,
  "NEF_SPUNBR_SPUN14_2025",     46,
  "NEF_SPUNBR_SPUN61_2025",     46
)

# Cómo estaban en out1_clean (antes de la imputación nearest)
out1_clean %>%
  filter(new.ID %in% ids_teste) %>%
  select(
    new.ID,
    morrone_biogeoregions_IDProv,
    morrone_biogeoregions_Region,
    morrone_biogeoregions_Subregion,
    morrone_biogeoregions_Provincias
  )

# Cómo quedaron en out1_complete (después de usar polígono más cercano)
morrone_check <- out1_complete %>%
  filter(new.ID %in% ids_teste) %>%
  select(
    new.ID,
    morrone_biogeoregions_IDProv,
  ) %>%
  left_join(morrone_expected, by = "new.ID")

morrone_check ### todo coincide con lo que habías hecho manualmente!!



#### 2) WWF ecoregions + nearest-polygon fixes #####

stopifnot(file.exists(path_wwf_shp))
wwf <- st_read(path_wwf_shp, quiet = TRUE)
sf_use_s2(FALSE)

# Remover cualquier columna antigua de WWF
out1_complete <- out1_complete %>%
  dplyr::select(-dplyr::matches("^wwf_ecoregions_"))

# Recortar a la extensión del Neotrópico (como antes)
nc_orig  <- read_sf("geodata_neotropic/NeotropicMap_Geo.shp")
ext_long <- range(st_coordinates(nc_orig)[,"X"])
ext_lat  <- range(st_coordinates(nc_orig)[,"Y"])

wwfSA <- st_crop(
  wwf,
  xmin = ext_long[1],
  ymin = ext_lat[1],
  xmax = ext_long[2],
  ymax = ext_lat[2]
)

# Intersections (planar)
wwfSA_pl <- st_transform(wwfSA, 2163)
o1_sf    <- st_as_sf(out1_complete,
                     coords = c("Longitude","Latitude"),
                     crs = 4326)
o1_pl    <- st_transform(o1_sf, 2163)

message("• WWF intersections...")
idx_list <- lapply(
  seq_len(nrow(o1_pl)),
  function(i) which(st_intersects(o1_pl[i,], wwfSA_pl, sparse = FALSE))
)

wwf_df <- purrr::imap_dfr(idx_list, function(ix, i) {
  if (length(ix) > 0) {
    cbind(
      data.frame(new.ID = out1_complete$new.ID[i]),
      st_drop_geometry(wwfSA_pl[ix,])
    )
  } else {
    empty_row <- as.list(rep(NA, ncol(wwfSA_pl)))
    names(empty_row) <- colnames(wwfSA_pl)
    data.frame(new.ID = out1_complete$new.ID[i],
               as.data.frame(empty_row))
  }
})

saveRDS(wwf_df, "geodata_neotropic/tmp_WP2_dataset_wwf.rds")
wwf_df <- readRDS("geodata_neotropic/tmp_WP2_dataset_wwf.rds")

wwf_tmp <- wwf_df %>%
  st_drop_geometry() %>%
  dplyr::rename_with(~ paste0("wwf_ecoregions_", .x), -new.ID)

summary_counts <- tibble(
  dataset = c("out1_complete", "wwf_df"),
  n_rows = c(nrow(out1_complete), nrow(wwf_df)),
  n_unique_ids = c(dplyr::n_distinct(out1_complete$new.ID),
                   dplyr::n_distinct(wwf_df$new.ID))
)

print(summary_counts)

# Join inicial WWF -> out2
out2 <- out1_complete %>%
  dplyr::left_join(ensure_distinct_by(wwf_tmp), by = "new.ID")

#### (Optional) WWF leaflet diagnostic map ####

faltantes_wwf <- out2 %>%
  dplyr::filter(is.na(wwf_ecoregions_ECO_ID))

if (nrow(faltantes_wwf) > 0) {
  library(leaflet); library(RColorBrewer)
  
  wwf_wgs <- st_transform(wwfSA, 4326)
  eco_levels <- sort(unique(wwf_wgs$ECO_NAME))
  
  pal_wwf <- colorFactor(
    palette = brewer.pal(max(8, min(length(eco_levels), 12)), "Set3"),
    domain  = eco_levels
  )
  
  faltantes_wwf_sf <- st_as_sf(
    faltantes_wwf,
    coords = c("Longitude","Latitude"),
    crs = 4326
  )
  
  leaflet(options = leafletOptions(minZoom = 2)) %>%
    addMapPane("labels", zIndex = 430) %>%
    addProviderTiles("CartoDB.Positron", group = "Light") %>%
    addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
    addPolygons(
      data        = wwf_wgs,
      fillColor   = ~pal_wwf(ECO_NAME),
      fillOpacity = 0.6,
      color       = "grey35",
      weight      = 0.7,
      smoothFactor= 0.5,
      popup = ~paste0(
        "<b>Ecoregion:</b> ", ECO_NAME,
        "<br><b>Biome:</b> ", ifelse(is.na(BIOME), "-", BIOME),
        "<br><b>Realm:</b> ", ifelse(is.na(REALM), "-", REALM),
        "<br><b>ECO_ID:</b> ", ifelse(is.na(ECO_ID), "-", ECO_ID)
      )
    ) %>%
    addCircleMarkers(
      data      = faltantes_wwf_sf,
      radius    = 4,
      color     = "black",
      fillColor = "white",
      fillOpacity = 0.95,
      stroke    = TRUE,
      weight    = 1,
      popup = ~paste0("<b>ID:</b> ", new.ID)
    ) %>%
    addLegend(
      "bottomright",
      pal     = pal_wwf,
      values  = wwf_wgs$ECO_NAME,
      title   = "WWF Ecoregions",
      opacity = 0.7
    )
}

#### Imputación automática WWF: ecorregión más cercana ####

if (nrow(faltantes_wwf) > 0) {
  
  # out2 como sf, preservando Longitude/Latitude
  out2_sf_wgs <- st_as_sf(
    out2,
    coords = c("Longitude","Latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  wwf_wgs <- st_transform(wwfSA, 4326)
  
  idx_miss <- which(out2$new.ID %in% faltantes_wwf$new.ID)
  
  nearest_ids <- st_nearest_feature(
    out2_sf_wgs[idx_miss, ],
    wwf_wgs
  )
  
  # Atributos de la ecorregión WWF más cercana
  wwf_nearest <- wwf_wgs[nearest_ids, ] %>%
    st_drop_geometry() %>%
    dplyr::mutate(new.ID = out2$new.ID[idx_miss]) %>%
    dplyr::rename_with(~ paste0("wwf_ecoregions_", .x), -new.ID)
  
  # Unir y coalescer: mantiene el valor original cuando existe,
  # y rellena con el nearest cuando está en NA
  out2_near <- out2 %>%
    dplyr::left_join(wwf_nearest, by = "new.ID", suffix = c("", "_nearest"))
  
  wwf_cols      <- grep("^wwf_ecoregions_", names(out2_near), value = TRUE)
  wwf_cols_base <- wwf_cols[!grepl("_nearest$", wwf_cols)]
  
  for (cn in wwf_cols_base) {
    cn_near <- paste0(cn, "_nearest")
    if (cn_near %in% names(out2_near)) {
      out2_near[[cn]] <- dplyr::coalesce(out2_near[[cn]],
                                         out2_near[[cn_near]])
    }
  }
  
  out2 <- out2_near %>%
    dplyr::select(-dplyr::ends_with("_nearest"))
  
} else {
  # si no hay faltantes WWF, out2 ya está ok
  out2 <- out2
}

#### Chequeos finales WWF ####

stopifnot(nrow(out2) == dplyr::n_distinct(out2$new.ID))

cat("N de puntos todavía sin WWF ECO_ID después de la imputación: ",
    sum(is.na(out2$wwf_ecoregions_ECO_ID)), "\n")

# 1) Tabla con los valores manuales (ECO_ID esperados)
manual_fix_wwf <- tibble::tribble(
  ~new.ID,                  ~ECO_ID,
  "NEF_GloFung.its2_89_2022",  61407,
  "NEF_GloFung.its2_710_2016", 60226,
  "NEF_GloFung.its1_183_2021", 60129,
  "NEF_GloFung.its1_200_2021", 61405,
  "NEF_GloFung.its1_237_2021", 61305,
  "NEF_GloFung.its1_327_2021", 60181,
  "NEF_GloFung.its1_330_2021", 60181,
  "NEF_GloFung.its1_388_2021", 61305,
  "NEF_GloFung.its1_389_2021", 60179,
  "NEF_GloFung.its1_394_2021", 61305,
  "NEF_GloFung.its1_396_2021", 61305,
  "NEF_GloFung.its1_402_2021", 61402,
  "NEF_GloFung.its1_426_2021", 60213,
  "NEF_GloFung.its1_432_2021", 61402,
  "NEF_GloFung.its1_443_2021", 61308,
  "NEF_GloFung.its1_482_2021", 61308,
  "NEF_diam_MRA_2025",         61401,
  "NEF_SPUNBR_SPUN78_2025",    60160,
  "NEF_SPUNBR_SPUN79_2025",    60160
)

# 2) Comparar ECO_ID manual vs lo que está ahora en out2
wwf_compare <- out2 %>%
  filter(new.ID %in% manual_fix_wwf$new.ID) %>%
  select(
    new.ID,
    wwf_ecoregions_ECO_ID,
  ) %>%
  left_join(manual_fix_wwf, by = "new.ID") %>%
  mutate(match = (wwf_ecoregions_ECO_ID == ECO_ID))

wwf_compare ## perfecto!! :)


#### 3) CLIMENV extraction (optional) + Holdridge (Thornthwaite)####


# Remove any old climenv columns
climenv_old <- grep("^climenv", names(out2), value = TRUE)
out2_noclim <- out2 %>% select(-all_of(climenv_old))

# 2) Tu dataset -> sf de PUNTOS en WGS84 (EPSG:4326)
#    Ajusta los nombres de columnas si difieren


#### 3) CLIMENV extraction + PCA imputation of missing climate data ####

library(tidyr)
library(missMDA)

# 3.1 Remover cualquier columna antigua de climenv -----------------------

climenv_old <- grep("^climenv", names(out2), value = TRUE)
out2_noclim <- out2 %>% dplyr::select(-all_of(climenv_old))

# 3.2 Dataset -> sf de puntos en WGS84 (EPSG:4326) -----------------------

stopifnot(all(c("Longitude","Latitude","new.ID") %in% names(out2_noclim)))

out2_sf <- st_as_sf(out2_noclim,
                    coords = c("Longitude","Latitude"),
                    crs    = 4326)

stopifnot(all(st_geometry_type(out2_sf) %in% c("POINT","MULTIPOINT")))

# 3.3 Descarga de los grids climenv (solo si la carpeta está vacía) ------

out_dir <- "geodata_neotropic/data_climenv"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (length(list.files(out_dir, all.files = TRUE, no.. = TRUE)) == 0) {
  message("• Downloading climenv grids into: ", out_dir)
  climenv::ce_download(path = out_dir, location = out2_sf)
}

# 3.4 Extracción climenv (o lectura del cache, si prefieres después) -----

message("• Extracting climenv...")
WP2_clim <- climenv::ce_extract(
  path       = out_dir,
  location   = out2_sf,
  location_g = "new.ID"  # agrupa por ID
)

saveRDS(WP2_clim, "geodata_neotropic/tmp_WP2_dataset_climenv.rds")
WP2_clim <- readRDS("geodata_neotropic/tmp_WP2_dataset_climenv.rds")

# 3.5 Tablas mensuales (monthly means) -----------------------------------
#    Excluye *_sd, elev, lat, Readme -> quedan solo las variables mensuales

keep_monthly <- names(WP2_clim)[!grepl("elev|lat|Readme|_sd", names(WP2_clim))]

tmp1 <- purrr::map(keep_monthly, function(nm) {
  out <- as.data.frame(WP2_clim[[nm]])
  colnames(out) <- paste("climenv_monthavg", nm, colnames(out), sep = "_")
  out %>% tibble::rownames_to_column("new.ID")
}) %>% purrr::reduce(dplyr::full_join, by = "new.ID")

# 3.6 Medias anuales (annual means) --------------------------------------
#    Excluye *_sd y Readme, hace rowMeans en las columnas mensuales

keep_annual <- names(WP2_clim)[!grepl("Readme|_sd", names(WP2_clim))]

tmp2 <- purrr::map(keep_annual, function(nm) {
  mat <- as.matrix(WP2_clim[[nm]])
  out <- as.data.frame(rowMeans(mat, na.rm = TRUE))
  colnames(out) <- nm
  out %>% tibble::rownames_to_column("new.ID")
}) %>% purrr::reduce(dplyr::full_join, by = "new.ID")

# Renombrar: elev/lat -> "climenv_*", los demás -> "climenv_annavg_*"

ann_cols <- setdiff(colnames(tmp2), "new.ID")
colnames(tmp2)[match(ann_cols, colnames(tmp2))] <-
  ifelse(grepl("^(elev|lat)$", ann_cols),
         paste0("climenv_", ann_cols),
         paste0("climenv_annavg_", ann_cols))

# 3.7 Unir clima (annual + monthly) de vuelta al dataset principal -------

out3 <- out2_noclim %>%
  dplyr::left_join(ensure_distinct_by(tmp2), by = "new.ID") %>%
  dplyr::left_join(ensure_distinct_by(tmp1), by = "new.ID")

stopifnot(nrow(out3) == dplyr::n_distinct(out3$new.ID))

# (Opcional) ver qué columnas climenv fueron creadas
# colnames(out3)[grepl("^climenv", colnames(out3))]

# 3.8 Diagnóstico de NAs en variables climáticas -------------------------

vars_clim <- grep("^climenv_", names(out3), value = TRUE)

# (a) Número de NAs por variable climática
na_climenv_long <- out3 %>%
  dplyr::summarise(across(all_of(vars_clim), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "n_NA"
  ) %>%
  dplyr::arrange(dplyr::desc(n_NA))

print(na_climenv_long)

# (b) Cuántas filas tienen por lo menos 1 NA climático?
out3 <- out3 %>%
  dplyr::mutate(
    n_NA_clim = rowSums(is.na(dplyr::select(., all_of(vars_clim))))
  )

table(out3$n_NA_clim == 0)  # TRUE = fila completa, FALSE = tiene algún NA

# 3.9 Imputación PCA (missMDA::imputePCA) para clima ----------------------
#     Usamos la misma lógica que Mica usó para el suelo:
#     - construimos una matriz solo con las variables climáticas (con NA)
#     - dejamos que imputePCA use correlaciones globales para rellenar NAs
#     - reescribimos solo los NAs, preservando valores observados

clim_mat <- as.matrix(out3[, vars_clim])

# ncp = número de componentes latentes usados para imputar.
# Puedes ajustar (3–5 es bien razonable); o dejar missMDA elegir.
imput_clim <- imputePCA(clim_mat, ncp = 5)

clim_imp <- imput_clim$completeObs  # matriz con NAs rellenados

# Sustituir solo los NAs en out3 por valores imputados
for (v in vars_clim) {
  na_idx <- which(is.na(out3[[v]]))
  if (length(na_idx) > 0) {
    out3[na_idx, v] <- clim_imp[na_idx, v]
  }
}

# 3.10 Chequeo post-imputación -------------------------------------------

# NAs remanentes en variables climáticas
na_climenv_after <- out3 %>%
  dplyr::summarise(across(all_of(vars_clim), ~ sum(is.na(.))))

print(na_climenv_after)

# Actualizar conteo de NAs por fila
out3 <- out3 %>%
  dplyr::mutate(
    n_NA_clim = rowSums(is.na(dplyr::select(., all_of(vars_clim))))
  )

table(out3$n_NA_clim == 0)  # idealmente, todo TRUE ahora

# Si quieres ver específicamente los IDs que podrían tener NA ahora:
problem_rows <- out3 %>% filter(n_NA_clim > 0)
problem_rows$new.ID # ahora realmente ya no hay más NAs climáticos

out3 <- out3 %>% select(-n_NA_clim)  # quitamos esta variable
colnames(out3)




#### 4) Holdridge life zones using SPEI::thornthwaite ####
# Objetivo de este bloque:
# - Usar las variables climáticas mensuales (tavg y precipitación) ya imputadas en out3
#   para calcular:
#      1) PET anual (evapotranspiración potencial)
#      2) TAP anual (precipitación total anual)
#      3) PER = PET/TAP
#      4) Clase de zona de vida de Holdridge ("Desert", "Moist forest", etc.)
# - Unir estos resultados de vuelta al dataset principal (out3 -> out4).


# helper para asegurar que las columnas mensuales estén en formato numérico
# (a veces climenv puede devolver character; esto evita bugs silenciosos)
to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))

# Vector con abreviaciones de los meses, para ordenar columnas *_Jan, *_Feb, ...
month_levels <- month.abb  # "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"

# Función para ordenar las columnas mensuales en la secuencia Jan–Dec
order_month_cols <- function(cols) {
  ord <- match(
    stringr::str_extract(cols, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"),
    month_levels
  )
  cols[order(ord)]
}

# 
# Función de clasificación de Holdridge
# 
# A partir de PER (PET/TAP) y TAP (precipitación anual), esta función:
# - localiza en qué clase de "evapotranspiración" (eje PER) cae el punto
# - localiza en qué clase de "faixa de precipitación" (eje TAP) cae el punto
# - intersecta las dos listas de nombres de formaciones (ej: "Moist forest", "Desert", etc.)
# - devuelve el nombre de la formación resultante como string.
holdridge_class <- function(PER, TAP) {
  # Eje PER (relación PET/TAP) -> tipos de formaciones asociadas a la evapotranspiración
  evapo <- list(
    c(NA, NA, "Wet tundra", rep("Wet forest", 4), "Rain forest"),
    c(NA, "Moist tundra", rep("Moist forest", 4), "Wet forest"),
    c("Dry tundra", "Dry bush", "Steppe", rep("Dry forest", 2), "Moist forest"),
    c("Desert", "Desert scrub", "Thorn steppe", "Very dry forest", "Dry forest"),
    c("Desert", "Desert scrub", "Thorn woodland", "Very dry forest"),
    c("Desert", "Desert scrub", "Thorn woodland"),
    c("Desert", "Desert scrub"),
    "Desert"
  )
  # Umbrales de PER usados para "binning"
  names(evapo) <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16)
  
  # Eje TAP (precipitación total anual) -> tipos de formaciones asociadas a la lluvia
  precb <- list(
    c(rep("Desert", 5), "Dry tundra", NA, NA),
    c(rep("Desert scrub", 4), "Dry bush", "Moist tundra"),
    c(rep("Thorn woodland", 2), "Thorn steppe", "Steppe", "Moist forest", "Wet tundra"),
    c(rep("Very dry forest", 2), "Dry forest", "Moist forest", "Wet forest"),
    c(rep("Dry forest", 2), "Moist forest", "Wet forest"),
    c(rep("Moist forest", 2), "Wet forest"),
    rep("Wet forest", 2),
    "Rain forest"
  )
  # Umbrales de TAP usados para "binning"
  names(precb) <- c(62.5, 125, 250, 500, 1000, 2000, 4000, 8000)
  
  # Para cada punto, descubrir en qué "clase" de PER cae
  idx_per <- findInterval(PER, as.numeric(names(evapo)))
  idx_per[idx_per == 0] <- 1  # si PER < menor umbral, va a la primera clase
  
  # Para cada punto, descubrir en qué "clase" de TAP cae
  idx_tap <- findInterval(TAP, as.numeric(names(precb)))
  idx_tap[idx_tap == 0] <- 1  # si TAP < menor umbral, va a la primera clase
  
  # Lista de formaciones por PER y por TAP
  evapo_c <- evapo[idx_per]
  prec_c  <- precb[idx_tap]
  
  # Intersección de las formaciones en ambos ejes (PER x TAP)
  vapply(
    seq_along(evapo_c),
    function(i) paste(intersect(evapo_c[[i]], prec_c[[i]]), collapse = "|"),
    character(1)
  )
}

# 
# 4.1 Seleccionar columnas climáticas mensuales necesarias
# 
# Usamos las variables de climenv ya construidas en out3:
# - tavg (temperatura media mensual)
# - prec (precipitación mensual)
# Las columnas vienen con patrón: "climenv_monthavg_tavg_m_Jan", etc.

tavg_cols <- grep("^climenv_monthavg_tavg_m_", names(out3), value = TRUE)
prec_cols <- grep("^climenv_monthavg_prec_m_", names(out3), value = TRUE)
tmin_cols <- grep("^climenv_monthavg_tmin_m_", names(out3), value = TRUE)  # no se usan en thornthwaite, pero sirven para inspección
tmax_cols <- grep("^climenv_monthavg_tmax_m_", names(out3), value = TRUE)  # idem

# Asegurar que tenemos 12 meses de tavg y 12 de precipitación (Jan–Dec)
stopifnot(length(tavg_cols) == 12, length(prec_cols) == 12)

# Ordenar las columnas en la secuencia correcta de meses
tavg_cols <- order_month_cols(tavg_cols)
prec_cols <- order_month_cols(prec_cols)

# 
# 4.2 Construir matrices mensuales numéricas (sites x 12 meses)
# 
# Aquí transformamos el data.frame en matrices numéricas:
# - filas = sitios (new.ID)
# - columnas = meses (Jan–Dec)

tavg_mat <- as.matrix(as.data.frame(lapply(out3[tavg_cols], to_num)))
prec_mat <- as.matrix(as.data.frame(lapply(out3[prec_cols], to_num)))

# Vector de latitud (grados) para cada sitio -> necesario para thornthwaite
lat_vec <- as.numeric(out3$Latitude)

## 4.3 PET mensual vía SPEI::thornthwaite ##
## # 
# thornthwaite(t, lat) necesita:
# - serie de 12 valores de temperatura media (°C)
# - latitud en grados
# y devuelve PET mensual (en mm) para cada mes de la serie.

pet_th_mat <- matrix(NA_real_, nrow(tavg_mat), 12)

for (i in seq_len(nrow(tavg_mat))) {
  ti <- tavg_mat[i, ]  # temperaturas del sitio i (12 meses)
  la <- lat_vec[i]     # latitud del sitio i
  
  # Si no está todo en NA y la latitud no es NA, calculamos PET
  if (!all(is.na(ti)) && !is.na(la)) {
    pet_th_mat[i, ] <- SPEI::thornthwaite(ti, lat = la)
  }
}

# 
# 4.4 TAP (precipitación total anual) y PET anual
# 
# TAP = suma de las precipitaciones mensuales (mm/año)
# PET_annual = suma de las PET mensuales (mm/año)

tap_A       <- rowSums(prec_mat, na.rm = TRUE)
petA_annual <- rowSums(pet_th_mat, na.rm = TRUE)

# PER = PET/TAP.
# Guard-rail importante: si TAP == 0, evitamos división por cero (NaN/Inf)
per_A <- ifelse(tap_A > 0, petA_annual / tap_A, NA_real_)

## 4.5 Clase de Holdridge para cada sitio
#

class_A <- holdridge_class(per_A, tap_A)

holdridge_A <- tibble::tibble(
  new.ID          = out3$new.ID,
  method_Holdrige = "thornthwaite",  # nombre del método usado para PET
  PET_annual      = petA_annual,     # PET anual (mm)
  TAP_annual      = tap_A,           # precipitación anual (mm)
  PER             = per_A,           # razón PET/TAP
  Holdridge_class = class_A          # clase textual de la zona de vida
)

# # 4.6 Unir de vuelta al dataset completo (out3 -> out4)
#
# Hacemos un left_join por new.ID, así cada sitio recibe su
# PET_annual / TAP_annual / PER / Holdridge_class.
# También removemos cualquier columna de geometría de WWF si todavía quedó algo.

out4 <- out3 %>%
  dplyr::left_join(holdridge_A, by = "new.ID") %>%
  dplyr::select(-dplyr::any_of("wwf_ecoregions_geometry"))

# 
# 4.7 Sanity checks
# 
# a) Garantizar que no duplicamos filas (1 fila por new.ID)
stopifnot(nrow(out4) == dplyr::n_distinct(out4$new.ID))

# b) Chequear si aún hay PER extraño (NaN o Inf) y si hay clases NA
cat("N NaN en PER: ", sum(is.nan(out4$PER)), "\n")
cat("N Inf en PER: ", sum(is.infinite(out4$PER)), "\n")
cat("N NA en Holdridge_class: ", sum(is.na(out4$Holdridge_class)), "\n")


# Yuhoooo!! todo el mundo tiene clima, PET, TAP, PER y una zona de vida de Holdridge bien definida, sin correcciones manuales,
# con:
#   Morrone por polígono + vecino más cercano,
#   WWF idem,
#   climenv completo vía imputación PCA,
#   y Holdridge corriendo fluido encima de todo eso.


##### Save final output #####
write_csv(out4, "geodata_neotropic/WP2_dataset_allvars.csv")
nrow(out4)
colnames(out4)

saveRDS(out4, "geodata_neotropic/WP2_dataset_allvars_NAsFree.rds", compress = "xz")






#######
# library(dplyr)
# 
# limite_km <- 50
# 
# # dist_df veio de antes:
#  dist_df <- tibble(
#    new.ID_sem_clima = sites_sem_clima$new.ID,
#    dist_m           = as.numeric(distances),
#    dist_km          = dist_m / 1000
#  )
# 
# ids_far <- dist_df %>%
#   filter(dist_km > limite_km) %>%
#   pull(new.ID_sem_clima)
# 
# length(ids_far)
# ids_far
# 
# # Tabela com os pontos a corrigir
# sites_far <- out4 %>%
#   filter(new.ID %in% ids_far) %>%
#   select(new.ID, Longitude, Latitude)
# 
# sites_far
# ### Agora vou para o shiny "Shiny_correcao_coordenas.R" , e vejo quais poderiam ser as novas coordenadas para esses pontos, ao ter os novos pontos corrigo o datafram da entrada iniicial Na primeri vez que fiz, só 5 pontos ficaram a mais de 50 km de distancia de algun otro sitio.
# ### 
# ### 
# #

