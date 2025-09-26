#09/11/2023
#L.Zinger

#_____________________________________
# Install/load required resources ----
#_____________________________________

## Load R packages ----
#_____________________________

#if(!require("rnaturalearth")) install.packages("rnaturalearth")
if(!require("sf")) install.packages("sf")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("elevatr")) install.packages("elevatr")
if(!require("climenv")) install.packages("climenv")
if (!require("devtools")) install.packages("devtools")
#if (!require("macroBiome")) devtools::install_github("szelepcsenyi/macroBiome")

#if(!require("rmapshaper")) install.packages("rmapshaper")
#if(!require("raster")) install.packages("raster")
#if(!require("openxlsx2")) install.packages("openxlsx2")
#if(!require("Ternary")) install.packages("Ternary")
#if(!require("MODISTools")) install.packages("MODISTools")
#if(!require("terra")) install.packages("terra")


## Check globalfungi data ----
#_____________________________

# import the two marker data and merge
#its1
globfits1 <-
  read_tsv("resources/Melanie_ITS1_SAMPLES_METADATA_ALL.txt",
           col_names = TRUE) %>%
  mutate(year_of_sampling = as.character(year_of_sampling))

#its2
globfits2 <-
  read_tsv("resources/Melanie_ITS2_SAMPLES_METADATA_ALL.txt",
           col_names = TRUE) %>%
  mutate(year_of_sampling = as.character(year_of_sampling))

#merge
globfall <- bind_rows(its1 = globfits1, its2 = globfits2, .id = "dataset")


# check lat / long range and distribution
ggplot(globfall, aes(latitude)) + 
  geom_histogram() + 
  theme_bw() + 
  facet_wrap(~dataset) + 
  ggtitle("Latitude distribution")

ggplot(globfall, aes(longitude)) + 
  geom_histogram() + 
  theme_bw() + 
  facet_wrap(~dataset) + 
  ggtitle("Longitude distribution")

#### Importante:::
#_____________________________________________
# Get Neotropical biogeogrphic provinces  ----
#_____________________________________________

#From Morrone et al 2022 neotropical regions 
#https://drive.google.com/drive/folders/189lpqHTQUtr33LlPQauki7ciJrmJcfVR
#the provided shapefile appears to be incomplete (e.g. do not have all province IDs)

## Format neotropical map ----
#_____________________________

# read and plot in the shapefile
nc <- read_sf("geodata_neotropic/NeotropicMap_Geo.shp")
nc <- st_as_sf(nc)

# test if correction/reduction of the shapefile has already been done because it takes time.
if(!file.exists("resources/morrone_shapefile_improved_sf.rds")){
  
  #if corrected file does not exist, then do the following modification
  # complement information of province ID, based on Morrone et al 2022 Map.
  # adicionando del articulo los codigos de provincias.
  nc$IDProv <- c(45, 51, 46, 6, 13, 41, 
                 18, 7, 42, 43, 47, 1, 
                 19, 52, 8, 53, 54, 20, 
                 48, 21, 22, 23, 30, 31,
                 9, 32, 10, 11, 36, 24, 
                 55, 14, NA, 33, 15, 44, 
                 34, 56, 49, 12, 57, 25,
                 37, 35, 26, 2, 3, 4,
                 50, 5, 27, 38, 28, 16,
                 29, 40, 17, 39)
  
  # complement information on Domain
  idx <- which(nc$Subregion == "South American Transition Zone")
  nc$Dominio[idx] <- "South American Transition Zone"
  
  idx <- which(nc$Subregion == "Antillean")
  nc$Dominio[idx] <- "Antillean"
  
  # add for more coherence with the map #usando la informacion del paper de Morrone
  nc$SubDominio <- NA
  nc$SubDominio[nc$IDProv<=5] <- "Mexican transition zone"
  nc$SubDominio[nc$IDProv>5 & nc$IDProv<13] <- "Antillean subregion"
  nc$SubDominio[nc$IDProv>12 & nc$IDProv<18] <- "Brazilian subregion Mesoamerican dominion"
  nc$SubDominio[nc$IDProv>17 & nc$IDProv<30] <- "Pacific subregion"
  nc$SubDominio[nc$IDProv>29 & nc$IDProv<36] <- "Boreal Brazilian dominion"
  nc$SubDominio[nc$IDProv>35 & nc$IDProv<40] <- "South Brazilian dominion"
  nc$SubDominio[nc$IDProv==40] <- "Chacoan subregion Southeastern Amazonian dominion"
  nc$SubDominio[nc$IDProv>40 & nc$IDProv<45] <- "Chacoan dominion"
  nc$SubDominio[nc$IDProv>44 & nc$IDProv<51] <- "Parana dominion"
  nc$SubDominio[nc$IDProv>50] <- "South American transition zone"
  nc$SubDominio[is.na(nc$IDProv)] <- "Neartic and Andean regions"
  
  # reduce resolution 
  nc_small <- ms_simplify(nc, keep = 0.05)
  saveRDS(nc_small, "resources/morrone_shapefile_improved_sf.rds") ###baja resolucion para marcar
  
} else {
  #if file does exist, then import it.
  nc_small <- readRDS("resources/morrone_shapefile_improved_sf.rds")
  
}


# define morrone's color vector
levels(as.factor(nc_small$SubDominio))
cols <- c("purple", "coral3", "yellow2", "dodgerblue2", "cyan2",
          "green4", "grey", "orange", "dodgerblue4", 
          "darkgoldenrod4", "salmon1")

# plot
ggplot(nc_small) + 
  geom_sf(aes(fill=SubDominio), colour = "grey35") +
  scale_fill_manual(values=cols) + 
  theme_void() 

## Filter points that fall into the morrone map ----
## ### nosotros tenemos todos los puntos dentro de Neotropics, no necesitamos hacer esto
#__________________________________________________

# extract lat long limits of the map range
toto <- st_coordinates(nc) ###nc is the original map
ext_long <- range(toto[,"X"])  
ext_lat <- range(toto[,"Y"])  

# restrict dataset to neotropics
idx_lat <- which(as.numeric(globfall$latitude) >= ext_lat[1] &
                   as.numeric(globfall$latitude) <= ext_lat[2])
idx_long <- which(as.numeric(globfall$longitude) >= ext_long[1] &
                    as.numeric(globfall$longitude) <= ext_long[2])
inter_latlong <- intersect(idx_lat, idx_long)
globfSA <- globfall[inter_latlong, ]

# test how many data lost (should be none)
nrow(globfSA) == nrow(globfall)

# exclude marine/aerial and other non-desired stuff
globfSA <- globfSA[which(globfSA$sample_type=="litter" |
                           globfSA$sample_type=="rhizosphere soil" |
                           globfSA$sample_type=="root" |
                           globfSA$sample_type=="root + rhizosphere soil" |
                           globfSA$sample_type=="soil" |
                           globfSA$sample_type=="topsoil"),]

## Map the distribution of the Global fungi points ----
#______________________________________________________
# transform into a sf object
globfSA_sf <- sf::st_as_sf(globfSA, 
                           coords = c("longitude", "latitude"), 
                           crs=4326) 

# define color vector for morrone
cols2 <- scales::alpha(c("purple", "coral3", "yellow2", "dodgerblue2", "cyan2",
                         "green4", "grey", "orange", "dodgerblue4", 
                         "darkgoldenrod4", "salmon1"),.4)

# plot all samples  
ggplot(nc_small) + 
  geom_sf(aes(fill=SubDominio), colour = "black") +
  scale_fill_manual(values=cols2) + 
  theme_void() + 
  geom_sf(data = globfSA_sf,
          aes(geometry = geometry, shape=dataset, color=sample_type),
          size = 1) + 
  scale_color_brewer(palette = "Dark2")

# export soil/root filtered data
write.table(globfSA, file="resources/globfungi_nefineo_filtered.tsv", sep="\t", 
            quote = FALSE, row.names = F, col.names = T)

# Map soil samples on neotropical map and save
globfSAsoil <-
  globfSA[which(globfSA$sample_type == "soil" |
                  globfSA$sample_type == "topsoil"), ]
globfSAsoil_sf <- sf::st_as_sf(globfSAsoil,
                               coords = c("longitude", "latitude"),
                               crs = 4326)
fig <-
  ggplot(nc_small) +
  geom_sf(aes(fill = SubDominio), colour = "grey35") +
  scale_fill_manual(values = cols2) +
  theme_void() +
  geom_sf(data = globfSAsoil_sf,
          aes(geometry = geometry, shape = target_gene),
          size = 1, color = "black") +
  labs(fill = "Biogeographic domains",
       shape = "DNA marker",
       title = "Soil samples distribution") +
  theme(
    plot.margin = margin(0.1, 0, 0, 0) ,
    legend.position = c(0.3, 0.3),
    legend.text = element_text(size = 8)
  )

ggsave(
  filename = "figures/01_soilsamples_map_morrone.pdf",
  device = "pdf",
  width = 6,
  height = 7,
  dpi = 150,
  units = "in"
)

# same on roots/rhizosphere

globfSAroot <-
  globfSA[which(globfSA$sample_type == "root" |
                  globfSA$sample_type == "root + rhizosphere soil" |
                  globfSA$sample_type == "rhizosphere soil"), ]
globfSAroot_sf <- sf::st_as_sf(globfSAroot,
                               coords = c("longitude", "latitude"),
                               crs = 4326)

fig <-
  ggplot(nc_small) +
  geom_sf(aes(fill = SubDominio), colour = "grey35") +
  scale_fill_manual(values = cols2) +
  theme_void() +
  geom_sf(data = globfSAroot_sf,
          aes(geometry = geometry, shape = target_gene),
          size = 1, color = "black") +
  labs(fill = "Biogeographic domains",
       shape = "DNA marker",
       title = "Root/rhizopshere samples distribution") +
  theme(
    plot.margin = margin(0.1, 0, 0, 0) ,
    legend.position = c(0.3, 0.3),
    legend.text = element_text(size = 8)
  )

ggsave(
  filename = "figures/02_rootrhizosamples_map_morrone.pdf",
  device = "pdf",
  width = 6,
  height = 7,
  dpi = 150,
  units = "in"
)


## Get neotropical classification per points ----
#_________________________________________________
# On all Globfungi neotropical data

# transformation to planar is required, as sf library assumes planar projection 
nc_small_pl <- st_transform(nc_small, 2163)
globfSA_sf_pl <- st_transform(globfSA_sf, 2163)

#find overlaps and format into a data.frame output. Takes time so to run only if necessary

if(!file.exists(results/tmp_globfSA_morrone.rds)){
  globfSA_morrone <- do.call("rbind", lapply(1:nrow(globfSA_sf), function(row) {
    idx <-
      which(st_intersects(globfSA_sf_pl[row,], nc_small_pl, sparse = FALSE))
    if (length(idx) > 0) {
      data.frame(PermanentID = globfSA_sf$PermanentID[row],
                 nc_small_pl[which(st_intersects(globfSA_sf_pl[row,], 
                                                 nc_small_pl, sparse = FALSE)), ])
    } else {
      tmp <- data.frame(matrix(NA, 1, ncol(nc_small_pl),
                               dimnames = list(row, colnames(nc_small_pl))))
      data.frame(PermanentID = globfSA_sf$PermanentID[row], tmp)
    }
  }))
  saveRDS(globfSA_morrone, "results/tmp_globfSA_morrone.rds")
} else {
  globfSA_morrone <- readRDS("results/tmp_globfSA_morrone.rds")
}

#format to merge with initial metadata.
tmp <- globfSA_morrone[, colnames(globfSA_morrone) != "geometry"]
colnames(tmp)[-1] <- paste("morrone_biogeoregions", colnames(tmp)[-1], sep = "_")

#test if samples are in same numbers and order, if not return warnings for checks

if(all(globfall$PermanentID == tmp$PermanentID)){
  
  out1 <- 
    globfall %>%
    bind_cols(tmp[,-grep("PermanentID", colnames(tmp))])
  
} else {
  print("Warning!! permanent ids number and order are different between the mapped and initial data; check that everything above was done correctly")
}

## Checks missing values for neotropical classification ----
#____________________________________________________________

# look for points do not have assigned morrone regions:
idx <- which(is.na(out1$morrone_biogeoregions_SubDominio))
length(idx)
# 196 samples not assigned initially

# !!! manual checks !!! 
# coordinates location were checked on goolge maps; corrections are then done with the closest polygon of the sample point.

View(out1[idx,grep("PermanentID|country|latitude|longitude|morrone", colnames(out1))])
View(out1[idx,])

# neartic and andean regions
tmp <- c("GF05018098S","GF05013195S","GF05001520V", "GF05001356V", 
         "GF03004312S","GF03004306S","GF03004233S","GF03004224S",
         "GF03004205S","GF03004192S","GF03004136S","GF03004130S",
         "GF03004068S","GF03004057S","GF03003971S", "GF03003914S")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  out1[which(out1$morrone_biogeoregions_SubDominio == "Neartic and Andean regions")[1], 
       grep("morrone", colnames(out1))]

# Farrer_2021_MM all USA
out1[which(out1$paper_id == "Farrer_2021_MM" &
             is.na(out1$morrone_biogeoregions_SubDominio)),
     grep("morrone", colnames(out1))] <-
  out1[which(out1$morrone_biogeoregions_SubDominio == "Neartic and Andean regions")[1], 
       grep("morrone", colnames(out1))]
# Cox_2016_EDC5 all antartica
out1[which(out1$paper_id == "Cox_2016_EDC5" &
             is.na(out1$morrone_biogeoregions_SubDominio)),
     grep("morrone", colnames(out1))] <-
  out1[which(out1$morrone_biogeoregions_SubDominio == "Neartic and Andean regions")[1], 
       grep("morrone", colnames(out1))]

# samples in sea closed to the coast: check in morrone and google map with coordinates
# Donald_2021_SQ
out1[which(out1$paper_id == "Donald_2021_SQ" &
             is.na(out1$morrone_biogeoregions_SubDominio)),
     grep("morrone", colnames(out1))] <-
  out1[which(out1$paper_id == "Donald_2021_SQ" &
               !is.na(out1$morrone_biogeoregions_SubDominio))[1],
       grep("morrone", colnames(out1))]

# Costarica samples Tedersoo_2021_TJ + Ettinger_2020_1ce1
tmp <- c("GF05001427V", "GF05001357V", 
         "GF03003969S", "GF03003924S", "GF03003890S")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  out1[which(out1$morrone_biogeoregions_Provincias == "Guatuso-Talamanca province")[1], 
       grep("morrone", colnames(out1))]

# Colombia Tedersoo_2021_TJ
tmp <- c("GF05001333V")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Choco-Darien province")[1],])

# Antilles Tedersoo_2021_TJ        
tmp <- c("GF05001003V", "GF05000416V", "GF05000415V")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Lesser Antilles province")[1],])

# Mexico Tedersoo_2021_TJ 
tmp <- c("GF05000500V")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Yucatan Peninsula Province")[1],])

# Puerto rico Tedersoo_2021_TJ + Urbina_2016_CE8E
tmp <- c("GF05000410V", "GF05000408V", 
         "GF01016999S")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Puerto Rico province")[1],])

# Cuba Tedersoo_2021_TJ
tmp <- c("GF05000294V", "GF05000288V")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Cuban province")[1],])

# Colombia carab Tedersoo_2021_TJ
tmp <- c("GF05000208V", "GF05000052V")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Guajira province")[1],])

# Brazilflorianopolis Geml_2022_WM
tmp <- c("GF05026779S", "GF05026777S")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Atlantic province")[1],])

# Panama Geml_2022_WM 
tmp <- c("GF05026758S", "GF05026757S")
out1[grep(paste(tmp, collapse = "|"), out1$PermanentID), 
     grep("morrone", colnames(out1))] <- 
  st_drop_geometry(nc_small_pl[which(nc_small_pl$Provincias == "Puntarenas-Chiriqui province")[1],])

# moving command for checks until no more NAs.
idx <- which(is.na(out1$morrone_biogeoregions_SubDominio))
length(idx)
View(out1[idx,grep("PermanentID|country|latitude|longitude|morrone", colnames(out1))])
View(out1[idx,])

## Basic stast ----
#__________________

tmp <- out1 %>%
  count(
    target_gene,
    sample_type,
    morrone_biogeoregions_Region,
    morrone_biogeoregions_Provincias
  )

fig <-
  ggplot(tmp,
         aes(
           x = sample_type,
           y = paste(
             morrone_biogeoregions_Region,
             morrone_biogeoregions_Provincias
           )
         )) +
  geom_point(aes(color = n, size = n)) +
  facet_wrap( ~ target_gene) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "sample type",
       y = "Neotropical regions",
       size = "# samples",
       color = "# samples") 

ggsave(
  filename = "figures/03_statsamples_morrone.pdf",
  device = "pdf",
  width = 7,
  height = 7,
  dpi = 150,
  units = "in"
)


## Export metadata temp file with neotropical classification ----
#________________________________________________________________

if(length(idx) == 0) {
  write.table(
    out1,
    file = "results/tmp_globfungi_nefineo_filtered_morrone.tsv",
    sep = "\t",
    quote = FALSE,
    row.names = F,
    col.names = T
  )
}


####NOt necesitamos nada anterior! 
####Importante aqui abajo:
# Get WWF ecoregions  ----
#___________________________
#From WWF 
#https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world

## Imports and checks ----
#__________________________

# read and plot in the shapefile
wwf <- st_read("resources/WWF_ecorregions/wwf_terr_ecos.shp")
sf_use_s2(FALSE)

wwfSA <-
  st_crop(
    wwf,
    xmin = ext_long[1],
    ymin = ext_lat[1],
    xmax = ext_long[2],
    ymax = ext_lat[2]
  )

#reformat names realsm and biomes
tmp <- factor(wwfSA$REALM)
levels(tmp) <- c("Antarctic", "Nearctic", 
                 "Neotropics", "Oceania")
wwfSA$REALM <- tmp

tmp <- factor(wwfSA$BIOME)
levels(tmp) <- c("Tropical & Subtropical Moist Broadleaf Forests", 
                 "Tropical & Subtropical Dry Broadleaf Forests", 
                 "Tropical & Subtropical Coniferous Forests", 
                 "Temperate Broadleaf & Mixed Forests", 
                 "Temperate Conifer Forests", 
                 "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                 "Temperate Grasslands, Savannas & Shrublands", 
                 "Flooded Grasslands & Savannas", 
                 "Montane Grasslands & Shrublands", 
                 "Tundra", 
                 "Mediterranean Forests, Woodlands & Scrub", 
                 "Deserts & Xeric Shrublands", 
                 "Mangroves", "98", "99")
wwfSA$BIOME <- tmp


## Map distribution of global fungi points on wwf map ----
#___________________________________________________________

fig <-
  ggplot(wwfSA) +
  geom_sf(aes(fill = ECO_NAME), colour = "grey35", alpha=0.5) +
  theme_void() +
  geom_sf(data = globfSAsoil_sf,
          aes(geometry = geometry, shape = target_gene),
          size = 1, color = "black") +
  guides(fill = "none") +
  labs(shape = "DNA marker",
       title = "Soil samples distribution") 

ggsave(
  filename = "figures/04_soilsamples_map_wwf.pdf",
  device = "pdf",
  width = 6,
  height = 7,
  dpi = 150,
  units = "in"
)

#same on roots/rhizosphere

fig <-
  ggplot(wwfSA) +
  geom_sf(aes(fill = ECO_NAME), colour = "grey35", alpha=0.5) +
  theme_void() +
  geom_sf(data = globfSAroot_sf,
          aes(geometry = geometry, shape = target_gene),
          size = 1, color = "black") +
  guides(fill = "none") +
  labs(shape = "DNA marker",
       title = "Root/rhizosphere samples distribution")

ggsave(
  filename = "figures/05_rootrhizosamples_map_wwf.pdf",
  device = "pdf",
  width = 6,
  height = 7,
  dpi = 150,
  units = "in"
)


## Get wwf ecoregions classification for global fungi data ----
#__________________________________________________________

# make wwf map planar ### hacer   que el la proyection sea del mismo sistema
wwfSA_pl <- st_transform(wwfSA, 2163)

if(!file.exists("results/tmp_globfSA_wwf.rds")){
  globfSA_wwf <-
    do.call("rbind", lapply(1:nrow(globfSA_sf), function(row) {
      idx <-
        which(st_intersects(globfSA_sf_pl[row, ], wwfSA_pl, sparse = FALSE))
      if (length(idx) > 0) {
        data.frame(PermanentID = globfSA_sf$PermanentID[row],
                   wwfSA_pl[which(st_intersects(globfSA_sf_pl[row, ], 
                                                wwfSA_pl, sparse = FALSE)),])
      } else {
        tmp <- data.frame(matrix(NA, 1, ncol(wwfSA_pl),
                                 dimnames = list(row, colnames(wwfSA_pl))))
        data.frame(PermanentID = globfSA_sf$PermanentID[row], tmp)
      }
    }))
  saveRDS(globfSA_wwf, "results/tmp_globfSA_wwf.rds")
} else {
  globfSA_wwf <- readRDS("results/tmp_globfSA_wwf.rds")
}

#format to merge with initial metadata.
tmp <- globfSA_wwf[, colnames(globfSA_wwf) != "geometry"]
colnames(tmp)[-1] <- paste("wwf_ecoregions", colnames(tmp)[-1], sep = "_")

#test if samples are in same numbers and order, if not return warnings for checks

if(all(out1$PermanentID == tmp$PermanentID)) {
  out2 <-
    out1 %>%
    bind_cols(tmp[, -grep("PermanentID", colnames(tmp))])
} else {
  print(
    "Warning!! permanent ids number and order are different between the mapped and post morrone data; check that everything above was done correctly"
  )
}

## Checks missing values for wwf ecoregions ----
#_______________________________________________

#Several points do not have assigned morrone regions:
idx <- which(is.na(out2$wwf_ecoregions_ECO_NAME))
length(idx)
#180 samples not assigned

# !!! manual checks !!! 
View(out2[idx,grep("PermanentID|country|latitude|longitude|wwf", colnames(out2))])
View(out2[idx,])
# use of https://www.arcgis.com/apps/View/index.html?appid=d60ec415febb4874ac5e0960a6a2e448 for reassignements.

#Hargreaves_2018_027D
out2[which(out2$paper_id == "Hargreaves_2018_027D" &
             is.na(out2$wwf_ecoregions_ECO_NAME)),
     grep("wwf", colnames(out2))] <-
  out2[which(out2$paper_id == "Hargreaves_2018_027D" &
               !is.na(out2$wwf_ecoregions_ECO_NAME))[1],
       grep("wwf", colnames(out2))]

#Donald_2021_SQ
out2[which(out2$paper_id == "Donald_2021_SQ" &
             is.na(out2$wwf_ecoregions_ECO_NAME)),
     grep("wwf", colnames(out2))] <-
  out2[which(out2$paper_id == "Donald_2021_SQ" &
               !is.na(out2$wwf_ecoregions_ECO_NAME))[1],
       grep("wwf", colnames(out2))]

#Lin_2021_RK
out2[which(out2$paper_id == "Lin_2021_RK" &
             is.na(out2$wwf_ecoregions_ECO_NAME)),
     grep("wwf", colnames(out2))] <-
  out2[which(out2$paper_id == "Lin_2021_RK" &
               !is.na(out2$wwf_ecoregions_ECO_NAME))[1],
       grep("wwf", colnames(out2))]

#Cox_2016_EDC5
out2[which(out2$paper_id == "Cox_2016_EDC5" &
             is.na(out2$wwf_ecoregions_ECO_NAME)),
     grep("wwf", colnames(out2))] <-
  out2[which(out2$wwf_ecoregions_ECO_NAME=="Scotia Sea Islands tundra")[1],
       grep("wwf", colnames(out2))]

#Vasar_2022_WX
out2[which(out2$paper_id == "Vasar_2022_WX" &
             is.na(out2$wwf_ecoregions_ECO_NAME)),
     grep("wwf", colnames(out2))] <-
  out2[which(out2$wwf_ecoregions_ECO_NAME=="Magellanic subpolar forests")[1],
       grep("wwf", colnames(out2))]

#Costarica samples + Panama atlant 
tmp <- c("GF05001427V", "GF05001357V", 
         "GF03003969S", "GF03003924S", "GF03003890S")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  out2[which(out2$wwf_ecoregions_ECO_NAME == "Isthmian-Atlantic moist forests")[1], 
       grep("wwf", colnames(out2))]

#One by ones
# Florida
tmp <- c("GF05001356V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Everglades")[1],])

# Colombia west
tmp <- c("GF05001333V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "South American Pacific mangroves")[1],])
tmp <- c("GF05000208V", "GF05000052V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Guajira-Barranquilla xeric scrub")[1],])

# Antilles
tmp <- c("GF05001003V", "GF05000416V", 
         "GF05000410V", "GF05000408V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Caribbean shrublands")[1],])

tmp <- c("GF05000415V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Windward Islands moist forests")[1],])

# Mexico
tmp <- c("GF05000500V", "GF05000497V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Yucatán moist forests")[1],])

# Puerto rico
tmp <- c("GF05000402V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Puerto Rican moist forests")[1],])

tmp <- c("GF01016999S")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Puerto Rican dry forests")[1],])

# Cuba
tmp <- c("GF05000294V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Cuban dry forests")[1],])
tmp <- c("GF05000288V")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Bahamian-Antillean mangroves")[1],])

# Panama
tmp <- c("GF05026758S", "GF05026757S")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "Isthmian-Pacific moist forests")[1],])

#usa raffale
tmp <- c("GF03004312S", "GF03004306S", "GF03004233S", "GF03004224S",
         "GF03004205S", "GF03004192S", "GF03004136S", "GF03004130S",
         "GF03004068S", "GF03004057S", "GF03003971S", "GF03003914S")
out2[grep(paste(tmp, collapse = "|"), out2$PermanentID), 
     grep("wwf", colnames(out2))] <- 
  st_drop_geometry(wwfSA_pl[which(wwfSA_pl$ECO_NAME == "California coastal sage and chaparral")[1],])

# manual checks function to move during the check/correction process.

idx <- which(is.na(out2$wwf_ecoregions_ECO_NAME))
length(idx)
View(out2[idx,grep("PermanentID|country|latitude|longitude|wwf", colnames(out2))])
View(out2[idx,])

## Basic stast ----
#__________________

tmp <- out2 %>%
  count(
    target_gene,
    sample_type,
    wwf_ecoregions_REALM,
    wwf_ecoregions_BIOME, 
    wwf_ecoregions_ECO_NAME,
  )

fig <-
  ggplot(tmp,
         aes(
           x = sample_type,
           y = wwf_ecoregions_ECO_NAME
         )) +
  geom_point(aes(color = n, size = n)) +
  facet_grid(wwf_ecoregions_REALM*wwf_ecoregions_BIOME ~ target_gene, 
             scales = "free_y",  space="free_y", switch = "y") +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0)) +
  labs(x = "sample type",
       y = "WWF ecoregions",
       size = "# samples",
       color = "# samples") 

ggsave(
  filename = "figures/06_statsamples_wwf.pdf",
  device = "pdf",
  width = 15,
  height = 15,
  dpi = 150,
  units = "in"
)


## Export metadata temp file with wwf classification ----
#________________________________________________________

if(length(idx) == 0) {
  write.table(
    out2,
    file = "results/tmp_globfungi_nefineo_filtered_morrone_wwf.tsv",
    sep = "\t",
    quote = FALSE,
    row.names = F,
    col.names = T
  )
  
}


# Get elevation ----
#__________________
#

#We dont need to do this: up to.. #Clim elev data!!!!
## Elevatr ----
#______________


#rename elev columns to distinguish the different elev data sources
colnames(out2)[grep("elev", colnames(out2))] <-
  paste("globfungi", colnames(out2)[grep("elev", colnames(out2))], 
        sep = "_")

# using the elevatr package needs to redefined the crs of global fungi sf.
# here elevation cells are coarse (z=5, ~4-5 km pixel), increased to z=12 require downloading 230652.5 Mb, not possible through API. Maybe to specify and see later if finer resolution is needed.

st_crs(globfSA_sf)
#4326
crs_dd <- 4326

#get elevation data
globfSA_elev <-
  elevatr::get_elev_point(locations = globfSA_sf,
                          prj = crs_dd,
                          src = "aws", 
                          overwrite = TRUE)


if(all(globfSA_elev$PermanentID == out2$PermanentID)) {
  out2$`elevatr_elevation_4.5km2` <- globfSA_elev$elevation
}

write.table(
  out2,
  file = "results/tmp_globfungi_nefineo_filtered_morrone_wwf_elevatr.tsv",
  sep = "\t",
  quote = FALSE,
  row.names = F,
  col.names = T
)


###fueron testadas algunas extraciones de otros datos, pero al final no necesariamente serian utiles

## Clim elev data ----  
#__________________________
# using the climenv package: access to three widely recognised modelled data sets, namely Global Climate Data (WorldClim 2), Climatologies at high resolution for the earth's land surface areas (CHELSA), and National Aeronautics and Space Administration's (NASA) Shuttle Radar Topography Mission (SRTM). Here are used WorldClim 2 and SRTM
# to do with more stable internet connexion

#subset to non N/A subregions otherwise the ce_download fails to downlaod tavg data.
globfSA_sub_sf <- globfSA_sf[which(globfSA_morrone$Subregion != "N/A"),]

### Get climenv data, to run only once as it takes time ----
#___________________________________________________________
if(!file.exists("resources/globfSA_climenv")){
  climenv::ce_download(
    "resources/globfSA_climenv",
    location = globfSA_sub_sf, 
  )
}

### Extract climenv data for each global fungi point ----
#________________________________________________________
#get all data, should be NA for most points in N/A regions.
# Output is a list containing the following data: 
# montly temperatures in degrees Celsius (tmax, tmean, tmin) or monthly precipitation in mm (prec). One data frame features elevation data (elev, mean of the cell). One data frame contains the central latitude of each feature. One, features the absolute minimum temperature for each month (abmt).

if(!file.exists("results/tmp_globfSA_climenv.rds")){
  globfSA_clim <- climenv::ce_extract(
    "resources/globfSA_climenv",
    location = globfSA_sub_sf,
    location_g = "PermanentID"
  )
  saveRDS(globfSA_clim, "results/tmp_globfSA_climenv.rds")
} else {
  globfSA_clim <- readRDS("results/tmp_globfSA_climenv.rds")
}

# rename colnames for big merging exclude sd columns as they are only 0s and NAs
tmp1 <-
  lapply((1:length(globfSA_clim))[!grepl("elev|lat|Readme|_sd", names(globfSA_clim))],
         function(x) {
           nm <- names(globfSA_clim)[x]
           out <- globfSA_clim[[x]]
           colnames(out) <-
             paste("climenv_monthavg", nm, colnames(out), sep = "_")
           as.data.frame(out)
         })
names(tmp1) <-
  names(globfSA_clim)[!grepl("elev|lat|Readme|_sd", names(globfSA_clim))]

tmp1 <- tmp1 %>%
  map( ~ .x %>%
         rownames_to_column("PermanentID")) %>%
  reduce(full_join, by = "PermanentID") 

# compute annual averages for each dataset for temperature and prec, exclude sd columns as they are only 0s and NAs
tmp2 <-
  lapply((1:length(globfSA_clim))[!grepl("Readme|_sd", names(globfSA_clim))], function(x) {
    nm <- names(globfSA_clim)[x]
    out <- as.data.frame(rowMeans(globfSA_clim[[x]][, , drop = FALSE]))
    colnames(out) <- nm
    out
  })
tmp2 <- tmp2 %>%
  map( ~ .x %>%
         rownames_to_column("PermanentID")) %>%
  reduce(full_join, by = "PermanentID")
colnames(tmp2)[-1] <- ifelse(grepl("elev|lat", colnames(tmp2)[-1]),
                             paste("climenv", colnames(tmp2)[-1], sep = "_"),
                             paste("climenv_annavg", colnames(tmp2)[-1], sep = "_"))


# merge with global fungi metadata 

out3 <- out2 %>%
  left_join(tmp2, by = join_by(PermanentID)) %>%
  left_join(tmp1, by = join_by(PermanentID)) 

## Checks ----
#_____________

### Missing values ----
#_______________________
# proportion of samples with NA values for each climenv data
tmp <- data.frame(apply(out3[grep("elev|climenv", colnames(out3))], 2, function(x) sum(is.na(x))) / nrow(out3)) 
colnames(tmp) <- "proportion of samples"
tmp <- 
  tmp %>%
  rownames_to_column("variable") 
tmp

# see what are NAs. 
table(out3$morrone_biogeoregions_Provincias[which(is.na(out3$climenv_annavg_tavg_m))])
#mostly Non neotropical, fuew others that are most likely close to the sea. More difficult to infer.

table(out3$morrone_biogeoregions_Provincias[which(is.na(out3$climenv_elev))])
#all non neotropical

### Basic stats -----
#____________________

tmp <- out3[grep("Region|elev|climenv_ann", colnames(out3))] %>%
  pivot_longer(!morrone_biogeoregions_Region, 
               names_to = "variable", 
               values_to = "values")

fig <- 
  ggplot(tmp, aes(values, fill=morrone_biogeoregions_Region)) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 40, color="grey35") + 
  facet_wrap(~variable, scales = "free") + 
  theme_bw() + 
  labs(fill="Biogeoregion")


ggsave(
  filename = "figures/07_statsclimenv.pdf",
  device = "pdf",
  width = 7,
  height = 6,
  dpi = 150,
  units = "in"
)

## Export climenv -----
#_______________________

write.table(
  out3,
  file = "results/tmp_globfungi_nefineo_filtered_morrone_wwf_elevatr_climenv.tsv",
  sep = "\t",
  quote = FALSE,
  row.names = F,
  col.names = T
)

# Get Holdridge classes ----
#___________________________

#extract Holdridge's (1967) life evapotranspiration ratios and total annual precipitations
# compute the climatic envelop, basis for Holdridge classes

holdridge_c <- data.frame(PermanentID = out3$PermanentID,
                          latitude = out3$latitude,
                          elev = out3$elevatr_elevation_4.5km2,
                          climenv::bioclimate(
                            out3[,grep("climenv_monthavg_tavg", colnames(out3))],
                            out3[,grep("climenv_monthavg_prec", colnames(out3))]))

# plot 
pdf(file="figures/08_holdrige_distrib.pdf")
Ternary::HoldridgePlot(hex.labels = Ternary::holdridgeLifeZonesUp)
Ternary::HoldridgeBelts()
Ternary::HoldridgePoints(holdridge_c[, "per"], holdridge_c[, "tap"], lwd = 1)
dev.off()


# Get Holdridge classes
evapo <- list(c(NA, NA, "Wet tundra", rep("Wet forest", 4), "Rain forest"),
              c(NA, "Moist tundra", rep("Moist forest", 4), "Wet forest"), 
              c("Dry tundra", "Dry bush", "Steppe", rep("Dry forest",2), "Moist forest"),
              c("Desert", "Desert scrub", "Thorn steppe", "Very dry forest", "Dry forest"),
              c("Desert", "Desert scrub", "Thorn woodland", "Very dry forest"),
              c("Desert", "Desert scrub", "Thorn woodland"),
              c("Desert", "Desert scrub"),
              "Desert")
names(evapo) <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16)

prec <- list(c(rep("Desert", 5), "Dry tundra", NA, NA), 
             c(rep("Desert scrub", 4), "Dry bush", "Moist tundra"),
             c(rep("Thorn woodland", 2), "Thorn steppe", "Steppe", "Moist forest", 
               "Wet tundra"),
             c(rep("Very dry forest", 2), "Dry forest", "Moist forest", "Wet forest"),
             c(rep("Dry forest", 2), "Moist forest", "Wet forest"),
             c(rep("Moist forest", 2), "Wet forest"), 
             rep("Wet forest", 2),
             "Rain forest")

names(prec) <- c(62.5, 125, 250, 500, 1000, 2000, 4000, 8000)

#quick and dirty hack, so far could not find a more robust way to infer Holdridge classes

idx <- findInterval(holdridge_c[, "per"], as.numeric(names(evapo)))
idx <- ifelse(idx == 0, 1, idx)
evapo_c <- evapo[idx]
idx <- findInterval(holdridge_c[, "tap"], as.numeric(names(prec)))
idx <- ifelse(idx == 0, 1, idx)
perc_c <- prec[idx]


evap_perc_c <- lapply(1:length(evapo_c), function(x) {
  paste(intersect(evapo_c[[x]], perc_c[[x]]), collapse = "|")
})

holdridge_c$holdridge_class <- unlist(evap_perc_c)

colnames(holdridge_c)[grep("abt|tap|^per$|holdridge", colnames(holdridge_c))] <- 
  paste("climenv", colnames(holdridge_c)[grep("abt|tap|^per$|holdridge", 
                                              colnames(holdridge_c))], sep="_")

saveRDS(holdridge_c, 
        "results/tmp_globfSA_climenv_holdridge.rds")

all(holdridge_c$PermanentID == out3$PermanentID)

out4 <- out3 %>%
  bind_cols(holdridge_c[,-c(1:3)])

## Export holdridge & Final -----
#________________________________

idx <- match(globfits1$PermanentID, out4$PermanentID)
write.table(
  out4[idx,],
  file = "results/globfungi_metadata_filtered_complemented_its1.tsv",
  sep = "\t",
  quote = FALSE,
  row.names = F,
  col.names = T
)

idx <- match(globfits2$PermanentID, out4$PermanentID)
write.table(
  out4[idx,],
  file = "results/globfungi_metadata_filtered_complemented_its2.tsv",
  sep = "\t",
  quote = FALSE,
  row.names = F,
  col.names = T
)

# NDVI ----
#__________
# NDVI data to be retrieved on Google Earth Engine