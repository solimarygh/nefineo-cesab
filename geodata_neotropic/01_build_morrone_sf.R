# 01_build_morrone_sf.R
# Build + fix + simplify Morrone shapefile once, then save as RDS for fast reuse.


#important!

suppressPackageStartupMessages({
  library(sf)
  library(rmapshaper)  # ms_simplify
  library(ggplot2)
  library (readr)
})

#### Important:::
#_____________________________________________
# Get Neotropical biogeographic provinces  ----
#_____________________________________________

# From Morrone et al. 2022 neotropical regions 
# https://drive.google.com/drive/folders/189lpqHTQUtr33LlPQauki7ciJrmJcfVR
# the provided shapefile appears to be incomplete (e.g., it does not contain all province IDs)

## Format neotropical map ----
#_____________________________


# Read original shapefile
nc <- read_sf("geodata_neotropic/NeotropicMap_Geo.shp")
nc <- st_as_sf(nc)

# Create reduced and corrected file, if it does not exist yet
if (!file.exists("geodata_neotropic/morrone_shapefile_improved_sf.rds")) {
  
  # Add province ID (based on Morrone 2022) 
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
  
  # Complete dominions
  nc$Dominio[nc$Subregion == "South American Transition Zone"] <- "South American Transition Zone"
  nc$Dominio[nc$Subregion == "Antillean"] <- "Antillean"
  
  # Complete subdominions (finer classification)
  nc$SubDominio <- NA
  nc$SubDominio[nc$IDProv <= 5] <- "Mexican transition zone"
  nc$SubDominio[nc$IDProv > 5 & nc$IDProv < 13] <- "Antillean subregion"
  nc$SubDominio[nc$IDProv > 12 & nc$IDProv < 18] <- "Brazilian subregion Mesoamerican dominion"
  nc$SubDominio[nc$IDProv > 17 & nc$IDProv < 30] <- "Pacific subregion"
  nc$SubDominio[nc$IDProv > 29 & nc$IDProv < 36] <- "Boreal Brazilian dominion"
  nc$SubDominio[nc$IDProv > 35 & nc$IDProv < 40] <- "South Brazilian dominion"
  nc$SubDominio[nc$IDProv == 40] <- "Chacoan subregion Southeastern Amazonian dominion"
  nc$SubDominio[nc$IDProv > 40 & nc$IDProv < 45] <- "Chacoan dominion"
  nc$SubDominio[nc$IDProv > 44 & nc$IDProv < 51] <- "Parana dominion"
  nc$SubDominio[nc$IDProv > 50] <- "South American transition zone"
  nc$SubDominio[is.na(nc$IDProv)] <- "Nearctic and Andean regions"
  
  # Reduce resolution for quick visualization
  nc_small <- ms_simplify(nc, keep = 0.05)
  
  # Save reduced sf object
  saveRDS(nc_small, "geodata_neotropic/morrone_shapefile_improved_sf.rds")
  
} else {
  nc_small <- readRDS("geodata_neotropic/morrone_shapefile_improved_sf.rds")
}

# Read simplified Morrone shapefile

# ⚠️ Make sure this file exists
nc_small <- readRDS("geodata_neotropic/morrone_shapefile_improved_sf.rds")


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

